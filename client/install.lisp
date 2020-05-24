;;;; Interface to clpm install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)

(defun install (&key projects
                  systems
                  asds
                  version ref source
                  (validate 'context-diff-approved-p)
                  no-deps
                  (context (default-context))
                  (update-asdf-config (asdf-integration-active-p))
                  no-resolve)
  "Install a set of projects and systems. Returns the source registry form for
the context.

PROJECTS and SYSTEMS must be lists of dependency specifiers. VERSION, REF, and
SOURCE must be strings and are used as the default constraints on PROJECTS and
SYSTEMS if such constraints cannot be extracted from the specifiers themselves.

VALIDATE must be a function of one argument (a diff) and returns non-NIL if the
install should proceed.

NO-RESOLVE only has an effect if the CONTEXT is a bundle. If it is non-NIL, then
the bundle will be installed completely from its lock file, without reresolving
any requirements.

If UPDATE-ASDF-CONFIG is non-NIL and CONTEXT is the active context, then ASDF's
source registry is updated with the results."
  (when (and (context-bundle-p context)
             (or projects systems))
    (error "INSTALL currently only accepts NIL for PROJECTS and SYSTEMS when working on bundles."))
  (with-clpm-proc (proc)
    (clpm-proc-print
     proc
     `(with-clpm-session ()
        ;; Use a symbol that we know is going to be present in the CLPM package.
        ,(if (context-bundle-p context)
             `(bundle-install :clpmfile ,context :validate ,(make-diff-validator-fun)
                              :no-resolve ,no-resolve)
             `(install :projects ',(ensure-list projects)
                       :systems ',(ensure-list systems)
                       :asds ',(mapcar #'merge-pathnames (ensure-list asds))
                       :no-deps-p ,no-deps
                       :context ,context
                       :save-context-p t
                       :validate ,(make-diff-validator-fun)
                       :version ,version
                       :ref ,ref
                       :source ,source))
        (list (source-registry :context ,context :ignore-inherited-source-registry ,(context-bundle-p context))
              (installed-primary-system-names :context ,context)
              (visible-primary-system-names :context ,context)
              (editable-primary-system-names :context ,context))))
    (clpm-proc-print proc
                     (funcall validate (make-context-diff-from-description (clpm-proc-read proc))))
    (destructuring-bind (source-registry installed-primary-system-names visible-system-names editable-system-names)
        (clpm-proc-read proc)
      (when (and update-asdf-config (equal context (active-context)))
        (asdf-configure-source-registry source-registry)
        (setf *active-context-installed-primary-system-names* installed-primary-system-names
              *active-context-visible-primary-system-names* visible-system-names
              *active-context-editable-primary-system-names* editable-system-names))
      source-registry)))
