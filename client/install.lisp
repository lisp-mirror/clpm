;;;; Interface to clpm install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)

(defun install (&key projects
                  systems
                  version ref source
                  (validate 'context-diff-approved-p)
                  no-deps
                  (context (default-context))
                  (update-asdf-config (asdf-integration-active-p)))
  "Install a set of projects and systems. If the install completed, returns
three values. The first is a source registry form, the second is a list of
systems installed in the context, and the third is a list of primary system
names visible when that source registry form is used.

PROJECTS and SYSTEMS must be lists of dependency specifiers. VERSION, REF, and
SOURCE must be strings and are used as the default constraints on PROJECTS and
SYSTEMS if such constraints cannot be extracted from the specifiers themselves.

VALIDATE must be a function of one argument (a diff) and returns non-NIL if the
install should proceed.

If UPDATE-ASDF-CONFIG is non-NIL and CONTEXT is the active context, then ASDF's
source registry is updated with the results."
  (when (and (context-bundle-p context)
             (or projects systems))
    (error "INSTALL currently only accepts NIL for PROJECTS and SYSTEMS when working on bundles."))
  (multiple-value-bind
        (source-registry installed-system-names visible-primary-system-names)
      (if (context-bundle-p context)
          (bundle-install :validate validate :clpmfile context)
          (let (diff-description)
            (with-clpm-proc (proc)
              (clpm-proc-print
               proc
               `(context-to-asdf-source-registry-form
                 (install :projects ',(ensure-list projects)
                          :systems ',(ensure-list systems)
                          :no-deps-p ,no-deps
                          :context ,context
                          :save-context-p t
                          :validate ,(make-diff-validator-fun)
                          :version ,version
                          :ref ,ref
                          :source ,source)))
              (setf diff-description (clpm-proc-read proc))
              (let ((validate-result (funcall validate (make-context-diff-from-description diff-description)))
                    source-registry)
                (clpm-proc-print proc validate-result)
                (setf source-registry (clpm-proc-read proc))
                (when validate-result
                  (clpm-proc-print proc `(mapcar 'system-name (context-installed-systems ,context)))
                  (clpm-proc-print proc `(context-visible-primary-system-names ,context))
                  (values source-registry (clpm-proc-read proc) (clpm-proc-read proc)))))))
    (when (and update-asdf-config (equal context (active-context)))
      (asdf-configure-source-registry source-registry)
      (unless (context-bundle-p context)
        (setf *active-context-installed-systems* installed-system-names
              *active-context-visible-primary-system-names* visible-primary-system-names)))
    (values source-registry installed-system-names visible-primary-system-names)))
