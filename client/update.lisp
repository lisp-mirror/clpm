;;;; Interface to clpm update
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)

(defun update (&key projects
                 systems
                 (validate 'context-diff-approved-p)
                 (context (default-context))
                 (update-asdf-config (asdf-integration-active-p)))
  "Update a set of projects and systems. Returns four values. The first is a
source registry form, the second is a list of systems installed in the context,
the third is a list of primary system names visible when that source registry
form is used, and the fourth is a list of primary system names that are editable
in the context.

PROJECTS and SYSTEMS must be lists of strings. If none are specified, every
project in the CONTEXT is eligible for upgrading.

VALIDATE must be a function of one argument (a diff) and returns non-NIL if the
install should proceed.

If UPDATE-ASDF-CONFIG is non-NIL and CONTEXT is the active context, then ASDF's
source registry is updated with the results."
  (with-clpm-proc (proc)
    (clpm-proc-print
     proc
     `(with-clpm-session ()
        ;; Use a symbol that we know is going to be present in the CLPM package.
        ,(if (context-bundle-p context)
             `(bundle-update :projects ',(ensure-list projects)
                             :systems ',(ensure-list systems)
                             :clpmfile ,context
                             :validate ,(make-diff-validator-fun))
             `(update :update-projects ',(ensure-list projects)
                      :update-systems ',(ensure-list systems)
                      :context ,context
                      :validate ,(make-diff-validator-fun)))
        (list (source-registry :context ,context)
              (installed-system-names :context ,context)
              (visible-primary-system-names :context ,context)
              (editable-primary-system-names :context ,context))))
    (clpm-proc-print proc
                     (funcall validate (make-context-diff-from-description (clpm-proc-read proc))))
    (destructuring-bind (source-registry installed-system-names visible-system-names editable-system-names)
        (clpm-proc-read proc)
      (when (and update-asdf-config (equal context (active-context)))
        (asdf-configure-source-registry source-registry)
        (setf *active-context-installed-systems* installed-system-names
              *active-context-visible-primary-system-names* visible-system-names
              *active-context-editable-primary-system-names* editable-system-names))
      (values source-registry installed-system-names visible-system-names editable-system-names))))
