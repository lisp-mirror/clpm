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
                  (context (context)))
  "Install a set of projects and systems. Returns non-NIL if the install proceeded.

PROJECTS and SYSTEMS must be lists of dependency specifiers. VERSION, REF, and
SOURCE must be strings and are used as the default constraints on PROJECTS and
SYSTEMS if such constraints cannot be extracted from the specifiers themselves.

VALIDATE must be a function of one argument (a diff) and returns non-NIL if the
install should proceed."
  (when (and (context-bundle-p context)
             (or projects systems))
    (error "INSTALL currently only accepts NIL for PROJECTS and SYSTEMS when working on bundles."))
  (if (stringp context)
      (let (diff-description)
        (with-clpm-proc (proc)
          (clpm-proc-print
           proc
           `(context-asd-pathnames
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
          (let ((validate-result (funcall validate (make-context-diff-from-description diff-description))))
            (clpm-proc-print proc validate-result)
            (clpm-proc-read proc)
            validate-result)))
      (bundle-install :validate validate :clpmfile (when (pathnamep context) context))))
