;;;; Basic installation support
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/install
    (:use #:cl
          #:clpm/context
          #:clpm/install/defs
          #:clpm/log
          #:clpm/resolve-2
          #:clpm/requirement
          #:clpm/source
          #:clpm/version-strings)
  (:export #:install
           #:install-release))

(in-package #:clpm/install)

(setup-logger)

(defun install (type name &key version-spec source context no-deps-p
                            (validate (constantly t)))
  (let* ((requirement-type (ecase type
                             (:project 'project-requirement)
                             (:system 'system-requirement)))
         (orig-context (get-context context))
         (context (copy-context orig-context))
         (source (get-source source))
         (version-spec (parse-version-specifier version-spec))
         (requirement (make-instance requirement-type
                                     :name name
                                     :version-spec version-spec
                                     :no-deps-p no-deps-p
                                     :source source
                                     :why t)))
    (context-add-requirement! context requirement)
    (let* ((new-context (resolve-requirements context))
           (diff (context-diff orig-context new-context)))
      (when (funcall validate diff)
        (mapc #'install-release (context-releases new-context))
        (context-write-asdf-files new-context)
        (save-global-context new-context)))))
