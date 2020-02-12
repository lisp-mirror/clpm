;;;; Basic update support
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/update
    (:use #:cl
          #:clpm/context
          #:clpm/install/defs
          #:clpm/log
          #:clpm/resolve)
  (:export #:update))

(in-package #:clpm/update)

(setup-logger)

(defun update (&key update-projects
                 (validate (constantly t))
                 context)
  (let* ((orig-context (get-context context))
         (context (copy-context orig-context)))
    (log:info "Updating ~:[all~;~{~A~^, ~}~] projects." update-projects update-projects)
    (let* ((new-context (resolve-requirements context :update-projects (or update-projects t)))
           (diff (context-diff orig-context new-context)))
      (when (funcall validate diff)
        (mapc #'install-release (context-releases new-context))
        (context-write-asdf-files new-context)
        (save-global-context new-context)))))
