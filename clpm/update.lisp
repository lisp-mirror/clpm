;;;; Basic update support
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/update
    (:use #:cl
          #:alexandria
          #:clpm/context
          #:clpm/context-diff
          #:clpm/install/defs
          #:clpm/log
          #:clpm/resolve
          #:clpm/session
          #:clpm/source)
  (:export #:update))

(in-package #:clpm/update)

(setup-logger)

(defun update (&key update-projects update-systems
                 (validate (constantly t))
                 context)
  (with-clpm-session ()
    (with-context (orig-context context)
      (with-context (context (copy-context orig-context))
        ;; Map all systems to their corresponding projects.
        (dolist (system update-systems)
          (when-let* ((release-cons (find-if (lambda (x)
                                               (member system x :test #'equal))
                                             (context-system-releases orig-context)
                                             :key #'cdr))
                      (release (car release-cons))
                      (project-name (project-name (release-project release))))
            (pushnew project-name update-projects :test #'equal)))

        (log:info "Updating ~:[all~;~:*~{~A~^, ~}~] projects." update-projects)
        (let* ((new-context (resolve-requirements context :update-projects (or update-projects t)))
               (diff (make-context-diff orig-context new-context)))
          (when (funcall validate diff)
            (mapc #'install-release (context-releases new-context))
            (context-write-asdf-files new-context)
            (save-context new-context)))))))
