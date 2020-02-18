;;;; Basic installation support
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/install
    (:use #:cl
          #:anaphora
          #:clpm/context
          #:clpm/install/defs
          #:clpm/log
          #:clpm/resolve
          #:clpm/requirement
          #:clpm/source
          #:clpm/version-strings)
  (:export #:install
           #:install-release
           #:install-requirements))

(in-package #:clpm/install)

(setup-logger)

(defun make-requirement (type name
                         &key version-spec source no-deps-p commit branch tag)
  (let* ((source (get-source source))
         (version-spec (parse-version-specifier version-spec))
         (common-args (list :name name
                            :source source
                            :why t
                            :no-deps-p no-deps-p)))
    (cond
      ((and (eql :project type)
            (or commit branch tag))
       ;; Requesting that we install a project from vcs.
       (assert (not version-spec))
       (apply #'make-instance 'vcs-project-requirement
              :tag tag
              :commit commit
              :branch branch
              common-args))
      ((eql :project type)
       ;; Install a project.
       (apply #'make-instance 'project-requirement
              :version-spec version-spec
              common-args))
      ((eql type :system)
       ;; Install a system.
       (assert (not (or commit branch tag)))
       (apply #'make-instance 'system-requirement
              :version-spec version-spec
              common-args)))))

(defun install (type name &key version-spec source context no-deps-p
                            commit branch tag
                            (validate (constantly t))
                            save-context-p)
  (let ((requirement (make-requirement type name
                                       :version-spec version-spec :source source
                                       :no-deps-p no-deps-p :commit commit
                                       :branch branch :tag tag)))
    (install-requirements (list requirement) :context context :validate validate
                                             :save-context-p save-context-p)))

(defun install-requirements (reqs &key
                                    context
                                    (validate (constantly t))
                                    save-context-p
                                    update-projects)
  (let* ((orig-context (get-context context))
         (new-context (copy-context orig-context)))
    (dolist (r reqs)
      (context-add-requirement! new-context r))
    (setf new-context (resolve-requirements new-context :update-projects update-projects))
    (when (funcall validate (context-diff orig-context new-context))
      (mapc #'install-release (context-releases new-context))
      (when save-context-p
        (context-write-asdf-files new-context)
        (save-global-context new-context)))
    new-context))
