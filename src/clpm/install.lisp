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
           #:install-release))

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
                            (validate (constantly t)))
  (let* ((orig-context (get-context context))
         (context (copy-context orig-context))
         (requirement (make-requirement type name
                                        :version-spec version-spec :source source
                                        :no-deps-p no-deps-p :commit commit
                                        :branch branch :tag tag))
         (update-projects nil)
         (add-result (context-add-requirement! context requirement)))
    (when (and (or commit branch tag) add-result)
      (push name update-projects))
    (let* ((new-context (resolve-requirements context :update-projects update-projects))
           (diff (context-diff orig-context new-context)))
      (when (funcall validate diff)
        (mapc #'install-release (context-releases new-context))
        (context-write-asdf-files new-context)
        (save-global-context new-context))
      new-context)))
