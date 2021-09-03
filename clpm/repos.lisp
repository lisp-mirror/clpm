;;;; VCS Repositories
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/repos
    (:use #:cl
          #:clpm/repos/defs
          #:clpm/repos/git
          #:clpm/repos/github
          #:clpm/repos/gitlab
          #:clpm/repos/local-git-override
          #:clpm/repos/plain-git)
  (:reexport #:clpm/repos/defs)
  (:reexport #:clpm/repos/git)
  (:reexport #:clpm/repos/github)
  (:reexport #:clpm/repos/gitlab)
  (:reexport #:clpm/repos/local-git-override)
  (:export #:make-repo-from-description))

(in-package #:clpm/repos)

(defun make-repo-from-description (desc)
  (destructuring-bind (repo-type &rest args) desc
    (ecase repo-type
      (:git
       (apply #'make-instance 'plain-git-repo args))
      (:github
       (apply #'make-instance 'github-repo args))
      (:gitlab
       (apply #'make-instance 'gitlab-repo args)))))
