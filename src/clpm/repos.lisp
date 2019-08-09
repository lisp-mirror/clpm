;;;; VCS Repositories
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/repos
    (:use #:cl
          #:clpm/repos/defs
          #:clpm/repos/git
          #:clpm/repos/gitlab)
  (:reexport #:clpm/repos/defs)
  (:reexport #:clpm/repos/git)
  (:reexport #:clpm/repos/gitlab)
  (:export #:make-repo-from-description))

(in-package #:clpm/repos)

(defun make-repo-from-description (desc)
  (destructuring-bind (repo-type &rest args) desc
    (ecase repo-type
      (:gitlab
       (apply #'make-instance 'gitlab-repo args)))))
