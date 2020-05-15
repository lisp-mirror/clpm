;;;; Git Repositories
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/repos/local-git-override
    (:use #:cl
          #:clpm/log
          #:clpm/repos/git)
  (:export #:local-git-override-repo))

(in-package #:clpm/repos/local-git-override)

(setup-logger)

(defclass local-git-override-repo (git-repo)
  ((directory-pathname
    :initarg :pathname
    :reader git-repo-local-dir)))

(defmethod initialize-instance :after ((repo local-git-override-repo) &rest initargs &key pathname)
  (declare (ignore initargs))
  (assert (uiop:absolute-pathname-p pathname))
  (assert (uiop:directory-pathname-p pathname))
  (assert (uiop:directory-exists-p pathname)))
