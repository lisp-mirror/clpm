;;;; Git Repositories hosted on Github servers
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/repos/github
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/archives
          #:clpm/cache
          #:clpm/config
          #:clpm/data
          #:clpm/http-client
          #:clpm/repos/defs
          #:clpm/repos/git
          #:split-sequence)
  (:import-from #:puri)
  (:export #:github-repo
           #:github-repo-host
           #:github-repo-path))

(in-package #:clpm/repos/github)

(defclass github-repo (git-repo)
  ((host
    :initarg :host
    :initform "github.com"
    :reader github-repo-host)
   (path
    :initarg :path
    :initform (error "Path must be provided")
    :reader github-repo-path)))

(defun github-repo-config (repo &rest path)
  (apply #'config-value :git :remotes (github-repo-host repo) path))

(defmethod git-repo-credentials ((repo github-repo))
  (let ((username (github-repo-config repo :username))
        (password (github-repo-config repo :password))
        (out nil))
    (when username
      (push (cons :username username) out))
    (when password
      (push (cons :password password) out))
    out))

(defmethod git-repo-uri-string ((repo github-repo))
  (ecase (or (github-repo-config repo :method)
             :https)
    (:https
     (uiop:strcat "https://" (github-repo-host repo) "/" (github-repo-path repo) ".git"))
    (:ssh
     (uiop:strcat "git@" (github-repo-host repo) ":" (github-repo-path repo) ".git"))))

(defun split-path (path)
  (aprog1 (split-sequence #\/ path)
    (dolist (segment it)
      (assert (not (equal "" segment)))
      (assert (not (starts-with #\. segment))))))

(defmethod git-repo-local-dir ((repo github-repo))
  "The cache directory for this repo is based on the hostname."
  (let* ((path (github-repo-path repo))
         (path-with.git (if (not (ends-with-subseq ".git" path))
                            (uiop:strcat path ".git")
                            path))
         (split-path (split-path path-with.git)))
    (clpm-cache-pathname
     `("vcs"
       "github"
       ,(github-repo-host repo)
       ,@split-path)
     :ensure-directory t)))

(defmethod repo-lib-base-pathname ((repo github-repo))
  (let* ((path (github-repo-path repo))
         (path-with.git (if (not (ends-with-subseq ".git" path))
                            (uiop:strcat path ".git")
                            path))
         (split-path (split-path path-with.git)))
    (clpm-data-pathname
     `("vcs"
       "github"
       ,(github-repo-host repo)
       ,@split-path)
     :ensure-directory t)))

(defmethod repo-to-form ((repo github-repo))
  `(:github
    ,@(when (not (equal (github-repo-host repo) "github.com"))
        (list :host (github-repo-host repo)))
    :path ,(github-repo-path repo)))
