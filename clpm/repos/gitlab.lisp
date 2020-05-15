;;;; Git Repositories hosted on Gitlab servers
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/repos/gitlab
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
  (:export #:gitlab-repo
           #:gitlab-repo-host
           #:gitlab-repo-path))

(in-package #:clpm/repos/gitlab)

(defclass gitlab-repo (git-repo)
  ((host
    :initarg :host
    :initform "gitlab.com"
    :reader gitlab-repo-host)
   (path
    :initarg :path
    :initform (error "Path must be provided")
    :reader gitlab-repo-path)))

(defun gitlab-repo-config (repo &rest path)
  (apply #'config-value :git :remotes (gitlab-repo-host repo) path))

(defmethod git-repo-credentials ((repo gitlab-repo))
  (let ((username (gitlab-repo-config repo :username))
        (password (gitlab-repo-config repo :password))
        (out nil))
    (when username
      (push (cons :username username) out))
    (when password
      (push (cons :password password) out))
    out))

(defmethod git-repo-uri-string ((repo gitlab-repo))
  (ecase (or (gitlab-repo-config repo :method)
             :https)
    (:https
     (uiop:strcat "https://" (gitlab-repo-host repo) "/" (gitlab-repo-path repo) ".git"))
    (:ssh
     (uiop:strcat "git@" (gitlab-repo-host repo) ":" (gitlab-repo-path repo) ".git"))))

(defun split-path (path)
  (aprog1 (split-sequence #\/ path)
    (dolist (segment it)
      (assert (not (equal "" segment)))
      (assert (not (starts-with #\. segment))))))

(defun url-encode-path (path)
  (let ((segments (split-path path)))
    (format nil "~{~A~^%2F~}" segments)))

(defun archive-uri (repo ref)
  (let ((api-uri
          (make-instance 'puri:uri
                         :scheme :https
                         :host (gitlab-repo-host repo)
                         :path (concatenate 'string
                                            "/api/v4/projects/"
                                            (url-encode-path (gitlab-repo-path repo))
                                            "/repository/archive.tar.gz")
                         :query (concatenate 'string
                                             "sha=" ref))))
    api-uri))

(defmethod git-repo-local-dir ((repo gitlab-repo))
  "The cache directory for this repo is based on the hostname."
  (let* ((path (gitlab-repo-path repo))
         (path-with.git (if (not (ends-with-subseq ".git" path))
                            (uiop:strcat path ".git")
                            path))
         (split-path (split-path path-with.git)))
    (clpm-cache-pathname
     `("vcs"
       "gitlab"
       ,(gitlab-repo-host repo)
       ,@split-path)
     :ensure-directory t)))

(defmethod repo-lib-base-pathname ((repo gitlab-repo))
  (let* ((path (gitlab-repo-path repo))
         (path-with.git (if (not (ends-with-subseq ".git" path))
                            (uiop:strcat path ".git")
                            path))
         (split-path (split-path path-with.git)))
    (clpm-data-pathname
     `("vcs"
       "gitlab"
       ,(gitlab-repo-host repo)
       ,@split-path)
     :ensure-directory t)))

(defmethod repo-to-form ((repo gitlab-repo))
  `(:gitlab
    ,@(when (not (equal (gitlab-repo-host repo) "gitlab.com"))
        (list :host (gitlab-repo-host repo)))
    :path ,(gitlab-repo-path repo)))
