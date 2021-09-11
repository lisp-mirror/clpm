;;;; Git repositories not hosted on a forge
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/repos/plain-git
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/cache
          #:clpm/data
          #:clpm/repos/defs
          #:clpm/repos/git
          #:split-sequence)
  (:import-from #:puri)
  (:export #:plain-git-repo))

(in-package #:clpm/repos/plain-git)

(defclass plain-git-repo (git-repo)
  ((repository
    :initarg :url
    :reader git-repo-uri-string)))

(defmethod git-repo-credentials ((repo plain-git-repo))
  nil)

(defun split-path (path)
  (when (eql (aref path 0) #\/)
    (setf path (subseq path 1)))
  (aprog1 (split-sequence #\/ path)
    (dolist (segment it)
      (assert (not (equal "" segment)))
      (assert (not (starts-with #\. segment))))))

(defmethod git-repo-local-dir ((repo plain-git-repo))
  "The cache directory for this repo is based on the hostname."
  (let* ((uri (parse-git-uri (git-repo-uri-string repo)))
         (host (git-uri/real-host uri))
         (path (puri:uri-path uri))
         (path-with.git (if (not (ends-with-subseq ".git" path))
                            (uiop:strcat path ".git")
                            path))
         (split-path (split-path path-with.git)))
    (clpm-cache-pathname
     `("vcs"
       ,(if (eql (puri:uri-scheme uri) :file)
            "gitfs"
            "git")
       ,@(when (not (null host))
           (list host))
       ,@split-path)
     :ensure-directory t)))

(defmethod repo-lib-base-pathname ((repo plain-git-repo))
  (let* ((uri (parse-git-uri (git-repo-uri-string repo)))
         (host (git-uri/real-host uri))
         (path (puri:uri-path uri))
         (path-with.git (if (not (ends-with-subseq ".git" path))
                            (uiop:strcat path ".git")
                            path))
         (split-path (split-path path-with.git)))
    (clpm-data-pathname
     `("vcs"
       ,(if (eql (puri:uri-scheme uri) :file)
            "gitfs"
            "git")
       ,@(when (not (null host))
           (list host))
       ,@split-path)
     :ensure-directory t)))

(defmethod repo-to-form ((repo plain-git-repo))
  `(:git
    :url ,(git-repo-uri-string repo)))
