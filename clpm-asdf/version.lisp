;;;; CLPM version definitions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-asdf)

;;; Application Version

(defun base-version ()
  (uiop:read-file-form (asdf:system-relative-pathname :clpm-asdf "version.lisp-expr")))

(defparameter *full-version* nil
  "Cache the full version information as of when the executable was built. Is
NIL until an image is dumped and is set by ~cache-full-version~.")

(defun clpm-git (args &rest keys)
  (uiop:with-current-directory ((asdf:system-relative-pathname :clpm ""))
    (apply #'uiop:run-program
           `("git" ,@args)
           keys)))

(defun git-exists-p ()
  "Returns T iff git exists and is callable."
  (ignore-errors
   (uiop:run-program '("git" "--version"))
   (clpm-git '("describe"))
   t))

(defun get-git-branch ()
  (or
   (ignore-errors
    (clpm-git '("symbolic-ref" "--short" "HEAD")
              :output '(:string :stripped t)))
   "HEAD"))

(defun get-git-commit ()
  (clpm-git '("rev-parse" "--short" "HEAD")
            :output '(:string :stripped t)))

(defun get-git-dirty-p ()
  (not (zerop (nth-value 2
                         (clpm-git '("diff-index" "--quiet" "HEAD")
                                   :ignore-error-status t)))))

(defun get-git-describe (&optional ref)
  (uiop:split-string
   (clpm-git `("describe" "--tags" "--match" "v*" ,@(when ref (list ref)))
             :output '(:string :stripped t))
   :max 3
   :separator '(#\-)))

(defun get-git-common-ancestor ()
  (clpm-git '("merge-base" "HEAD" "origin/master")
            :output '(:string :stripped t)))

(defun get-git-distance-from-master ()
  (parse-integer (clpm-git '("rev-list" "--count" "HEAD" "--not" "origin/master")
                           :output '(:string :stripped t))))

(defun get-git-tag ()
  (multiple-value-bind (stdout stderr code)
      (clpm-git '("describe" "--tags" "--exact-match" "--match" "v*" "HEAD")
                :ignore-error-status t
                :output '(:string :stripped t))
    (declare (ignore stderr))
    (values (zerop code)
            stdout)))

(defun get-full-version ()
  (if (git-exists-p)
      (let ((git-branch (get-git-branch))
            (git-describe (get-git-describe))
            (git-dirty-p (get-git-dirty-p)))
        (multiple-value-bind (git-tag-p git-tag) (get-git-tag)
          (if git-tag-p
              ;; This is a tagged version, just return the tag without a leading
              ;; v.
              (subseq git-tag 1)
              ;; Here's the fun part, assemble the entire version string.
              (let* ((version (cl-semver:read-version-from-string (base-version)))
                     (primary-version (list (cl-semver:version-major version)
                                            (cl-semver:version-minor version)
                                            (cl-semver:version-patch version)))
                     (prerelease-category (cl-semver:version-pre-release-identifiers version)))
                (unless prerelease-category
                  (setf prerelease-category '(0)))
                (if (equal git-branch "master")
                    (format nil "~{~A~^.~}-~{~A~^.~}.~A+~A~:[~;-dirty~]"
                            primary-version
                            prerelease-category
                            (second git-describe)
                            (third git-describe)
                            git-dirty-p)
                    (let* ((ancestor (get-git-common-ancestor))
                           (ancestor-describe (get-git-describe ancestor))
                           (distance-from-master (get-git-distance-from-master)))
                      (format nil "~{~A~^.~}-~{~A~^.~}.~A.0.~A.~A+~A~:[~;-dirty~]"
                              primary-version prerelease-category
                              (second ancestor-describe)
                              git-branch
                              distance-from-master
                              (third git-describe)
                              git-dirty-p)))))))
      ;; We don't have git... Best we can currently do is return the base
      ;; version number.
      (base-version)))

(defun cache-full-version ()
  "Cache the full version into ~*full-version*~."
  (setf *full-version* (get-full-version)))
(uiop:register-image-dump-hook 'cache-full-version)

(defun clpm-version ()
  "Return the CLPM version string."
  (or
   *full-version*
   (get-full-version)))
