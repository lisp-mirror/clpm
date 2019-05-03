;;;; CLPM version definitions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/version
    (:use #:cl
          #:clpm/version-strings)
  (:export #:clpm-version))

(in-package #:clpm/version)


;;; Application Version

;; NOTE: If this form changes position in this file you *must* update clpm.asd as
;; well.
(defparameter *version-for-asdf* "0.1.0"
  "ASDF is overly picky about its version numbers. This should be the primary
  version number (no prerelease info) as ~*base-version*~.")

(defparameter *base-version* "0.1.0-alpha.1"
  "The base version number of CLPM.")

(defparameter *full-version* nil
  "Cache the full version information as of when the executable was built. Is
NIL until an image is dumped and is set by ~cache-full-version~.")

(defun git-exists-p ()
  "Returns T iff git exists and is callable."
  (ignore-errors
   (uiop:run-program '("git" "--version"))
   (clpm-git '("describe"))
   t))

(defun clpm-git (args &rest keys)
  (uiop:with-current-directory ((asdf:system-relative-pathname :clpm ""))
    (apply #'uiop:run-program
           `("git" ,@args)
           keys)))

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
  (parse-version-string
   (clpm-git `("describe" "--tags" "--match" "v*" ,@(when ref (list ref)))
             :output '(:string :stripped t))))

(defun get-git-common-ancestor ()
  (clpm-git '("merge-base" "HEAD" "master")
            :output '(:string :stripped t)))

(defun get-git-distance-from-master ()
  (parse-integer (clpm-git '("rev-list" "--count" "HEAD" "--not" "master")
                           :output '(:string :stripped t))))

(defun git-tag-p (describe)
  (or (= 1 (length describe))
      (= 3 (length describe))))

(defun get-full-version ()
  (if (git-exists-p)
      (let ((git-branch (get-git-branch))
            (git-describe (get-git-describe))
            (git-dirty-p (get-git-dirty-p)))
        (if (git-tag-p git-describe)
            ;; This is a tagged version, just return the base version number.
            *base-version*
            ;; Here's the fun part, assemble the entire version string.
            (destructuring-bind (primary-version prerelease-category build-meta)
                (parse-semantic-version *base-version*)
              (declare (ignore build-meta))
              (unless prerelease-category
                (setf prerelease-category '(0)))
              (if (equal git-branch "master")
                  (format nil "~{~A~^.~}-~{~A~^.~}.~A+~A~:[~;-dirty~]"
                          primary-version
                          prerelease-category
                          (first (third git-describe))
                          (first (fifth git-describe))
                          git-dirty-p)
                  (let* ((ancestor (get-git-common-ancestor))
                         (ancestor-describe (get-git-describe ancestor))
                         (distance-from-master (get-git-distance-from-master)))
                    (format nil "~A-0.~A.~A.~A+~A~:[~;-dirty~]"
                            *base-version*
                            (first (third ancestor-describe))
                            git-branch
                            distance-from-master
                            (first (fifth git-describe))
                            git-dirty-p))))))
      ;; We don't have git... Best we can currently do is return the base
      ;; version number.
      *base-version*))

(defun cache-full-version ()
  "Cache the full version into ~*full-version*~."
  (setf *full-version* (get-full-version)))
(uiop:register-image-dump-hook 'cache-full-version)

(defun clpm-version ()
  "Return the CLPM version string."
  (or
   *full-version*
   (get-full-version)))
