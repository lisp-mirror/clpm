;;;; CLPM version definitions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/version
    (:use #:cl)
  (:export #:clpm-version))

(in-package #:clpm/version)


;;; Application Version

;; NOTE: If this form changes position in this file you *must* update clpm.asd as
;; well.
(defparameter *version* "0.0.8"
  "The version number of CLPM.")

(defparameter *git-version* nil
  "Cache the git version information as of when the executable was built. Is NIL
until an image is dumped and is set by ~cache-git-version~.")

(defun git-exists-p ()
  "Returns T iff git exists and is callable."
  (ignore-errors
   (uiop:run-program '("git" "--version"))
   t))

(defun get-git-version ()
  "Query the git version of CLPM. Returns an empty string if it cannot be
determined (.git directory is missing, git executable is missing, etc.)."
  (if (and (uiop:directory-exists-p (asdf:system-relative-pathname :clpm ".git/"))
           (git-exists-p))
      (uiop:with-current-directory ((asdf:system-relative-pathname :clpm ""))
        (let ((commit (uiop:run-program
                       '("git" "rev-parse" "--short" "HEAD")
                       :output '(:string :stripped t)))
              (dirty-p (not (zerop (nth-value 2
                                              (uiop:run-program '("git" "diff-index" "--quiet" "HEAD")
                                                                :ignore-error-status t))))))
          (format nil "~A~:[~;-dirty~]" commit dirty-p)))
      ""))

(defun cache-git-version ()
  "Cache the git version into ~*git-version*~."
  (setf *git-version* (get-git-version)))
(uiop:register-image-dump-hook 'cache-git-version)

(defun git-version ()
  "Get the git version from ~*git-version*~ or ~get-git-version~."
  (or *git-version*
      (get-git-version)))

(defun clpm-version ()
  "Return the CLPM version string."
  (let ((git-version (git-version)))
    (if (string= "" git-version)
        *version*
        (concatenate 'string *version* "+" git-version))))
