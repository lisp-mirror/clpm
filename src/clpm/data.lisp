;;;; Support for generating pathnames to files in CLPM's datastore on the
;;;; filesystem.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; COPYING for license information.

(uiop:define-package #:clpm/data
    (:use #:cl)
  (:export #:clpm-data-pathname))

(in-package #:clpm/data)

(defvar *clpm-data-root* nil
  "Pathname to the directory designating the root of CLPM's data directory.")

(defun compute-clpm-data-root ()
  "Compute ~*clpm-data-root*~ using the CLPM_DATA_DIR variable or the XDG data
home."
  (setf *clpm-data-root*
        (or (uiop:getenv-absolute-directory "CLPM_DATA_DIR")
            (uiop:xdg-data-home "common-lisp" "clpm/"))))

(defun clear-clpm-data-root ()
  "Clear the ~*clpm-data-root*~ variable."
  (setf *clpm-data-root* nil))

(uiop:register-clear-configuration-hook 'clear-clpm-data-root)
(uiop:register-image-restore-hook 'compute-clpm-data-root)

(defun clpm-data-pathname (x &key ensure-directory)
  "Given a list of directories, optionally ending with a file name and type,
~x~ relative to ~*clpm-data-root*~, return an absolute pathname. If
~ensure-directory~ is non-NIL, ensures the returned pathname is a directory."
  (check-type x list)
  (uiop:resolve-absolute-location
   (list*
    *clpm-data-root*
    x)
   :ensure-directory ensure-directory))
