;;;; Support for generating pathnames to files in CLPM's cache on the
;;;; filesystem.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; COPYING for license information.

(uiop:define-package #:clpm/cache
    (:use #:cl)
  (:export #:clpm-cache))

(in-package #:clpm/cache)

(defvar *clpm-cache-root* nil
  "Pathname to the directory designating the root of CLPM's cache directory.")

(defun compute-clpm-cache-root ()
  "Compute ~*clpm-cache-root*~ using the CLPM_CACHE variable or the XDG cache
home."
  (setf *clpm-cache-root*
        (or (uiop:getenv-absolute-directory "CLPM_CACHE")
            (uiop:xdg-cache-home "clpm/"))))

(defun clear-clpm-cache-root ()
  "Clear the ~*clpm-cache-root*~ variable."
  (setf *clpm-cache-root* nil))

(uiop:register-clear-configuration-hook 'clear-clpm-cache-root)
(uiop:register-image-restore-hook 'compute-clpm-cache-root)

(defun clpm-cache (x &key ensure-directory)
  "Given a list of directories, optionally ending with a file name and type,
~x~ relative to ~*clpm-cache-root*~, return an absolute pathname. If
~ensure-directory~ is non-NIL, ensures the returned pathname is a directory."
  (check-type x list)
  (uiop:resolve-absolute-location
   (list*
    *clpm-cache-root*
    x)
   :ensure-directory ensure-directory))
