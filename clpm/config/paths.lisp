;;;; Definitions for pathnames to CLPM configuration.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/config/paths
    (:use #:cl)
  (:export #:*clpm-config-directories*
           #:clpm-config-pathname))

(in-package #:clpm/config/paths)

(defvar *clpm-config-directories* nil
  "A list of directory pathnames where configurations can be found.")

(defun system-config-directories ()
  "Returns the pathnames to the system-wide default config directories."
  (uiop:system-config-pathnames "clpm/"))

(defun user-config-directories ()
  "Returns the pathnames to the user's XDG default config directories."
  (append
   (when (uiop:os-windows-p)
     (list (uiop:resolve-absolute-location (list (uiop:get-folder-path :local-appdata)
                                                 "clpm"
                                                 "config")
                                           :ensure-directory t)))
   (uiop:xdg-config-pathnames "clpm/")))

(defparameter *default-clpm-config-directories*
  (list 'user-config-directories
        'system-config-directories)
  "A list of functions to call that generate the default CLPM config directory
pathnames.")

(defun compute-clpm-config-dirs ()
  "Compute ~*clpm-config-directories*~ using ~*default-clpm-config-directories*~
and the CLPM_CONFIG_DIRS environment variable. The pathnames from
~*default-clpm-config-directories*~ are spliced in wherever there is an empty
directory in CLPM_CONFIG_DIRS."
  (let* ((env-dirs (uiop:getenv-absolute-directories "CLPM_CONFIG_DIRS"))
         (nil-cell (member nil env-dirs))
         (*default-clpm-config-directories* (reduce #'append (mapcar #'funcall *default-clpm-config-directories*))))
    (if env-dirs
        (progn
          (when nil-cell
            (setf (car nil-cell)
                  (first *default-clpm-config-directories*))
            (setf (cdr nil-cell)
                  (append (rest *default-clpm-config-directories*)
                          (cdr nil-cell))))
          (setf *clpm-config-directories* env-dirs))
        (setf *clpm-config-directories* *default-clpm-config-directories*))))

(defun clear-clpm-config-directories ()
  "Clear the ~*clpm-config-directories*~ variable."
  (setf *clpm-config-directories* nil))

(uiop:register-clear-configuration-hook 'clear-clpm-config-directories)
(uiop:register-image-restore-hook 'compute-clpm-config-dirs)

(defun clpm-config-pathname (x &key (direction :input) ensure-directory)
  "Given a list of directories, optionally ending with a file name and type,
~x~ relative to an element of ~*clpm-config-directories*~, return an absolute
pathname. If ~ensure-directory~ is non-NIL, ensures the returned pathname is a
directory. If ~:direction~ is ~:input~ the pathname to an existing file is
returned. If ~:direction~ is ~:output~, ~x~ is taken to be relaitve to the first
directory in ~*clpm-config-directories*~."
  (let ((files (mapcar (lambda (defaults)
                         (uiop:resolve-absolute-location (list* defaults x)
                                                         :ensure-directory ensure-directory))
                       *clpm-config-directories*)))
    (uiop:find-preferred-file files :direction direction)))
