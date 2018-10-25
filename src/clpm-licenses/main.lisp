;;;; CLPM Licenses. This file contains code to load the licenses of CLPM's
;;;; dependencies into the image.

(uiop:define-package #:clpm-licenses
    (:use #:cl)
  (:export #:*clpm-license*
           #:*licenses*
           #:*notices*))

(in-package #:clpm-licenses)

(defparameter *license-root-dir*
  (asdf:system-relative-pathname :clpm-licenses "licenses/")
  "Points to the root directory of licenses in the CLPM source folder. Is
cleared when UIOP's clear configuration hook is run (such as on image dump).")

(defun clear-root-dir ()
  (setf *license-root-dir* nil))

(uiop:register-clear-configuration-hook 'clear-root-dir)

(defparameter *licenses* (make-hash-table :test 'equalp)
  "A hash table that maps project names to license strings.")

(defparameter *notices* (make-hash-table :test 'equalp)
  "A hash table that maps project names to NOTICE strings.")

(defparameter *clpm-license* nil)

(defun load-licenses ()
  "Load all licenses from *license-root-dir*."
  (let ((files (uiop:directory* (uiop:wilden *license-root-dir*))))
    (dolist (f files)
      (let ((name (pathname-name f))
            (type (pathname-type f))
            (contents (uiop:read-file-string f)))
        (unless (equal name "README")
          (if (equal type "NOTICE")
              (setf (gethash name *notices*) contents)
              (setf (gethash name *licenses*) contents)))))
    (setf *clpm-license* (uiop:read-file-string (asdf:system-relative-pathname :clpm-licenses "LICENSE")))))

(eval-when (:load-toplevel)
  (load-licenses))
