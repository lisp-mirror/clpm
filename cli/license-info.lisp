;;;; clpm license-info
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/license-info
    (:use #:cl
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:uiop
                #:*stdout*))

(in-package #:clpm-cli/license-info)

(defparameter *license-root-dir*
  (asdf:system-relative-pathname :clpm-cli "licenses/")
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
    (setf *clpm-license* (uiop:read-file-string (asdf:system-relative-pathname :clpm "LICENSE")))))

(eval-when (:load-toplevel)
  (load-licenses))

(defparameter *license-info-ui*
  (adopt:make-interface
   :name "clpm license-info"
   :summary "Common Lisp Package Manager License Info"
   :usage "license-info [options]"
   :help "Display the license info for CLPM and its dependencies."
   :contents (list *group-common*)))

(defparameter *license-separator*
  "
================================================================================
")

(defun print-licenses (stream)
  (format stream "CLPM is licensed under the following terms:~%~%~A~%" *clpm-license*)
  (fresh-line stream)

  (loop
    :for project-name :being :the :hash-keys :in *licenses* :using (hash-value license)
    :for notice := (gethash project-name *notices*)
    :do
       (fresh-line stream)
       (write-string *license-separator* stream)
    :when notice
      :do
         (format stream "~A~%~%~A~%" notice license)
    :else
      :do
         (format stream
                 "CLPM contains code from the ~A project, which is licensed under the following terms:~%~%~A~%"
                 project-name license))
  (fresh-line stream))

(define-cli-command (("license-info") *license-info-ui*) (args options)
  (declare (ignore args options))
  (print-licenses *stdout*)
  t)
