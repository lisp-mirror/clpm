;;;; Source definitions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/source
    (:use #:cl
          #:clpm/cache
          #:clpm/config
          #:clpm/sources/config
          #:clpm/sources/defs
          #:clpm/sources/fs
          #:clpm/sources/vcs)
  (:reexport #:clpm/sources/config)
  (:reexport #:clpm/sources/defs)
  (:reexport #:clpm/sources/fs)
  (:reexport #:clpm/sources/vcs)
  (:export #:get-source
           #:sources))

(in-package #:clpm/source)

(defvar *sources*)

(defun sources ()
  (unless (boundp '*sources*)
    (setf *sources* (load-sources)))
  *sources*)

(defun clear-sources ()
  (makunbound '*sources*))

(defun get-source (source-designator &optional (errorp t))
  (cond
    ((null source-designator)
     nil)
    ((typep source-designator 'clpm-source)
     source-designator)
    ((stringp source-designator)
     (let ((source (find source-designator (sources) :key #'source-name :test #'equal)))
       (when (and (not source) errorp)
         (error "Unable to find source named ~S" source-designator))
       source))
    (errorp
     (error "Unable to translate ~S to a source object" source-designator))))

(uiop:register-clear-configuration-hook 'clear-sources)
