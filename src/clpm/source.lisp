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
           #:sources
           #:with-sources))

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
    (t
     (let ((source (find source-designator (sources) :key #'source-name :test #'equal)))
       (when (and (not source) errorp)
         (error "Unable to find source named ~S" source-designator))
       source))))

(defun call-with-sources (sources thunk)
  (let ((*sources* sources))
    (funcall thunk)))

(defmacro with-sources ((sources) &body body)
  `(call-with-sources ,sources (lambda () ,@body)))

(uiop:register-clear-configuration-hook 'clear-sources)
