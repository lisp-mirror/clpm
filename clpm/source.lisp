;;;; Source definitions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/source
    (:use #:cl
          #:clpm/cache
          #:clpm/config
          #:clpm/session
          #:clpm/sources/config
          #:clpm/sources/defs
          #:clpm/sources/fs
          #:clpm/sources/vcs)
  (:reexport #:clpm/sources/config)
  (:reexport #:clpm/sources/defs)
  (:reexport #:clpm/sources/fs)
  (:reexport #:clpm/sources/vcs)
  (:export #:get-source
           #:get-vcs-source-for-project
           #:sources
           #:with-sources))

(in-package #:clpm/source)

(defmethod slot-unbound (class (session clpm-session) (slot-name (eql 'global-sources)))
  (setf (global-sources session) (load-global-sources)))

(defvar *visible-sources*)

(defun sources ()
  (if (boundp '*visible-sources*)
      *visible-sources*
      (global-sources)))

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

(defun get-vcs-source-for-project (project-name &optional (errorp t))
  (or
   (loop :for vcs-source :in (remove-if-not (lambda (x) (typep x 'vcs-source)) (sources))
         :when (equal project-name (project-name (vcs-source-project vcs-source)))
           :do (return vcs-source))
   (when errorp
     (error "Unable to find VCS source for project ~S" project-name))))

(defun call-with-sources (sources thunk)
  (let ((*visible-sources* sources))
    (funcall thunk)))

(defmacro with-sources ((sources) &body body)
  `(call-with-sources ,sources (lambda () ,@body)))
