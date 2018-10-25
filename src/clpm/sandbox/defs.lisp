(uiop:define-package #:clpm/sandbox/defs
    (:use #:cl
          #:alexandria)
  (:export #:*sandbox-methods*
           #:available-sandboxes
           #:launch-program-in-sandbox
           #:sandbox-augment-command
           #:sandbox-available-p))

(in-package #:clpm/sandbox/defs)

(defparameter *sandbox-methods*
  (list :firejail))

(defvar *available-sandboxes* nil)

(defun clear-available-sandboxes ()
  (setf *available-sandboxes* nil))

(uiop:register-clear-configuration-hook 'clear-available-sandboxes)

(defgeneric launch-program-in-sandbox (sandbox-method sandbox-args
                                       command &rest args &key &allow-other-keys))

(defgeneric sandbox-available-p (sandbox-method))

(defgeneric sandbox-augment-command (sandbox-method command &key read-write-pathnames))

(defmethod sandbox-augment-command ((sandbox-method string)
                                    command
                                    &rest args
                                    &key &allow-other-keys)
  (apply #'sandbox-augment-command (make-keyword (uiop:standard-case-symbol-name sandbox-method))
         command args))

(defmethod sandbox-augment-command ((sandbox-method (eql :none))
                                    command
                                    &key &allow-other-keys)
  command)

(defun available-sandboxes ()
  (unless *available-sandboxes*
    (setf *available-sandboxes* (remove-if-not #'sandbox-available-p *sandbox-methods*)))
  *available-sandboxes*)

(defmethod sandbox-augment-command ((sandbox-method (eql :auto))
                                    command
                                    &rest args &key &allow-other-keys)
  (let ((available-sandboxes (available-sandboxes)))
    (if available-sandboxes
        command
        (apply #'sandbox-augment-command command (first available-sandboxes)
               args))))
