;;;; Miscellaneous utilities
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/utils
    (:use #:cl
          #:alexandria
          #:puri
          #:split-sequence
          #:trivial-gray-streams)
  (:export #:*live-script-location*
           #:ensure-uri-scheme-https!
           #:mktemp
           #:posix-environment-alist
           #:process-stream
           #:retriable-error
           #:run-program-augment-env-args
           #:uri-to-string
           #:url-port
           #:with-retries))

(in-package #:clpm/utils)

(defvar *live-script-location* nil
  "If loaded from scripts/clpm-live, this is set to the pathname where the
clpm-live script is located.")

(defun clear-live-script-location ()
  "On image dump, remove the pathname to the clpm-live script."
  (setf *live-script-location* nil))
(uiop:register-image-dump-hook 'clear-live-script-location)

#+sbcl
(defun posix-environment-alist ()
  "Returns an alist representing the environemnt variables."
  (let ((out nil))
    (dolist (pair-string (sb-ext:posix-environ))
      (let* ((pos-of-= (position #\= pair-string))
             (name (subseq pair-string 0 pos-of-=))
             (value (subseq pair-string (1+ pos-of-=))))
        (push (cons name value) out)))
    (nreverse out)))

#-sbcl
(defun posix-environment-alist ()
  "Returns an alist representing the environemnt variables."
  (error "Not implemented"))

#+sbcl
(defun run-program-augment-env-args (new-env-alist)
  "Given an alist of environment variables, return a list of arguments suitable
for ~uiop:{launch/run}-program~ to set the augmented environment for the child
process."
  (let ((env (posix-environment-alist)))
    (dolist (pair new-env-alist)
      (destructuring-bind (name . value) pair
        (setf (assoc-value env name :test #'equal) value)))
    (list :environment
          (mapcar (lambda (c)
                    (concatenate 'string (car c) "=" (cdr c)))
                  env))))

#+ccl
(defun run-program-augment-env-args (new-env-alist)
  (list :env new-env-alist))

#-(or sbcl ccl)
(defun run-program-augment-env-args (new-env-alist)
  (error "not implemented"))

#+(and sbcl unix)
(defun mktemp ()
  "Make a temporary directory and return its pathname."
  (let ((template-pathname (merge-pathnames "clpm-XXXXXX"
                                            (uiop:temporary-directory))))
    (uiop:ensure-directory-pathname (sb-posix:mkdtemp (namestring template-pathname)))))

#-(and sbcl unix)
(defun mktemp ()
  (error "not implemented"))

(defun uri-to-string (uri)
  "Convert a puri URI to a string."
  (with-output-to-string (s)
    (render-uri uri s)))

(defgeneric ensure-uri-scheme-https! (uri)
  (:documentation
   "Given a URI, make sure the scheme is ~:https~. Errors if input is anything
other than ~:https~ or ~:http~."))

(defmethod ensure-uri-scheme-https! ((uri string))
  (uri-to-string (ensure-uri-scheme-https! (parse-uri uri))))

(defmethod ensure-uri-scheme-https! ((uri uri))
  (unless (member (uri-scheme uri) '(:http :https))
    (error "Refusing to change scheme ~S to HTTPS" (uri-scheme uri)))
  (setf (uri-scheme uri) :https)
  uri)

(defun url-port (url)
  (or (puri:uri-port url)
      (ecase (puri:uri-scheme url)
        (:https
         443)
        (:http
         80))))

(define-condition retriable-error (error)
  ())

(defun call-with-retries (thunk &key (max 3) (sleep 1))
  (let ((num-tries 1))
    (block nil
      (tagbody
       top
         (handler-case
             (return (funcall thunk))
           (retriable-error (e)
             (format *error-output* "~&Get error ~S~%" e)
             (when (< num-tries max)
               (incf num-tries)
               (format *error-output* "Sleeping and retrying~%")
               (sleep sleep)
               (go top))
             (error e)))))))

(defmacro with-retries ((&key (max 3) (sleep 1)) &body body)
  `(call-with-retries (lambda () ,@body) :max ,max :sleep ,sleep))

(defclass process-stream (fundamental-binary-input-stream)
  ((true-stream
    :initarg :true-stream
    :reader process-stream-true-stream)
   (process-info
    :initarg :process-info
    :reader process-stream-process-info)))

(defmethod stream-element-type ((stream process-stream))
  '(unsigned-byte 8))

(defmethod stream-read-byte ((stream process-stream))
  (read-byte (process-stream-true-stream stream) nil :eof))

(defmethod close ((stream process-stream) &key abort)
  "Close the underlying stream and cleanup the process. ABORT is ignored."
  (declare (ignore abort))
  (let ((proc (process-stream-process-info stream)))
    (close (uiop:process-info-input proc))
    (close (uiop:process-info-output proc))
    (close (uiop:process-info-error-output proc))
    (uiop:wait-process proc))
  (close (process-stream-true-stream stream)))
