;;;; Miscellaneous utilities
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/utils
    (:use #:cl
          #:alexandria
          #:anaphora
          #:puri
          #:split-sequence
          #:trivial-gray-streams
          #:uuid)
  (:export #:*live-script-location*
           #:ensure-uri-scheme-https!
           #:merge-hts
           #:posix-environment-alist
           #:process-stream
           #:retriable-error
           #:run-program-augment-env-args
           #:safe-sort
           #:sort-plist
           #:uri-to-string
           #:url-port
           #:with-forms-from-stream
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
    (awhen (uiop:process-info-input proc)
      (close it))
    (awhen (uiop:process-info-output proc)
      (close it))
    (awhen (uiop:process-info-error-output proc)
      (close it))
    (uiop:wait-process proc))
  (close (process-stream-true-stream stream)))

(defun call-with-forms-from-stream (stream thunk)
  (let ((sentinel (gensym)))
    (loop
      (let ((f (read stream nil sentinel)))
        (when (eq f sentinel)
          (return))
        (funcall thunk f)))))

(defmacro with-forms-from-stream ((stream form-binding) &body body)
  `(call-with-forms-from-stream ,stream (lambda (,form-binding) ,@body)))

(defun merge-hts (new-ht default-ht)
  "Recursively merge two hash tables. If a key is present in ~new-ht~ its value
overwrites the value from ~default-ht~."
  (let ((out (copy-hash-table default-ht)))
    (maphash
     (lambda (k v)
       (multiple-value-bind (default-v exists-p)
           (gethash k out)
         (cond
           ((or
             (hash-table-p v)
             (hash-table-p default-v))
            ;; Either both must be hash tables or the default shouldn't exist.
            (unless (or (not exists-p)
                        (and (hash-table-p default-v)
                             (hash-table-p v)))
              (error "Cannot merge ~S with ~S" v default-v))
            ;; Merge the hash tables together
            (if (null default-v)
                (setf (gethash k out) v)
                (setf (gethash k out) (merge-hts v default-v))))
           ((or
             (listp v)
             (and exists-p (listp default-v)))
            ;; The new value is a list. The default value must be a list.
            (unless (or (not exists-p)
                        (and (listp default-v)
                             (listp v)))
              (error "Cannot merge ~S with ~S" v default-v))
            ;; Append the lists together.
            (setf (gethash k out) (append v default-v)))
           (t
            (setf (gethash k out) v)))))
     new-ht)
    out))

(defun sort-plist (plist order)
  (let ((missing (gensym))
        (out))
    (loop
      :for key :in order
      :for value := (getf plist key missing)
      :unless (eql value missing)
        :do (push key out)
            (push value out))
    (loop
      :for key :in plist :by #'cddr
      :for value :in (rest plist) :by #'cddr
      :unless (member key order)
        :do (push key out)
            (push value out))
    (nreverse out)))

(defun safe-sort (sequence predicate &rest args &key key)
  (declare (ignore key))
  (apply #'sort (copy-seq sequence) predicate args))
