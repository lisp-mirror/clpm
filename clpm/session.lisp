;;;; CLPM Sessions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/session
    (:use #:cl)
  (:export #:clpm-session
           #:config-sources
           #:global-sources
           #:with-clpm-session))

(in-package #:clpm/session)

(defclass clpm-session ()
  ((cache
    :initarg :cache
    :initform (make-hash-table :test 'equal)
    :reader clpm-session-cache
    :documentation
    "A hash table used to cache results of function calls.")
   (global-sources
    :initarg :sources
    :accessor clpm-session-global-sources)))

(defvar *session* nil
  "Bound to a CLPM-SESSION object when inside a CLPM session.")

(defun global-sources (&optional (session *session*))
  (clpm-session-global-sources session))

(defun (setf global-sources) (new-value &optional (session *session*))
  (setf (clpm-session-global-sources session) new-value))

(defun call-with-clpm-session (thunk &key override key)
  "Starts a new CLPM session if one does not currently exist (unless OVERRIDE is
non-NIL). A session consists of a function cache, config sources, and config
cache.

If KEY is provided and an entry exists in the session's cache with that
key (compared with EQUAL), it is returned from the cache instead of invoking
THUNK. If THUNK is invoked, its results are stored in the cache."
  (cond
    ((or override (not *session*))
     (let* ((*session* (make-instance 'clpm-session)))
       (funcall thunk)))
    ((and *session* key)
     (multiple-value-bind (cached-values cached-values-exist-p)
         (gethash key (clpm-session-cache *session*))
       (if cached-values-exist-p
           (values-list cached-values)
           (let ((result (multiple-value-list (funcall thunk))))
             (setf (gethash key (clpm-session-cache *session*)) result)
             (values-list result)))))
    (t (funcall thunk))))

(defmacro with-clpm-session ((&key override key) &body body)
  `(call-with-clpm-session (lambda () ,@body)
                           :override ,override
                           :key ,key))
