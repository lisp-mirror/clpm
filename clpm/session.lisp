;;;; CLPM Sessions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/session
    (:use #:cl)
  (:export #:*config-sources*
           #:with-clpm-session))

(in-package #:clpm/session)

(defvar *session-cache* nil
  "Bound to a hash-table when inside a CLPM session. Used to cache function
return values.")

(defvar *config-sources* nil
  "Bound to a list of config source specifiers inside a CLPM session.")

(defun call-with-clpm-session (thunk &key override key)
  "Starts a new CLPM session if one does not currently exist (unless OVERRIDE is
non-NIL).

If KEY is provided and an entry exists in *SESSION-CACHE* with that key, it is
returned from the cache instead of invoking THUNK. If THUNK is invoked, its
results are stored in the cache."
  (cond
    ((or override (not *session-cache*))
     (let* ((*session-cache* (make-hash-table :test 'equal))
            (*config-sources* *config-sources*))
       (funcall thunk)))
    ((and *session-cache* key)
     (multiple-value-bind (cached-values cached-values-exist-p)
         (gethash key *session-cache*)
       (if cached-values-exist-p
           (values-list cached-values)
           (let ((result (multiple-value-list (funcall thunk))))
             (setf (gethash key *session-cache*) result)
             (values-list result)))))
    (t (funcall thunk))))

(defmacro with-clpm-session ((&key override key) &body body)
  `(call-with-clpm-session (lambda () ,@body) :override ,override :key ,key))
