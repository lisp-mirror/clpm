(uiop:define-package #:clpm/http-client/defs
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/config)
  (:export #:client-available-p
           #:fetch-error
           #:fetch-url-to-stream
           #:%fetch-url-to-stream
           #:register-http-client
           #:simple-fetch-error))

(in-package #:clpm/http-client/defs)

(defvar *all-http-clients* nil)

(defvar *available-http-clients* nil)

(defun clear-available-clients ()
  (setf *available-http-clients* nil))

(uiop:register-clear-configuration-hook 'clear-available-clients)

(defgeneric client-available-p (client))

(defun register-http-client (key class)
  (pushnew (cons key class) *all-http-clients* :test #'equal))

(defun make-client (pair)
  (destructuring-bind (key . class)
      pair
    (apply #'make-instance class
           (awhen (config-value :http-client key)
             (hash-table-plist it)))))

(defun compute-available-clients ()
  (let ((client-list (mapcar (lambda (x)
                               (cons (car x) (make-client x)))
                             *all-http-clients*)))
    (remove-if-not #'client-available-p client-list :key #'cdr)))

(defun available-clients ()
  (unless *available-http-clients*
    (setf *available-http-clients* (compute-available-clients)))
  *available-http-clients*)

(define-condition fetch-error (error)
  ())

(define-condition simple-fetch-error (fetch-error simple-error)
  ())

(defgeneric %fetch-url-to-stream (client url stream &key headers))

(defun get-preferred-client ()
  (let* ((client-key (config-value :http-client :method))
         (available-clients (available-clients))
         (client (if (eql :auto client-key)
                     (cdr (first available-clients))
                     (assoc-value available-clients client-key))))
    (unless client
      (error "Unable to find an HTTP client."))
    client))

(defun fetch-url-to-stream (url stream &key headers)
  (%fetch-url-to-stream (get-preferred-client) url stream :headers headers))
