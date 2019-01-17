;;;; Definitions for pluggable HTTP clients.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/http-client/defs
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/config
          #:iterate)
  (:export #:fetch-url-to-stream
           #:%fetch-url-to-stream
           #:http-client
           #:http-client-available-p
           #:http-fetch-error
           #:http-simple-fetch-error
           #:register-http-client))

(in-package #:clpm/http-client/defs)

(defclass http-client ()
  ((priority
    :initarg :priority
    :reader http-client-priority)))

(defvar *all-http-clients* nil
  "A list of all HTTP clients loaded into the image. An alist that maps
a (keyword) name to a class name.")

(defvar *available-http-clients* nil
  "A list of HTTP clients that are loaded into the image *and* have all their
dependencies (such as external programs) met. An alist that maps a (keyword)
name to a client instance. Computed when necessary by
~available-http-clients~.")

(defun register-http-client (key class)
  "Register a new ~key~, ~class~ pair in ~*all-http-clients*~."
  (pushnew (cons key class) *all-http-clients* :test #'equal))

(defun clear-available-clients ()
  "Clear the list of available http clients."
  (setf *available-http-clients* nil))

;; Make sure the available HTTP clients are cleared on image dump.
(uiop:register-clear-configuration-hook 'clear-available-clients)

(defun make-http-client (pair)
  "Given a name/class pair, instantiate the class, using any setting specified
by the user's config."
  (destructuring-bind (key . class)
      pair
    (apply #'make-instance class
           (awhen (config-value :http-client key)
             (hash-table-plist it)))))

(defun compute-available-http-clients ()
  "Compute an alist suitable for storing in
~*available-http-clients*~. Instantiate all registered HTTP clients, then remove
the ones where ~http-client-available-p~ returns NIL."
  (let ((client-list (mapcar (lambda (x)
                               (cons (car x) (make-http-client x)))
                             *all-http-clients*)))
    (remove-if-not #'http-client-available-p client-list :key #'cdr)))

(defun available-http-clients ()
  "Return an alist of all available HTTP clients. Caches results in
~*available-http-clients*~."
  (unless *available-http-clients*
    (setf *available-http-clients* (compute-available-http-clients)))
  *available-http-clients*)

(defun get-preferred-http-client ()
  "Return the HTTP client instance that is available and most preferred."
  (let* ((client-key (config-value :http-client :method))
         (available-http-clients (available-http-clients))
         (client (if (eql :auto client-key)
                     (iter
                       (for client :in (mapcar #'cdr available-http-clients))
                       (finding client :minimizing (http-client-priority client)))
                     (assoc-value available-http-clients client-key))))
    (unless client
      (error "Unable to find an HTTP client."))
    client))


;; * HTTP Client API

(defgeneric http-client-available-p (client)
  (:documentation
   "Returns T iff ~client~ is able to be used to fetch resources over HTTP."))

(define-condition http-fetch-error (error)
  ()
  (:documentation "An error condition raised when there is a problem fetching a
resource over HTTP."))

(define-condition http-simple-fetch-error (http-fetch-error simple-error)
  ())

(defgeneric %fetch-url-to-stream (client url stream &key headers)
  (:documentation "Using ~client~, GET the resource located at ~url~ and send it
to ~stream~. ~headers~ is an alist of header name (keyword)/value pairs to send
with the request."))


;; * GETing URLs

(defun fetch-url-to-stream (url stream &key headers)
  "Given a ~url~, GET it and put its contents into ~stream~. ~headers~ is an
alist of header name (keyword)/value pairs to send with the request."
  (%fetch-url-to-stream (get-preferred-http-client) url stream :headers headers))
