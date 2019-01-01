(uiop:define-package #:clpm/archives/defs
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/config)
  (:export #:gzipped-archive
           #:gzipped-tar-archive
           #:register-tar-client
           #:tar-archive
           #:tar-client-available-p
           #:unarchive
           #:unarchive-tar
           #:%unarchive))

(in-package #:clpm/archives/defs)

(defvar *all-tar-clients* nil)

(defvar *available-tar-clients* nil)

(defun clear-available-tar-clients ()
  (setf *available-tar-clients* nil))

(uiop:register-clear-configuration-hook 'clear-available-tar-clients)

(defgeneric tar-client-available-p (client))

(defun register-tar-client (key class)
  (pushnew (cons key class) *all-tar-clients* :test #'equal))

(defun make-tar-client (pair)
  (destructuring-bind (key . class)
      pair
    (apply #'make-instance class
           (awhen (config-value :archives key)
             (hash-table-plist it)))))

(defun compute-available-tar-clients ()
  (let ((client-list (mapcar (lambda (x)
                               (cons (car x) (make-tar-client x)))
                             *all-tar-clients*)))
    (remove-if-not #'tar-client-available-p client-list :key #'cdr)))

(defun available-tar-clients ()
  (unless *available-tar-clients*
    (setf *available-tar-clients* (compute-available-tar-clients)))
  *available-tar-clients*)

(defun get-preferred-tar-client ()
  (let* ((client-key (config-value :archives :tar-method))
         (available-clients (available-tar-clients))
         (client (if (eql :auto client-key)
                     (cdr (first available-clients))
                     (assoc-value available-clients client-key))))
    (unless client
      (error "Unable to find a tar client."))
    client))

(defclass tar-archive ()
  ())

(defclass gzipped-archive ()
  ())

(defclass gzipped-tar-archive (gzipped-archive tar-archive)
  ())

(defgeneric unarchive (archive-type archive-stream destination-pathname))

(defmethod unarchive ((archive-type symbol) archive-stream destination-pathname)
  (unarchive (make-instance archive-type) archive-stream destination-pathname))

(defmethod unarchive ((archive-type tar-archive) archive-stream destination-pathname)
  (unarchive-tar (get-preferred-tar-client) archive-stream destination-pathname))

(defgeneric unarchive-tar (client archive-stream destination-pathname))
