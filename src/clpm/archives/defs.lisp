;;;; Support for extracting archives.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

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

;; * Maintaining and filtering tar clients

(defvar *all-tar-clients* nil
  "A list of all tar clients loaded into the image. An alist that maps
a (keyword) name to a class name.")

(defvar *available-tar-clients* nil
  "A list of tar clients that are loaded into the image *and* have all their
dependencies (such as external programs) met. An alist that maps a (keyword)
name to a client instance. Computed when necessary by ~available-tar-clients~.")

(defun register-tar-client (key class)
  "Register a new ~key~, ~class~ pair in ~*all-tar-clients*~."
  (pushnew (cons key class) *all-tar-clients* :test #'equal))

(defun clear-available-tar-clients ()
  "Clear the list of available tar clients."
  (setf *available-tar-clients* nil))

;; Make sure the available tar clients are cleared on image dump.
(uiop:register-clear-configuration-hook 'clear-available-tar-clients)

(defun make-tar-client (pair)
  "Given a name/class pair, instantiate the class, using any settings specified
by the user's config."
  (destructuring-bind (key . class)
      pair
    (apply #'make-instance class
           (awhen (config-value :archives key)
             (hash-table-plist it)))))

(defun compute-available-tar-clients ()
  "Compute an alist suitable for storing in ~*available-tar-clients*~.
Instantiate all registered tar clients, then remove the ones where
~tar-client-available-p~ returns NIL."
  (let ((client-list (mapcar (lambda (x)
                               (cons (car x) (make-tar-client x)))
                             *all-tar-clients*)))
    (remove-if-not #'tar-client-available-p client-list :key #'cdr)))

(defun available-tar-clients ()
  "Return an alist of all available tar clients. Caches results in
~*available-tar-clients*~."
  (unless *available-tar-clients*
    (setf *available-tar-clients* (compute-available-tar-clients)))
  *available-tar-clients*)

(defun get-preferred-tar-client ()
  "Return the tar client instance that is available and most preferred."
  (let* ((client-key (config-value :archives :tar-method))
         (available-clients (available-tar-clients))
         (client (if (eql :auto client-key)
                     (cdr (first available-clients))
                     (assoc-value available-clients client-key))))
    (unless client
      (error "Unable to find a tar client."))
    client))


;; * Tar client API

(defgeneric tar-client-available-p (client)
  (:documentation
   "Returns T iff ~client~ is able to be used to extract tar files."))

(defgeneric unarchive-tar (client archive-stream destination-pathname
                           &key strip-components)
  (:documentation
   "Given a tar archive in ~archive-stream~, extract its contents to
~destination-pathname~ using ~client~."))


;; * Extracting archives

(defclass tar-archive ()
  ()
  (:documentation
   "An archive in tar format."))

(defclass gzipped-archive ()
  ()
  (:documentation
   "An archive that is compressed using gzip."))

(defclass gzipped-tar-archive (gzipped-archive tar-archive)
  ()
  (:documentation
   "A gzipped tar archive."))

(defgeneric unarchive (archive-type archive-stream destination-pathname
                       &key strip-components)
  (:documentation
   "Given an archive contained in ~archive-stream~, extract its contents to
~destination-pathname~. ~archive-type~ is used to dispatch to the correct
methods (this does *not* look at ~archive-stream~ to guess what the correct
archive type is)"))

(defmethod unarchive ((archive-type symbol) archive-stream destination-pathname
                      &key strip-components)
  "If ~archive-type~ is a symbol, instantiate the class it names and call
again."
  (unarchive (make-instance archive-type) archive-stream destination-pathname
             :strip-components strip-components))

(defmethod unarchive ((archive-type tar-archive) archive-stream destination-pathname
                      &key strip-components)
  "If ~archive-type~ is a ~tar-archive~, dispatch to ~unarchive-tar~ using the
preferred tar client."
  (unarchive-tar (get-preferred-tar-client) archive-stream destination-pathname
                 :strip-components strip-components))
