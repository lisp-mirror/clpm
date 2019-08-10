;;;; Helpers for sources backed by a sqlite database
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/sources/db-backed
    (:use #:cl
          #:alexandria
          #:clpm/sources/defs
          #:exit-hooks
          #:sxql)
  (:import-from #:dbd.sqlite3)
  (:import-from #:dbi)
  (:import-from #:mito.core
                #:*connection*
                #:create-dao
                #:dao-table-class
                #:dao-table-mixin
                #:object-id
                #:save-dao)
  (:import-from #:trivial-garbage
                #:make-weak-hash-table)
  (:export #:*connection*
           #:*current-source*
           #:create-dao
           #:dao-table-class
           #:dao-table-mixin
           #:db-backed-mixin
           #:db-backed-object-source
           #:db-backed-source
           #:db-backed-source-meta
           #:db-source-indices
           #:db-source-meta-class-name
           #:db-source-table-names
           #:find-dao
           #:object-id
           #:retrieve-dao
           #:save-dao
           #:with-source-connection))

(in-package #:clpm/sources/db-backed)

(defun maybe-load-sqlite-lib ()
  (unless (member :cl-sqlite-foreign-libs-already-loaded *features*)
    (sqlite-ffi:load-library)))

(uiop:register-image-restore-hook 'maybe-load-sqlite-lib nil)


;;; * Mito utilities
;;;
;;; We use mito as an ORM on top of a sqlite database.

(defclass db-backed-mixin ()
  ((source
    :initarg :source
    :accessor db-backed-object-source
    :ghost t
    :documentation
    "The source this object is part of. Automatically populated on load from
    the database."))
  (:metaclass dao-table-mixin)
  (:documentation
   "A mixin used to add a reference to the toplevel source an object belongs
   to. If an object of this type is retrieved from the database, the source is
   automatically populated based on the source the connection belongs to."))

(defvar *current-source* nil
  "Bound to the ~db-backed-source~ object in use by the ORM layer (i.e., the
  source used to compute which database to connect to.")

(defvar *dao-cache* (make-weak-hash-table :test 'equal :weakness :value)
  "Some other pieces of CLPM require that two objects representing the same thing
  (i.e., a specific release) be EQ to each other. To facilitate that, this
  caches all results returned from the ORM layer. It is a weak cache so as to
  not prevent GC.")

(defun clear-dao-cache ()
  "Make sure the cache is cleared on image dump."
  (setf *dao-cache* (make-weak-hash-table :test 'equal :weakness :value)))

(uiop:register-image-dump-hook 'clear-dao-cache)

(defun find-dao (class &rest fields-and-values)
  "A wrapper around ~mito:find-dao~ that uses the ~*dao-cache*~ and populates the
  ~source~ slot on ~db-backed-mixin~ objects."
  (let ((result (apply #'mito.core:find-dao class fields-and-values)))
    (when result
      (let ((id (object-id result)))
        (multiple-value-bind (out exists-p)
            (ensure-gethash (list *current-source* class id) *dao-cache* result)
          (when (and (not exists-p)
                     (typep out 'db-backed-mixin))
            (setf (db-backed-object-source out) *current-source*))
          out)))))

(defun retrieve-dao (class &rest fields-and-values)
  "A wrapper around ~mito:retrieve-dao~ that uses the ~*dao-cache*~ and populates
  the ~source~ slot on ~db-backed-mixin~ objects."
  (let ((results (apply #'mito.core:retrieve-dao class fields-and-values)))
    (loop
      :for result :in results
      :for id := (object-id result)
      :for (actual-result exists-p) := (multiple-value-list
                                        (ensure-gethash (list *current-source* class id) *dao-cache* result))
      :collect actual-result
      :when (and (not exists-p) (typep actual-result 'db-backed-mixin))
        :do (setf (db-backed-object-source actual-result) *current-source*))))

(defun seed-empty-db (source conn)
  "Given a connection to an empty database, seed it."
  (let ((*connection* conn))
    ;; Create the tables
    (dolist (table (db-source-table-names source))
      (mito.core:ensure-table-exists table))
    ;; Create the indices
    (dolist (index (db-source-indices source))
      (destructuring-bind (name . on)
          index
        (mito.core:execute-sql (create-index name :on on))))
    ;; Create the metadata table.
    (create-dao (db-source-meta-class-name source))))


;;; * Source objects

(defclass db-backed-source ()
  ((connection
    :accessor source-db-connection
    :documentation
    "Holds a reference to the DB connection for this source, if active."))
  (:documentation
   "Mixin for sources that store data in a sqlite database."))

(defclass db-backed-source-meta ()
  ((id
    :initform 0
    :accessor db-source-meta-id
    :primary-key t
    :reader object-id
    :col-type :integer
    :documentation
    "To make the single row easy to query, give it a constant ID of 0.")
   (db-version
    :initform 1
    :accessor db-source-meta-db-version
    :col-type (:smallint () :unsigned)
    :documentation
    "The version of the database schema in use."))
  (:metaclass dao-table-class)
  (:documentation
   "A table in the database used to store metadata about the source and the
   database itself in a single row."))

(defgeneric db-source-meta-class-name (source))

(defgeneric db-source-table-names (source)
  (:documentation
   "A list of table names used in the database. Used for seeding an empty
  DB."))

(defgeneric db-source-indices (source)
  (:documentation
   "Currently, Mito ignores keys when operating on a sqlite DB. So we manually
  create the indices we're interested in. Used for seeding an empty DB."))

(defun check-schema-or-error (source conn)
  "Make sure the database connected to has a compatible schema. Error if it does
not."
  (let* ((*connection* conn)
         ;; Use the underlying ~find-dao~ to bypass our cache.
         (meta (mito.core:find-dao (db-source-meta-class-name source) :id 0)))
    (unless (= 1 (db-source-meta-db-version meta))
      (error "Unknown DB Schema!"))))

(defmethod slot-unbound (class (self db-backed-source) (slot-name (eql 'connection)))
  "Creates a new connection to ~self~'s database. If the database does not
exist, it is populated with the correct tables and indices. Errors if the
database schema is incorrect."
  (let* ((db-pathname (merge-pathnames "clpm-source.db"
                                       (source/lib-directory self)))
         (db-exists-p (probe-file db-pathname)))
    (assert (uiop:absolute-pathname-p db-pathname))
    (unless db-exists-p
      (ensure-directories-exist db-pathname))
    (let ((connection (dbi:connect :sqlite3
                                   :database-name db-pathname)))
      (unless db-exists-p
        (seed-empty-db self connection))
      (check-schema-or-error self connection)
      (add-exit-hook (lambda () (dbi:disconnect connection)))
      (setf (source-db-connection self) connection))))


;;; * Macros

(defun call-with-source-connection (source thunk)
  "Call ~thunk~ with ~*connection*~ and ~*current-source*~ bound to ~source~'s
  connection and ~source~ respectively."
  (let ((*connection* (source-db-connection source))
        (*current-source* source))
    (funcall thunk)))

(defmacro with-source-connection ((source) &body body)
  `(call-with-source-connection ,source (lambda () ,@body)))
