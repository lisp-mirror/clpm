;;;; Definitions for using files as config sources for CLPM.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/config/file-source
    (:use #:cl
          #:alexandria
          #:clpm/config/paths
          #:clpm/config/source-defs
          #:clpm/utils)
  (:export #:config-file-source))

(in-package #:clpm/config/file-source)

(defclass config-file-source (config-source)
  ((root-ht
    :accessor config-file-source-root-ht)
   (pathname
    :reader config-file-source-pathname
    :initarg :pathname
    :initform (clpm-config-pathname '("clpm.conf"))))
  (:documentation
   "A configuration source backed by a file."))

(defmethod initialize-instance :after ((config-source config-file-source) &rest initargs)
  (declare (ignore initargs))
  (if (probe-file (config-file-source-pathname config-source))
      (with-open-file (s (config-file-source-pathname config-source))
        (setf (config-file-source-root-ht config-source)
              (load-config-from-stream s)))
      (setf (config-file-source-root-ht config-source) (make-hash-table :test 'equal))))

(defgeneric parse-config-value (value))

(defmethod parse-config-value ((value t))
  value)

(defun parse-config-table (table-form)
  (pop table-form)
  (let ((out (make-hash-table :test 'equal)))
    (loop
      :for key :in table-form :by #'cddr
      :for raw-value :in (rest table-form) :by #'cddr
      :for value := (parse-config-value raw-value)
      :do (setf (gethash key out) value))
    out))

(defmethod parse-config-value ((form list))
  (if (eql 'table (first form))
      (parse-config-table form)
      form))

(defun parse-toplevel-config-form (form table)
  (destructuring-bind (path &rest values) form
    ;; VALUES may have a single element or have multiple elements. If it has
    ;; multiple elements, it is treated as a table.
    (let ((value (if (length= 1 values)
                     (parse-config-value (first values))
                     (parse-config-value (list* 'table values)))))
      (multiple-value-bind (orig-value orig-value-exists-p) (gethashes* table path)
        (if orig-value-exists-p
            (cond
              ((and (hash-table-p orig-value) (hash-table-p value))
               (setf (gethashes* table path) (merge-hts value orig-value)))
              ((or (hash-table-p orig-value) (hash-table-p value))
               (error "Unable to merge a table with a value at ~S" path))
              (t
               (setf (gethashes* table path) value)))
            (setf (gethashes* table path) value)))))
  table)

(defun parse-toplevel-config-forms (forms)
  (let ((out (make-hash-table :test 'equal)))
    (mapcar (rcurry #'parse-toplevel-config-form out) forms)
    out))

(defun load-config-from-stream (stream)
  "Given a stream containing CLPM config forms, return a hash table representing
it."
  (let ((out (make-hash-table :test 'equal))
        (version-checked-p nil))
    (uiop:with-safe-io-syntax (:package :clpm/config/file-source)
      (with-forms-from-stream (stream f)
        (if version-checked-p
            (parse-toplevel-config-form f out)
            (if (equal f '(version "0.2"))
                (setf version-checked-p t)
                (error "Unknown config version statement: ~S" f)))))
    out))

(defun gethashes* (hash-table-or-value keys)
  "Given a hash table and a list of keys, look up the value in the hash table."
  (if keys
      (when hash-table-or-value
        (multiple-value-bind (next-table exists-p) (gethash (first keys) hash-table-or-value)
          (when exists-p
            (gethashes* next-table (rest keys)))))
      (values hash-table-or-value t)))

(defun (setf gethashes*) (value hash-table keys)
  "Modify the value in ~hash-table~ addressed by the list of ~keys~."
  (if (rest keys)
      (setf (gethashes* (ensure-gethash (first keys) hash-table
                                        (make-hash-table :test 'equalp))
                        (rest keys))
            value)
      (setf (gethash (first keys) hash-table) value)))

(defun gethashes (hash-table &rest keys)
  "Given a hash table and a list of keys, look up the value in the hash table."
  (gethashes* hash-table keys))

(defun (setf gethashes) (value hash-table &rest keys)
  "Modify the value in ~hash-table~ addressed by the list of ~keys~."
  (setf (gethashes* hash-table keys) value))

(defmethod config-source-value ((config-source config-file-source) path)
  (gethashes* (config-file-source-root-ht config-source) path))

(defmethod config-source-implicit-keys ((config-source config-file-source) path)
  "Just look up the parent hash table and return the keys."
  (let ((table (gethashes* (config-file-source-root-ht config-source) path)))
    (when table
      (hash-table-keys table))))
