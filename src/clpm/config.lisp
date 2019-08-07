;;;; Support for generating pathnames to files in CLPM's config directories on
;;;; the filesystem and for reading said config.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/config
    (:use #:cl
          #:alexandria
          #:iterate)
  (:export #:*clpm-config-directories*
           #:clpm-config-pathname
           #:config-value
           #:merge-file-into-config!
           #:merge-ht-into-config!
           #:print-config))

(in-package #:clpm/config)

;; * Pathnames

(defvar *clpm-config-directories* nil
  "A list of directory pathnames where configurations can be found.")

(defun system-config-directories ()
  "Returns the pathnames to the system-wide default config directories."
  (uiop:system-config-pathnames "common-lisp" "clpm/"))

(defun user-config-directories ()
  "Returns the pathnames to the user's XDG default config directories."
  (uiop:xdg-config-pathnames "common-lisp" "clpm/"))

(defparameter *default-clpm-config-directories*
  (list 'user-config-directories
        'system-config-directories)
  "A list of functions to call that generate the default CLPM config directory
pathnames.")

(defun compute-clpm-config-dirs ()
  "Compute ~*clpm-config-directories*~ using ~*default-clpm-config-directories*~
and the CLPM_CONFIG_DIRS environment variable. The pathnames from
~*default-clpm-config-directories*~ are spliced in wherever there is an empty
directory in CLPM_CONFIG_DIRS."
  (let* ((env-dirs (uiop:getenv-absolute-directories "CLPM_CONFIG_DIRS"))
         (nil-cell (member nil env-dirs))
         (*default-clpm-config-directories* (reduce #'append (mapcar #'funcall *default-clpm-config-directories*))))
    (if env-dirs
        (progn
          (when nil-cell
            (setf (car nil-cell)
                  (first *default-clpm-config-directories*))
            (setf (cdr nil-cell)
                  (append (rest *default-clpm-config-directories*)
                          (cdr nil-cell))))
          (setf *clpm-config-directories* env-dirs))
        (setf *clpm-config-directories* *default-clpm-config-directories*))))

(defun clear-clpm-config-directories ()
  "Clear the ~*clpm-config-directories*~ variable."
  (setf *clpm-config-directories* nil))

(uiop:register-clear-configuration-hook 'clear-clpm-config-directories)
(uiop:register-image-restore-hook 'compute-clpm-config-dirs)

(defun clpm-config-pathname (x &key (direction :input) ensure-directory)
  "Given a list of directories, optionally ending with a file name and type,
~x~ relative to an element of ~*clpm-config-directories*~, return an absolute
pathname. If ~ensure-directory~ is non-NIL, ensures the returned pathname is a
directory. If ~:direction~ is ~:input~ the pathname to an existing file is
returned. If ~:direction~ is ~:output~, ~x~ is taken to be relaitve to the first
directory in ~*clpm-config-directories*~."
  (let ((files (mapcar (lambda (defaults)
                         (uiop:resolve-absolute-location (list* defaults x)
                                                         :ensure-directory ensure-directory))
                       *clpm-config-directories*)))
    (uiop:find-preferred-file files :direction direction)))



;; * Configuration variables and merging configs

(defparameter *default-config*
  '(progn
    (version "0.1")

    ((table :grovel :sandbox)
     :method :auto)

    ((table :archives)
     :tar-method :auto)

    ((table :archives :tar)
     :path "tar")

    ((table :http-client)
     :method :auto)

    ((table :http-client :curl)
     :path "curl"))
  "Default configuration of clpm.")

(defvar *config* nil
  "Active configuration. Computed at program execution time.")

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

(defun merge-ht-into-config! (new-ht)
  "Merge a hash table into the active config."
  (setf *config* (merge-hts new-ht *config*)))


;; * Parsing configs

(defgeneric parse-config-value (value))

(defmethod parse-config-value ((value t))
  value)

(defmethod parse-config-value ((value list))
  (assert (eql 'table (first value)))
  (parse-config-form value))

(defgeneric %parse-config-form (first rest table))

(defmethod %parse-config-form ((first (eql 'table)) rest table)
  (loop
    :for key :in rest :by #'cddr
    :for value :in (rest rest) :by #'cddr
    :do (setf (gethash key table) (parse-config-value value))))

(defmethod %parse-config-form ((first list) rest table)
  (destructuring-bind (first &rest path) first
    (assert (eql 'table first))
    (let ((sub-table (make-hash-table :test 'equalp)))
      (%parse-config-form 'table rest sub-table)
      (setf (gethashes* table path) sub-table))))

(defmethod %parse-config-form ((first (eql 'progn)) rest table)
  (mapc (rcurry #'parse-config-form table) rest)
  table)

(defmethod %parse-config-form ((first (eql 'version)) rest table)
  (assert (equal (first rest) "0.1")))

(defun parse-config-form (form &optional (table (make-hash-table :test 'equalp)))
  "Given a top level config represented as a ~(clpm-config plist)~, return a
hash table representing the config."
  (assert (listp form))
  (%parse-config-form (first form) (rest form) table)
  table)

(defun merge-file-into-config! (pathname)
  "Read a config form from ~pathname~ and merge it into the currently active
config."
  (uiop:with-safe-io-syntax (:package :clpm/config)
    (dolist (subform (uiop:read-file-forms pathname))
      (merge-ht-into-config! (parse-config-form subform)))))

(defun load-global-config ()
  "Seed ~*config*~ with ~*default-config*~ and merge the primary CLPM config
file (clpm.conf) into ~*active*~."
  (setf *config* (parse-config-form *default-config*))
  (let* ((config-file (clpm-config-pathname '("clpm.conf"))))
    (when config-file
      (merge-file-into-config! config-file)))
  *config*)

(defun clear-global-config ()
  "Clear the ~*config*~ variable."
  (setf *config* nil))


;; * Querying the config


(defun gethashes* (hash-table-or-value keys)
  "Given a hash table and a list of keys, look up the value in the hash table."
  (if keys
      (when hash-table-or-value
        (gethashes* (gethash (first keys) hash-table-or-value) (rest keys)))
      hash-table-or-value))

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

(defun config-value (&rest path)
  "Get the configuration value located at ~path~."
  (gethashes* *config* path))

(defun flatten-hts (ht)
  "Given a hash table ~ht~, recursively flatten it into a plist."
  (iter
    (for (key value) :on (hash-table-plist ht) :by #'cddr)
    (collect key)
    (if (hash-table-p value)
        (collect (flatten-hts value))
        (collect value))))

(defun print-table (table path stream)
  (let ((tables nil)
        (values nil))
    (maphash (lambda (k v)
               (if (hash-table-p v)
                   (push (cons (append path (list k)) v) tables)
                   (push (cons k v) values)))
             table)
    (when values
      (let ((*print-pretty* t)
            (*print-case* :downcase))
        (pprint-logical-block (stream values :prefix "(" :suffix ")")
          (format stream "(table ~{~S~^ ~})" path)
          (pprint-exit-if-list-exhausted)
          (pprint-newline :mandatory stream)
          (loop
            (let* ((pair (pprint-pop))
                   (key (car pair))
                   (value (cdr pair)))
              (prin1 key stream)
              (write-char #\Space stream)
              (prin1 value stream)
              (pprint-exit-if-list-exhausted)
              (pprint-newline :mandatory stream))))
        (pprint-newline :mandatory stream)
        (terpri stream)
        (terpri stream)))
    (dolist (sub-table tables)
      (print-table (cdr sub-table) (car sub-table) stream))))

(defun print-config (stream)
  "Print the configuration to ~stream~."
  (format stream "(version \"0.1\")~%~%")
  (maphash (lambda (k v)
             (assert (hash-table-p v))
             (print-table v (list k) stream))
           *config*))

(uiop:register-clear-configuration-hook 'clear-global-config)
(uiop:register-image-restore-hook 'load-global-config)
