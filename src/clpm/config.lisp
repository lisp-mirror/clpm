;;;; Support for generating pathnames to files in CLPM's config directories on
;;;; the filesystem and for reading said config.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/config
    (:use #:cl
          #:alexandria
          #:clpm/utils
          #:iterate)
  (:export #:*clpm-config-directories*
           #:clpm-config-pathname
           #:config-add-file-source!
           #:config-table-keys
           #:config-value
           #:print-config)
  (:import-from #:cl-ppcre))

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

(defparameter *config-info*
  `((()
     :type hash-table)

    ((:archives)
     :type hash-table)
    ((:archives :tar)
     :type hash-table)
    ((:archives :tar :type)
     :type (member :auto :tar)
     :default :auto
     :documentation
     "The tar implementation to use.")

    ((:context)
     :type string
     :default "default"
     :documentation
     "The name of the default context.")

    ((:contexts)
     :type hash-table)
    ((:contexts :*)
     :wildcard-types (string)
     :type hash-table)
    ((:contexts :* :source-registry.d-files)
     :wildcard-types (string)
     :type (list (or string pathname))
     :documentation
     "Used to inform ASDF where to find systems for this context. Outputs the same contents to every file in a format suitable for inclusion in a source-registry.d directory (see ASDF manual).")
    ((:contexts :* :source-registry-files)
     :wildcard-types (string)
     :type (list (or string pathname))
     :documentation
     "Used to inform ASDF where to find systems for this context. Outputs the same contents to every file in a source-registry.conf format (see ASDF manual).")

    ((:curl)
     :type hash-table)
    ((:curl :path)
     :type (or string pathname)
     :default "curl"
     :documentation
     "The path to the curl executable.")

    ((:drakma)
     :type hash-table)

    ((:git)
     :type hash-table)
    ((:git :path)
     :type (or string pathname)
     :default "git"
     :documentation
     "The path to the git executable.")
    ((:git :remotes)
     :type hash-table)
    ((:git :remotes :*)
     :wildcard-types (hostname)
     :type hash-table)
    ((:git :remotes :* :type)
     :wildcard-types (hostname)
     :type (member :gitlab)
     :documentation
     "The type of git server hosted at this hostname.")
    ((:git :remotes :* :method)
     :wildcard-types (hostname)
     :type (member :ssh :https)
     :documentation
     "The preferred method of connecting to this server.")
    ((:git :remotes :* :username)
     :wildcard-types (hostname)
     :type string
     :default "git"
     :documentation
     "The username to use when connecting to this server.")
    ((:git :remotes :* :password)
     :wildcard-types (hostname)
     :type string
     :documentation
     "The password to use when connecting to this server. Only used in HTTPS method.")

    ((:grovel)
     :type hash-table)
    ((:grovel :sandbox)
     :type hash-table)
    ((:grovel :sandbox :type)
     :type (member :auto :firejail)
     :default :auto
     :documentation
     "The sandbox type to use when groveling.")

    ((:http)
     :type hash-table)
    ((:http :headers)
     :type hash-table)
    ((:http :headers :*)
     :wildcard-types (hostname)
     :type hash-table)
    ((:http :headers :* :*)
     :wildcard-types (hostname header)
     :type hash-table)
    ((:http :headers :* :* :secure-only-p)
     :wildcard-types (hostname header)
     :type boolean
     :default nil
     :documentation
     "If non-NIL, then the specified header should only be sent to hostname if the connection is secure.")
    ((:http :headers :* :* :contents)
     :wildcard-types (hostname header)
     :type (or string pathname)
     :default nil
     :documentation
     "If set, the contents of this file are sent as the header value.")
    ((:http :headers :* :* :exec)
     :wildcard-types (hostname header)
     :type (or string pathname)
     :documentation
     "If set, it must name a program that must print the value of the header to stdout.")
    ((:http :headers :* :* :value)
     :wildcard-types (hostname header)
     :type string
     :documentation
     "If set, the value of the header to send.")

    ((:http-client)
     :type hash-table)
    ((:http-client :type)
     :type (member :auto
                   ,@(when (featurep :clpm-drakma) (list :drakma))
                   ,@(when (featurep :clpm-curl) (list :curl)))
     :default :auto
     :documentation
     "The HTTP client implementation to use.")

    ((:tar)
     :type hash-table)
    ((:tar :path)
     :type (or string pathname)
     :default "tar"
     :documentation
     "The path to the tar executable.")))

(defun paths-match (canonical-path path)
  (and (length= canonical-path path)
       (every (lambda (left right)
                (or (eql left :*)
                    (equal left right)))
              canonical-path path)))

(defun get-config-entry (path)
  (find-if (rcurry #'paths-match path) *config-info* :key #'car))

(defvar *config-sources* nil)

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


;; * Parsing configs

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
    (uiop:with-safe-io-syntax (:package :clpm/config)
      (with-forms-from-stream (stream f)
        (if version-checked-p
            (parse-toplevel-config-form f out)
            (if (equal f '(version "0.2"))
                (setf version-checked-p t)
                (error "Unknown config version statement: ~S" f)))))
    out))

(defun load-config-from-file (pn)
  (with-open-file (s pn)
    (load-config-from-stream s)))

(defun load-global-config ()
  "Seed *config-sources* with the default config and primary CLPM config
file (clpm.conf)."
  (let ((config-file (clpm-config-pathname '("clpm.conf"))))
    (setf *config-sources*
          (list (if config-file
                    (load-config-from-file config-file)
                    (make-hash-table :test 'equal)))))
  *config-sources*)

(defun config-add-file-source! (pn)
  (when (uiop:probe-file* pn)
    (push (load-config-from-file pn) *config-sources*)))

(defun clear-global-config ()
  "Clear the *config-sources* variable."
  (setf *config-sources* nil))


;; * Querying the config


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

(defun path-to-env-var (path config-info)
  "Given a config path, return the environment variable which would contain its
value."
  (let* ((wildcard-types (getf (cdr config-info) :wildcard-types))
         (canonical-path (car config-info))
         (type (getf (cdr config-info) :type))
         (hash-table-p (eql type 'hash-table)))
    (format nil "CLPM_~{~A~^_~}~:[~;_~]"
            (mapcar (lambda (x)
                      (if (eql (pop canonical-path) :*)
                          (ecase (pop wildcard-types)
                            ((header string)
                             (if (stringp x)
                                 (string-upcase
                                  (cl-ppcre:regex-replace-all "-" x "_"))
                                 (string-upcase
                                  (cl-ppcre:regex-replace-all "-" (symbol-name x) "_"))))
                            (hostname
                             (uiop:strcat
                              (string-upcase
                               (cl-ppcre:regex-replace-all #\. (cl-ppcre:regex-replace-all "-" x "_") "__"))
                              "_")))
                          x))
                    path)
            hash-table-p)))

(defun parse-env-value (value type)
  (cond
    ((eql type 'string)
     value)
    ((eql type 'boolean)
     (let ((value (string-downcase value))
           (orig-value value))
       (cond
         ((or (equal value "0")
              (equal value "n")
              (equal value "no")
              (equal value "false"))
          nil)
         ((or (equal value "1")
              (equal value "y")
              (equal value "yes")
              (equal value "true"))
          t)
         (t
          (error "Unable to parse ~S as a boolean." orig-value)))))
    ((and (listp type)
          (eql (first type) 'member)
          (every #'keywordp (rest type)))
     (let ((kw (make-keyword (uiop:standard-case-symbol-name value))))
       (unless (typep kw type)
         (error "Unknown value ~S for type ~S" kw type))
       kw))
    ((equal type '(or string pathname))
     (uiop:parse-native-namestring value))
    (t
     (error "Unknown type ~S to parse from a string." type))))

(defun config-table-keys (&rest path)
  "Return a list of keys in the table rooted at PATH. This currently does not
look at environment variables."
  (let* ((config-info (get-config-entry path))
         (type (getf (cdr config-info) :type))
         (canonical-path (car config-info)))
    (assert config-info)
    (assert (eql type 'hash-table))
    (let ((sub-paths (loop
                       :for info :in *config-info*
                       :for info-path := (car info)
                       :when (and (starts-with-subseq canonical-path info-path)
                                  (length= (1+ (length path)) info-path))
                         :collect info-path)))
      (if (and (length= 1 sub-paths)
               (eql :* (last-elt (first sub-paths))))
          (let ((tables (mapcar (rcurry #'gethashes* path) *config-sources*)))
            (remove-duplicates (mapcan #'hash-table-keys (remove nil tables))
                               :test #'equal))
          (mapcar #'last-elt sub-paths)))))

(defun config-value (&rest path)
  "Get the configuration value located at path. First search environment
variables, then the config file, then the default config."
  (let* ((config-info (get-config-entry path))
         (type (getf (cdr config-info) :type)))
    (assert config-info)
    (if (eql type 'hash-table)
        (let ((keys (apply #'config-table-keys path))
              (out (make-hash-table :test 'equal)))
          (dolist (key keys)
            (setf (gethash key out) (apply #'config-value (append path (list key)))))
          out)
        (let ((env-var-name (path-to-env-var path config-info)))
          (if-let ((env-value (uiop:getenv env-var-name)))
            (parse-env-value env-value type)
            (loop
              :for config-source :in *config-sources*
              :for (value exists-p) := (multiple-value-list (gethashes* config-source path))
              :until exists-p
              :finally
                 (if exists-p
                     (return value)
                     (return (getf (cdr config-info) :default)))))))))

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

;; (defun print-config (stream)
;;   "Print the configuration to ~stream~."
;;   (format stream "(version \"0.2\")~%~%")
;;   (maphash (lambda (k v)
;;              (assert (hash-table-p v))
;;              (print-table v (list k) stream))
;;            *config*))

(uiop:register-clear-configuration-hook 'clear-global-config)
(uiop:register-image-restore-hook 'load-global-config)
