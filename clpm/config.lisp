;;;; Support for generating pathnames to files in CLPM's config directories on
;;;; the filesystem and for reading said config.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/config
    (:use #:cl
          #:alexandria
          #:clpm/config/cli-source
          #:clpm/config/default-source
          #:clpm/config/defs
          #:clpm/config/env-source
          #:clpm/config/file-source
          #:clpm/config/paths
          #:clpm/config/source-defs
          #:clpm/session
          #:clpm/utils
          #:iterate)
  (:export #:*clpm-config-directories*
           #:clpm-config-pathname
           #:config-table-keys
           #:config-value
           #:print-config
           #:with-config-source)
  (:import-from #:cl-ppcre))

(in-package #:clpm/config)

(defvar *config-sources* (list 'config-env-source
                               'config-file-source
                               'config-default-source))



(defun get-config-source (config-source-specifier)
  (with-clpm-session (:key `(get-config-source ,config-source-specifier))
    (when config-source-specifier
      (etypecase config-source-specifier
        (config-source
         config-source-specifier)
        (list
         (apply #'make-instance config-source-specifier))
        (symbol
         (make-instance config-source-specifier))))))

(defun config-type-p (obj type)
  (or (and (typep obj 'config-source)
           (typep obj type))
      (and (listp obj)
           (subtypep (first obj) type))
      (subtypep obj type)))

(defun config-source-type (config-source)
  (etypecase config-source
    (config-source
     (type-of config-source))
    (symbol
     config-source)
    (list
     (first config-source))))

(defun add-config-source (config-sources config-source)
  (ecase (config-source-type config-source)
    (config-cli-source
     ;; CLI sources go first.
     (list* config-source config-sources))
    (config-file-source
     ;; File sources go after CLI and env sources.
     (flet ((env-or-cli-source-p (x)
              (or (config-type-p x 'config-env-source)
                  (config-type-p x 'config-cli-source))))
       (append (remove-if-not #'env-or-cli-source-p config-sources)
               (list config-source)
               (remove-if #'env-or-cli-source-p config-sources))))))

(defun call-with-config-source (thunk &key config-source pathname options-ht)
  "If CONFIG-SOURCE is already on *CONFIG-SOURCES*, just call THUNK, otherwise,
rebind *CONFIG-SOURCES* with CONFIG-SOURCE placed in the appropriate position
and then call THUNK."
  (assert (xor config-source pathname options-ht))
  (when pathname
    (setf config-source (list 'config-file-source :pathname (pathname pathname))))
  (when options-ht
    (setf config-source (list 'config-cli-source :arg-ht options-ht)))
  (if (member config-source *config-sources* :test #'equal)
      (funcall thunk)
      (let ((*config-sources* (add-config-source *config-sources* config-source)))
        (funcall thunk))))

(defmacro with-config-source ((&key config-source pathname options-ht) &body body)
  `(call-with-config-source (lambda () ,@body) :config-source ,config-source
                                               :pathname ,pathname
                                               :options-ht ,options-ht))



(defun config-table-keys (&rest path)
  "Return a list of keys in the table rooted at PATH. This currently does n"
  (with-clpm-session (:key `(config-table-keys ,*config-sources* ,@path))
    (let ((defined-children (get-children-of-config-path path)))
      (if (equal defined-children '(:*))
          (remove-duplicates (mapcan (compose (rcurry #'config-source-implicit-keys path)
                                              #'get-config-source)
                                     *config-sources*)
                             :test #'equal)
          defined-children))))

(defun config-value (&rest path)
  "Get the configuration value located at path. First search environment
variables, then the config file, then the default config."
  (with-clpm-session (:key `(config-value ,*config-sources* ,@path))
    (let* ((config-info (get-config-entry path))
           (type (getf (cdr config-info) :type)))
      (assert config-info)
      (if (eql type 'hash-table)
          (let ((keys (apply #'config-table-keys path))
                (out (make-hash-table :test 'equal)))
            (dolist (key keys)
              (setf (gethash key out) (apply #'config-value (append path (list key)))))
            out)
          (loop
            :for config-source-specifier :in *config-sources*
            :for config-source := (get-config-source config-source-specifier)
            :for (value exists-p) := (multiple-value-list (config-source-value config-source path))
            :until exists-p
            :finally
               (return value))))))

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
          (format stream "(~{~S~^ ~})" path)
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
  (format stream "(version \"0.2\")~%~%")
  (print-table (config-value) nil stream))
