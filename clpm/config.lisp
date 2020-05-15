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
           #:config-add-cli-source!
           #:config-add-file-source!
           #:config-table-keys
           #:config-value
           #:print-config
           #:with-config-file-source-added)
  (:import-from #:cl-ppcre))

(in-package #:clpm/config)

(defparameter *default-config-sources* (list 'config-env-source
                                             'config-file-source
                                             'config-default-source))



(defun initialize-config-sources ()
  (setf *config-sources* *default-config-sources*))
(uiop:register-image-restore-hook 'initialize-config-sources)

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

(defun config-add-cli-source! (ht)
  (push (make-instance 'config-cli-source
                       :arg-ht ht)
        *config-sources*))

(defun config-type-p (obj type)
  (or (and (typep obj 'config-source)
           (typep obj type))
      (and (listp obj)
           (subtypep (first obj) type))
      (subtypep obj type)))

(defun config-add-file-source! (pn)
  (when (uiop:probe-file* pn)
    (setf *config-sources*
          (append (remove-if-not (lambda (x)
                                   (or
                                    (config-type-p x 'config-env-source)
                                    (config-type-p x 'config-cli-source)))
                                 *config-sources*)
                  (list (make-instance 'config-file-source :pathname pn))
                  (remove-if (lambda (x)
                               (or
                                (config-type-p x 'config-env-source)
                                (config-type-p x 'config-cli-source)))
                             *config-sources*)))))

(defun call-with-config-file-source-added (thunk pn)
  "Call THUNK with PN added to the config sources in the dynamic environment."
  (let ((*config-sources* *config-sources*))
    (config-add-file-source! pn)
    (funcall thunk)))

(defmacro with-config-file-source-added ((pn) &body body)
  `(call-with-config-file-source-added (lambda () ,@body) ,pn))

(defun clear-global-config ()
  "Clear the *config-sources* variable."
  (setf *config-sources* nil))




(defun config-table-keys (&rest path)
  "Return a list of keys in the table rooted at PATH. This currently does n"
  (with-clpm-session (:key `(config-table-keys ,@path))
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
  (with-clpm-session (:key `(config-value ,@path))
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

(uiop:register-clear-configuration-hook 'clear-global-config)
