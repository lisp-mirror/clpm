(uiop:define-package #:clpm/sources/config
    (:use #:cl
          #:alexandria
          #:clpm/config
          #:clpm/sources/defs
          #:clpm/sources/quicklisp
          #:clpm/utils)
  (:import-from #:uiop
                #:read-file-form
                #:with-safe-io-syntax)
  (:export #:load-source-from-form
           #:load-sources))

(in-package #:clpm/sources/config)

(defun validate-source (source)
  "Ensures that the source has a name, and a type."
  (let* ((name (source/name source)))
    (assert (and (stringp name)
                 (not (string-equal "" name))))))

(defun resolve-type (type)
  (ecase type
    (:quicklisp
     'quicklisp-source)))

(defun load-source-from-form (f)
  (destructuring-bind (name &rest args &key type url &allow-other-keys)
      f
    (assert (stringp name))
    (assert (stringp url))
    (assert (keywordp type))
    (let* ((trimmed-args (remove-from-plist args :url :type))
           (new-args (loop
                       :for key :in trimmed-args :by #'cddr
                       :for value :in (rest trimmed-args) :by #'cddr
                       :collect key
                       :collect value))
           (source (apply #'make-instance (resolve-type type)
                          :name name
                          :url url
                          new-args)))
      (validate-source source)
      source)))

(defun load-sources ()
  (let ((sources-table (config-value :sources))
        (source-forms nil))
    (maphash (lambda (k v)
               (push (cons k (hash-table-plist v)) source-forms))
             sources-table)
    (mapcar #'load-source-from-form source-forms)))
