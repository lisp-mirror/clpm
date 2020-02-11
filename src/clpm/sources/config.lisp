;;;; Configuring sources
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/sources/config
    (:use #:cl
          #:alexandria
          #:clpm/config
          #:clpm/sources/clpi
          #:clpm/sources/defs
          #:clpm/sources/fs
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
  (let* ((name (source-name source)))
    (assert (and (stringp name)
                 (not (string-equal "" name))))))

(defun resolve-type (type)
  (ecase type
    (:clpi
     'clpi-source)
    (:quicklisp
     'ql-flat-source)))

(defun load-source-from-form (f)
  (if (and (listp f) (eql (first f) :file-system))
      (fs-source-from-form f)
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
          source))))

(defun load-sources ()
  (let ((pn (clpm-config-pathname '("sources.conf"))))
    (when (probe-file pn)
      (uiop:with-safe-io-syntax ()
        (mapcar #'load-source-from-form (uiop:read-file-forms pn))))))
