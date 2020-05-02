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
          #:clpm/sources/ql-clpi
          #:clpm/sources/quicklisp
          #:clpm/sources/vcs
          #:clpm/utils)
  (:import-from #:uiop
                #:read-file-form
                #:with-safe-io-syntax)
  (:export #:load-source-from-form
           #:load-sources))

(in-package #:clpm/sources/config)

(defun resolve-type (type)
  (ecase type
    (:clpi
     'clpi-dual-source)
    (:file-system
     'fs-source)
    (:ql-clpi
     'ql-clpi-dual-source)
    (:quicklisp
     'ql-source)
    (:vcs
     'vcs-source)))

(defun load-source-from-form (f)
  (destructuring-bind (name &rest args &key type &allow-other-keys)
      f
    (assert (or (eql type :file-system)
                (eql type :vcs)
                (stringp name)))
    (assert (keywordp type))
    (apply #'make-source
           (resolve-type type)
           :name name
           (remove-from-plist args :type))))

(defun load-sources ()
  (let ((pn (clpm-config-pathname '("sources.conf"))))
    (when pn
      (uiop:with-safe-io-syntax ()
        (mapcar #'load-source-from-form (uiop:read-file-forms pn))))))
