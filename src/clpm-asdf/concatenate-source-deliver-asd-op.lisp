;;;; Support for concatenating source and asd files for a package inferred
;;;; system
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-asdf/concatenate-source-deliver-asd-op
    (:use #:cl
          #:asdf
          #:clpm-asdf/concatenate-package-inferred-system-source-op)
  (:export #:concatenate-source-deliver-asd-op))

(in-package #:clpm-asdf/concatenate-source-deliver-asd-op)

(defclass concatenate-source-deliver-asd-op (selfward-operation)
  ((selfward-operation
    :initform 'concatenate-package-inferred-system-source-op)))

(defmethod input-files ((op concatenate-source-deliver-asd-op) (s system))
  (list (system-source-file s)
        (output-file (selfward-operation op) s)))

(defmethod output-files ((op concatenate-source-deliver-asd-op) (c system))
  (values (list (uiop:subpathname (component-pathname c)
                                  (asdf::component-build-pathname c)
                                  :type "asd")
                (uiop:subpathname (component-pathname c)
                                  (asdf::component-build-pathname c)
                                  :type "lisp"))
          t))

(defmethod perform ((op concatenate-source-deliver-asd-op) (c system))
  (let ((output-asd-file (uiop:subpathname (component-pathname c)
                                           (asdf::component-build-pathname c)
                                           :type "asd"))
        (output-lisp-file (output-file (selfward-operation op) c))
        (desired-output-lisp-file (uiop:subpathname (component-pathname c)
                                                    (asdf::component-build-pathname c)
                                                    :type "lisp")))

    (ensure-directories-exist output-asd-file)
    (unless (uiop:pathname-equal output-lisp-file desired-output-lisp-file)
      (uiop:copy-file output-lisp-file desired-output-lisp-file))

    (with-open-file (s output-asd-file
                       :direction :output
                       :if-exists :supersede)
      (uiop:with-safe-io-syntax (:package :asdf-user)
        (let ((*print-case* :downcase))
          (prin1 `(defsystem ,(component-name c)
                    :version ,(component-version c)
                    :description ,(system-description c)
                    :license ,(system-license c)
                    :components ((:file ,(pathname-name desired-output-lisp-file))))
                 s))))))
