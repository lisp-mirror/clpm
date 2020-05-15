;;;; Definitions for using the default values as a CLPM config source.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/config/default-source
    (:use #:cl
          #:clpm/config/defs
          #:clpm/config/source-defs)
  (:export #:config-default-source))

(in-package #:clpm/config/default-source)

(defclass config-default-source (config-source)
  ()
  (:documentation
   "A config source that simply returns the default values."))

(defmethod config-source-value ((config-source config-default-source) path)
  (let ((config-entry (get-config-entry path)))
    (values (getf (cdr config-entry) :default)
            t)))

(defmethod config-source-implicit-keys ((config-source config-default-source) path)
  "The default config provides no implicit keys."
  nil)
