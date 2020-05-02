;;;; Definitions for using CLI options as a config source
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/config/cli-source
    (:use #:cl
          #:clpm/config/defs
          #:clpm/config/source-defs)
  (:export #:config-cli-source))

(in-package #:clpm/config/cli-source)

(defclass config-cli-source ()
  ((local))
  (:documentation
   "A configuration source backed by CLI arguments."))

(defmethod initialize-instance :after ((config-source config-cli-source)
                                       &key arg-ht)
  (multiple-value-bind (value exists-p)
      (gethash :cli-config-local arg-ht)
    (when exists-p
      (setf (slot-value config-source 'local) value))))

(defmethod config-source-value ((config-source config-cli-source) path)
  (when (and (equal path '(:local))
             (slot-boundp config-source 'local))
    (values (slot-value config-source 'local) t)))

(defmethod config-source-implicit-keys ((config-source config-cli-source) path)
  nil)
