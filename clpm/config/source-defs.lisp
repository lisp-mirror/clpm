;;;; Definitions for CLPM configuration sources.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/config/source-defs
    (:use #:cl)
  (:export #:config-source
           #:config-source-implicit-keys
           #:config-source-value))

(in-package #:clpm/config/source-defs)

(defclass config-source ()
  ()
  (:documentation
   "Parent class for config sources."))

(defgeneric config-source-value (config-source path))

(defgeneric config-source-implicit-keys (config-source path)
  (:documentation
   "Given a path, return all possible values for the next path segment defined
in this config source. The only defined child to `PATH` MUST be `:*`"))
