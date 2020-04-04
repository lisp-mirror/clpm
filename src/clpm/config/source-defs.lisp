;;;; Definitions for CLPM configuration sources.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/config/source-defs
    (:use #:cl)
  (:export #:config-source-implicit-keys
           #:config-source-value))

(in-package #:clpm/config/source-defs)

(defgeneric config-source-value (config-source path))

(defgeneric config-source-implicit-keys (config-source path)
  (:documentation
   "Given a path ending in a wildcard, return all possible values for the
wildcard defined in this config source."))
