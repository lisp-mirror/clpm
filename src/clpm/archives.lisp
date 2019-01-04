;;;; Support for extracting archives.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/archives
    (:use #:cl
          #:clpm/archives/defs
          #:clpm/archives/chipz
          #:clpm/archives/tar)
  (:reexport #:clpm/archives/defs))

(in-package #:clpm/archives)
