;;;; Basic installation support
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/install/defs
    (:use #:cl
          #:clpm/sources/defs)
  (:export #:install-release))

(in-package #:clpm/install/defs)

(defgeneric install-release (release)
  (:documentation "Install a ~release~."))
