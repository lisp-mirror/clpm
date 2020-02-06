;;;; Basic installation support
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/install/defs
    (:use #:cl
          #:clpm/sources/defs
          #:clpm/sources/vcs)
  (:export #:install-release))

(in-package #:clpm/install/defs)

(defgeneric install-release (release)
  (:documentation "Install a ~release~."))

(defmethod install-release ((release vcs-release))
  (ensure-vcs-release-installed! release))
