;;;; CLPM Build helpers
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-build/clpm-build
    (:nicknames #:clpm-build)
  (:use #:cl
        #:clpm-build/features))

(in-package #:clpm-build/clpm-build)
