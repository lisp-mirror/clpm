;;;; Common Lisp Package Manager - CLPM.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/clpm
    (:nicknames #:clpm)
  (:use #:cl
        #:clpm/bundle
        #:clpm/context
        #:clpm/context-diff
        #:clpm/install
        #:clpm/source
        #:clpm/version)
  (:export #:install))

(in-package #:clpm/clpm)
