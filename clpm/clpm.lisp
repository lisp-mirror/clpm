;;;; Common Lisp Package Manager - CLPM.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/clpm
    (:nicknames #:clpm)
  (:use #:cl
        #:clpm/bundle
        #:clpm/config
        #:clpm/context
        #:clpm/context-diff
        #:clpm/install
        #:clpm/source
        #:clpm/sync
        #:clpm/update
        #:clpm/version)
  (:export #:install)
  ;; From config
  (:export #:config-value)
  ;; From source
  (:export #:source-name
           #:sources
           #:sync-source)
  ;; From sync
  (:export #:sync)
  ;; From update
  (:export #:update)
  ;; From version
  (:export #:clpm-version))

(in-package #:clpm/clpm)
