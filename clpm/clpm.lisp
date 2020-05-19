;;;; Common Lisp Package Manager - CLPM.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/clpm
    (:nicknames #:clpm)
  (:use #:cl
        #:clpm/bundle
        #:clpm/client
        #:clpm/config
        #:clpm/context-diff
        #:clpm/context-queries
        #:clpm/exec
        #:clpm/install
        #:clpm/session
        #:clpm/source
        #:clpm/sync
        #:clpm/update
        #:clpm/version)
  ;; From bundle
  (:export #:bundle-exec
           #:bundle-init
           #:bundle-install)
  ;; From client
  (:export #:client-asd-pathname)
  ;; From config
  (:export #:config-value)
  ;; From context-queries
  (:export #:asd-pathnames
           #:find-system-asd-pathname
           #:output-translations
           #:source-registry)
  ;; From exec
  (:export #:exec)
  ;; From install
  (:export #:install)
  ;; From session
  (:export #:with-clpm-session)
  ;; From sync
  (:export #:sync)
  ;; From update
  (:export #:update)
  ;; From version
  (:export #:clpm-version))

(in-package #:clpm/clpm)
