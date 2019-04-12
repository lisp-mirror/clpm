;;;; The CLPM client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/clpm-client
    (:nicknames #:clpm-client)
  (:use #:cl
        ;; Everything must depend on header so that it comes first in the
        ;; concatenated file.
        #:clpm-client/header
        #:clpm-client/asdf
        #:clpm-client/bundle
        #:clpm-client/cleanup
        #:clpm-client/clpm)
  (:export #:*clpm-cleanup-on-dump-p*
           #:*clpm-executable*
           #:*clpm-system-not-found-behavior*
           #:activate-clpm
           #:cleanup-clpm!
           #:clpm-active-p
           #:clpm-install-system
           #:clpm-missing-system
           #:clpmfile-pathname
           #:deactivate-clpm
           #:inside-bundle-exec-p
           #:run-clpm))

(in-package #:clpm-client)

;; Register the fact that the clpm client is available by adding :clpm-client to
;; *FEATURES*.
(pushnew :clpm-client *features*)
