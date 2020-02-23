;;;; The CLPM client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/clpm-client
    (:nicknames #:clpm-client)
  (:use #:cl
        #:clpm-client/asdf
        #:clpm-client/bundle
        #:clpm-client/cleanup
        #:clpm-client/clpm
        #:clpm-client/version)
  (:export #:*clpm-cleanup-on-dump-p*
           #:*clpm-context*
           #:*clpm-executable*
           #:*clpm-system-not-found-behavior*
           #:activate-clpm-asdf-integration
           #:cleanup-clpm!
           #:clpm-asdf-integration-active-p
           #:clpm-install-and-reload-bundle
           #:clpm-install-system
           #:clpm-install-system-and-dependencies
           #:clpm-missing-system
           #:clpm-client-version
           #:clpm-inside-bundle-exec-p
           #:clpm-reload-bundle
           #:clpm-version
           #:clpmfile-pathname
           #:deactivate-clpm-asdf-integration))

(in-package #:clpm-client)

;; Register the fact that the clpm client is available by adding :clpm-client to
;; *FEATURES*.
(pushnew :clpm-client *features*)
