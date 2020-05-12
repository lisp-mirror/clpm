;;;; The CLPM client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client
    (:use #:cl)
  (:export #:*asdf-system-not-found-behavior*
           #:*cleanup-on-dump-p*
           #:*clpm-dribble*
           #:*clpm-dribble-input-prefix*
           #:*clpm-dribble-output-prefix*
           #:*clpm-error-dribble*
           #:*clpm-executable*
           #:*default-context*
           #:asdf-integration-active-p
           #:activate-asdf-integration
           #:approve-diff
           #:bundle-init
           #:cleanup-clpm-client
           #:clpm-client-version
           #:clpm-error
           #:clpm-error-error-output
           #:clpm-error-wrapped-condition
           #:clpm-system-definition-search
           #:clpm-version
           #:context
           #:context-diff
           #:context-diff-empty-p
           #:context-diff-needs-approval
           #:context-diff-needs-approval-diff
           #:context-diff-release-diffs
           #:context-source-registry
           #:deactivate-asdf-integration
           #:enter-context
           #:inside-bundle-exec-p
           #:install
           #:maybe-cleanup-clpm-client
           #:missing-system
           #:missing-system-name
           #:print-context-diff-to-stream
           #:reject-diff
           #:release-diff
           #:release-diff-new-source
           #:release-diff-new-version
           #:release-diff-old-source
           #:release-diff-old-version
           #:release-diff-project-name))

(in-package #:clpm-client)

;; Register the fact that the clpm client is available by adding :clpm-client to
;; *FEATURES*.
(pushnew :clpm-client *features*)
