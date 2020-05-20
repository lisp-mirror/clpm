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
           #:*context-diff-approval-method*
           #:*default-context*
           #:asdf-integration-active-p
           #:activate-asdf-integration
           #:activate-context
           #:active-context
           #:approve-diff
           #:bundle-init
           #:cleanup-clpm-client
           #:clpm-client-version
           #:clpm-error
           #:clpm-error-error-output
           #:clpm-error-wrapped-condition
           #:clpm-system-definition-pre-search
           #:clpm-system-definition-search
           #:clpm-version
           #:context-diff
           #:context-diff-empty-p
           #:context-diff-needs-approval
           #:context-diff-needs-approval-diff
           #:context-diff-release-diffs
           #:context-source-registry
           #:deactivate-asdf-integration
           #:default-context
           #:inside-bundle-exec-p
           #:install
           #:install-and-reload-config
           #:install-without-dependencies-and-reload-config
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
           #:release-diff-project-name
           #:reload-config
           #:reresolve-requirements-and-reload-config
           #:sync
           #:update))

(in-package #:clpm-client)

;; Register the fact that the clpm client is available by adding :clpm-client to
;; *FEATURES*.
(pushnew :clpm-client *features*)
