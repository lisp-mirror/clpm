;;;; Common Lisp Package Manager CLI - CLPM-CLI
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/clpm-cli
    (:nicknames #:clpm-cli)
  (:use #:cl
        #:clpm-cli/bundle
        #:clpm-cli/client
        #:clpm-cli/clpi
        #:clpm-cli/config
        #:clpm-cli/context
        #:clpm-cli/deploy
        #:clpm-cli/entry
        #:clpm-cli/exec
        #:clpm-cli/hack
        #:clpm-cli/install
        #:clpm-cli/license-info
        #:clpm-cli/man
        #:clpm-cli/sync
        #:clpm-cli/update
        #:clpm-cli/version))

(in-package #:clpm-cli)
