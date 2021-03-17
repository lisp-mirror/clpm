;;;; Common Lisp Project Manager CLI - CLPM-CLI
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands
    (:use #:cl
          #:clpm-cli/commands/asd-pathnames
          #:clpm-cli/commands/bundle
          #:clpm-cli/commands/client
          #:clpm-cli/commands/clpi
          #:clpm-cli/commands/config
          #:clpm-cli/commands/exec
          #:clpm-cli/commands/find
          #:clpm-cli/commands/hack
          #:clpm-cli/commands/install
          #:clpm-cli/commands/license-info
          #:clpm-cli/commands/output-translations
          #:clpm-cli/commands/source-registry
          #:clpm-cli/commands/sync
          #:clpm-cli/commands/update
          #:clpm-cli/commands/version))

(in-package #:clpm-cli/commands)
