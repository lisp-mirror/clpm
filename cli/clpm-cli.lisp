;;;; Common Lisp Package Manager CLI - CLPM-CLI
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/clpm-cli
    (:nicknames #:clpm-cli)
  (:use #:cl
        #:clpm-cli/commands
        #:clpm-cli/deploy
        #:clpm-cli/entry
        #:clpm-cli/man))

(in-package #:clpm-cli)
