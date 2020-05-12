;;;; Common Lisp Package Manager - CLPM.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/clpm
    (:nicknames #:clpm)
  (:use #:cl
        #:clpm/bundle
        #:clpm/cli/bundle
        #:clpm/cli/client
        #:clpm/cli/clpi
        #:clpm/cli/config
        #:clpm/cli/context
        #:clpm/cli/entry
        #:clpm/cli/exec
        #:clpm/cli/hack
        #:clpm/cli/install
        #:clpm/cli/license-info
        #:clpm/cli/sync
        #:clpm/cli/update
        #:clpm/cli/version
        #:clpm/context
        #:clpm/deploy
        #:clpm/install
        #:clpm/man
        #:clpm/source
        #:clpm/version))

(in-package #:clpm/clpm)
