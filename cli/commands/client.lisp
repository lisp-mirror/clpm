;;;; clpm client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/client
    (:use #:cl
          #:clpm-cli/commands/client/cat
          #:clpm-cli/commands/client/common
          #:clpm-cli/commands/client/rc
          #:clpm-cli/commands/client/repl
          #:clpm-cli/commands/client/source-registry.d))

(in-package #:clpm-cli/commands/client)
