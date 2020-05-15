;;;; clpm clpi
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/clpi
    (:use #:cl
          #:clpm-cli/commands/clpi/common
          #:clpm-cli/commands/clpi/release))

(in-package #:clpm-cli/commands/clpi)
