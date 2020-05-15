;;;; clpm config
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/config
    (:use #:cl
          #:clpm-cli/commands/config/common
          #:clpm-cli/commands/config/info))

(in-package #:clpm-cli/commands/config)
