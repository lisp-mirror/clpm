;;;; Context command definitions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/context
    (:use #:cl
          #:clpm-cli/commands/context/common
          #:clpm-cli/commands/context/find
          #:clpm-cli/commands/context/output-translations
          #:clpm-cli/commands/context/pathnames
          #:clpm-cli/commands/context/source-registry
          #:clpm-cli/commands/context/systems
          #:clpm-cli/commands/context/visible-systems))

(in-package #:clpm-cli/commands/context)
