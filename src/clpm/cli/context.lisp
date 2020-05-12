;;;; Context command definitions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/context
    (:use #:cl
          #:clpm/cli/context/common
          #:clpm/cli/context/find
          #:clpm/cli/context/output-translations
          #:clpm/cli/context/pathnames
          #:clpm/cli/context/source-registry
          #:clpm/cli/context/systems
          #:clpm/cli/context/visible-systems))

(in-package #:clpm/cli/context)
