;;;; Context command definitions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/context
    (:use #:cl
          #:clpm/cli/context/common
          #:clpm/cli/context/pathnames))

(in-package #:clpm/cli/context)
