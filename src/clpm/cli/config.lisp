;;;; clpm config
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/config
    (:use #:cl
          #:clpm/cli/config/default
          #:clpm/cli/config/info))

(in-package #:clpm/cli/config)
