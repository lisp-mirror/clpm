;;;; clpm client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/client
    (:use #:cl
          #:clpm/cli/client/common
          #:clpm/cli/client/rc))

(in-package #:clpm/cli/client)
