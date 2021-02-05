;;;; When loaded, this dumps CLPM as an executable, as well as all supporting files
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:cl-user)

(asdf:operate 'clpm-cli/clpm-program-op:clpm-program-op :clpm-cli)
