;;;; Pluggable sandbox clients.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/sandbox
    (:use #:cl
          #:clpm/sandbox/defs
          #+clpm-firejail #:clpm/sandbox/firejail)
  (:reexport #:clpm/sandbox/defs))

(in-package #:clpm/sandbox)
