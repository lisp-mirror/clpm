;;;; Common Lisp Package Manager - CLPM.

;; * Package Definition
(uiop:define-package #:clpm/clpm
    (:use #:cl
          #:clpm/cli/bundle
          #:clpm/cli/config
          #:clpm/cli/entry
          #:clpm/cli/license-info
          #:clpm/cli/sync))

(in-package #:clpm/clpm)
