(uiop:define-package #:clpm/sandbox
    (:use #:cl
          #:clpm/sandbox/defs
          #:clpm/sandbox/firejail)
  (:reexport #:clpm/sandbox/defs))

(in-package #:clpm/sandbox)
