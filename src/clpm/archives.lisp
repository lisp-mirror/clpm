(uiop:define-package #:clpm/archives
    (:use #:cl
          #:clpm/archives/defs
          #:clpm/archives/chipz
          #:clpm/archives/tar)
  (:reexport #:clpm/archives/defs))

(in-package #:clpm/archives)
