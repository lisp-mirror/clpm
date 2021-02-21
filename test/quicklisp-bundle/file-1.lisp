(uiop:define-package #:quicklisp-bundle
    (:use #:cl
          #:alexandria))

(in-package #:quicklisp-bundle)

(assert (equal (iota 3) '(0 1 2)))
