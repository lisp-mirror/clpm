;;;; Interface for caching the clpm-deps system, used to grovel system info
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/deps
    (:use #:cl)
  (:export #:*clpm-groveler-asd-pathname*
           #:deps-asd-pathname))

(in-package #:clpm/deps)

(defvar *clpm-groveler-asd-pathname*
  (asdf:system-relative-pathname :clpm "src/clpm-groveler/clpm-groveler.asd"))

(defun deps-asd-pathname ()
  "Return the pathname to the deps .asd file."
  *clpm-groveler-asd-pathname*)
