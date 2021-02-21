;;;; Script to print the CLPM version number
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:cl-user)

(load (merge-pathnames "common.lisp" *load-truename*))

(in-package #:clpm-scripts)

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (asdf:load-system :clpm-asdf))

(write-string (clpm-asdf::clpm-version))
(terpri)
