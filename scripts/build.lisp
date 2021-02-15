;;;; When loaded, this builds a dynamically linked CLPM.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:cl-user)

(load (merge-pathnames "common.lisp" *load-truename*))

(in-package #:clpm-scripts)

(setup-asdf)

(format uiop:*stderr*
        "I will build CLPM from sources located at ~A~%The built files will be located at ~Abin/~%~%~%"
        *root-pathname*
        *build-root-pathname*)

(asdf:make :clpm)
