;;;; When loaded, this builds a dynamically linked CLPM.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:cl-user)

(load (merge-pathnames "common.lisp"
                       *load-truename*))

(in-package #:clpm-scripts)

(setup-asdf "default")

(format uiop:*stderr*
        "I will build CLPM from sources located at ~A~%The built files will be located at ~Abin/~%~%~%"
        *root-pathname*
        *build-root-pathname*)

(asdf:load-system :clpm-cli)

(when (uiop:featurep :windows)
  (uiop:copy-file (merge-pathnames "License.rtf" *root-pathname*)
                  (merge-pathnames "License.rtf" *build-root-pathname*)))

(asdf:make :clpm-cli)
