;;;; When loaded, this builds a dynamically linked CLPM.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:cl-user)

(load (merge-pathnames "common.lisp"
                       *load-truename*))

(in-package #:clpm-scripts)

(load (merge-pathnames "../logical-pathname.lisp" *load-truename*))

(setup-asdf "default")

(format uiop:*stderr*
        "I will build CLPM from sources located at ~A~%The built files will be located at ~Abin/~%~%~%"
        *root-pathname*
        *build-root-pathname*)

(asdf:load-system :clpm)

(when (and (uiop:featurep :linux)
           (equal uiop:*command-line-arguments* (list "release")))
  (format uiop:*stderr* "Performing release build. Setting default CLPM home.~%~%")
  (setf (symbol-value (uiop:find-symbol* '#:*default-clpm-home* '#:clpm/deploy))
        "/usr/local/lib/clpm"))

(when (uiop:featurep :windows)
  (uiop:copy-file (merge-pathnames "License.rtf" *root-pathname*)
                  (merge-pathnames "License.rtf" *build-root-pathname*)))

(asdf:make :clpm)
