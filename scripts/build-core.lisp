;;;; When loaded, this builds a core for CLPM.
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
(require :sb-posix)
(asdf:load-system :clpm-cli)

(setf uiop:*image-entry-point* (uiop:find-symbol* :main :clpm-cli/entry))

(dolist (lib sb-alien::*shared-objects*)
  (setf (sb-alien::shared-object-dont-save lib) t))

(defun my-arch ()
  #+x86-64 :amd64)

(uiop:dump-image (merge-pathnames (format nil "~A-~A/clpm.core"
                                          (string-downcase (uiop:operating-system))
                                          (string-downcase (my-arch)))
                                  *build-root-pathname*))
