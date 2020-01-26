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

;; Special handling for Windows builds.
(when (uiop:os-windows-p)
  (pushnew :deploy-console *features*)
  ;; CL-PLUS-SSL requires sb-bsd-socket be loaded (it uses an internal package
  ;; loaded by that contrib), but it doesn't declare a dependency on it.
  ;; (require :sb-bsd-sockets)
  ;; By default, openssl is ~useless on Windows. Don't build it until we can
  ;; figure out how to make it use the OS' certificate store.
  (pushnew :drakma-no-ssl *features*))

(asdf:make :clpm)
