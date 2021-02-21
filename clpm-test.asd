;;;; CLPM Test System Definition
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

#-:asdf3.2
(error "Requires ASDF >=3.2")

(defsystem #:clpm-test
  :version (:read-file-form "version.lisp-expr")
  :description "Tests for CLPM"
  :license "BSD-2-Clause"
  :pathname "test/"
  :depends-on (#:fiveam #:hunchentoot)
  :serial t
  :components
  ((:file "package")
   (:file "suite")
   (:file "quicklisp-bundle")
   (:file "tests")))
