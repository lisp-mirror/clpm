;;;; CLPM-ASDF System Definition
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(load-system :asdf-release-ops)

(defsystem #:clpm-asdf
  :version (:read-file-form "version.lisp-expr")
  :description "ASDF bootstrapping for CLPM"
  :license "BSD-2-Clause"
  :pathname "clpm-asdf/"
  :depends-on (#:asdf-release-ops
               #:cl-semver
               #:trivial-features)
  :components ((:file "package")
               (:file "features" :depends-on ("package"))
               (:file "version" :depends-on ("package"))
               (:file "system" :depends-on ("package" "features" "version"))
               (:file "dependencies-license-op" :depends-on ("package" "system"))
               (:file "client" :depends-on ("package" "system"))))
