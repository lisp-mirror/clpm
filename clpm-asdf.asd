;;;; CLPM ASDF Extensions System Definition
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

#-:asdf3.1
(error "Requires ASDF >=3.1")

(defsystem #:clpm-asdf
  :version "0.0.1"
  :description "ASDF Extensions for CLPM"
  :license "BSD-2-Clause"
  :pathname "src/clpm-asdf/"
  :class :package-inferred-system
  :depends-on (#:clpm-asdf/clpm-asdf)
  ;; :components
  ;; ((:file "clpm-client"))
  )
