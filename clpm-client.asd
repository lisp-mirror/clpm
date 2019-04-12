;;;; CLPM Client System Definition
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

#-:asdf3.2
(error "Requires ASDF >=3.2")

(defsystem #:clpm-client
  :version "0.0.1"
  :description "A client for CLPM"
  :license "BSD-2-Clause"
  :pathname "src/clpm-client/"
  :class :package-inferred-system
  :defsystem-depends-on (:clpm-asdf)
  :build-operation "clpm-asdf:concatenate-source-deliver-asd-op"
  :build-pathname "../../build/client/clpm-client"
  :depends-on (#:clpm-client/clpm-client))
