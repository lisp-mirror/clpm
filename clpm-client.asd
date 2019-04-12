;;;; CLPM Client System Definition
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

#-:asdf3.2
(error "Requires ASDF >=3.2")

(defsystem #:clpm-client
  :version (:read-file-form "src/clpm-client/header.lisp" :at (1 2))
  :description "A client for CLPM"
  :license "BSD-2-Clause"
  :pathname "src/clpm-client/"
  :class :package-inferred-system
  :defsystem-depends-on (:clpm-asdf)
  :build-operation "clpm-asdf:concatenate-source-deliver-asd-tarball-op"
  :depends-on (#:clpm-client/clpm-client))

(defmethod asdf::component-build-pathname ((c (eql (find-system "clpm-client"))))
  (concatenate 'string "../../build/clpm-client-" (component-version c) "/clpm-client"))
