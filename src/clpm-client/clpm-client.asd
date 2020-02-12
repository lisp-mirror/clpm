;;;; CLPM Client System Definition
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

#-:asdf3.2
(error "Requires ASDF >=3.2")

(defsystem #:clpm-client
  :version (:read-file-form "header.lisp" :at (1 2))
  :description "A client for CLPM"
  :license "BSD-2-Clause"
  :class :package-inferred-system
  :depends-on (#:clpm-client/clpm-client))
