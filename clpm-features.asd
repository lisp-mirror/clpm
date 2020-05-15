;;;; CLPM-Features System Definition
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

#-:asdf3.2
(error "Requires ASDF >=3.2")

(in-package :asdf-user)

(defsystem #:clpm-features
  :version (:read-file-form "clpm/version.lisp" :at (2 2))
  :description "Configuring *FEATURES* for CLPM"
  :license "BSD-2-Clause"
  :pathname "features/"
  :class :package-inferred-system
  :depends-on (#:clpm-features/clpm-features))
