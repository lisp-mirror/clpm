;;;; CLPM-CLI System Definition
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

#-:asdf3.2
(error "Requires ASDF >=3.2")

(in-package :asdf-user)

(defsystem #:clpm-cli
  :version (:read-file-form "clpm/version.lisp" :at (2 2))
  :description "Command Line Interface for CLPM"
  :license "BSD-2-Clause"
  :pathname "cli/"
  :class :package-inferred-system
  :entry-point "clpm-cli/entry:main"
  :build-operation "program-op"
  :build-pathname "build/clpm"
  :depends-on (#:clpm-cli/clpm-cli))
