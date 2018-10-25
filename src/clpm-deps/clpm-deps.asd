;;;; CLPM Deps System Definition

(defsystem #:clpm-deps
  :description "A package for CLPM to determine dependencies"
  :license "BSD-2-Clause"
  :version (:read-file-form "version.sexp")
  :components ((:file "main")))
