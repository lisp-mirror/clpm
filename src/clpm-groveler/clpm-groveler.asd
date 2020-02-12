;;;; CLPM Grovler System Definition
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(defsystem #:clpm-groveler
  :description "A package for CLPM to determine dependencies"
  :license "BSD-2-Clause"
  :version (:read-file-form "version.sexp")
  :components ((:file "main")))
