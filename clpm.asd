;;;; CLPM System Definition

#-:asdf3.1
(error "Requires ASDF >=3.1")

(defsystem #:clpm
  :version (:read-file-form "src/clpm/version.lisp" :at (2 2))
  :description "A Common Lisp package Manager"
  :license "BSD-2-Clause"
  :pathname "src/clpm/"
  :class :package-inferred-system
  :entry-point "clpm/cli/entry:main"
  :depends-on (#:clpm/clpm))
