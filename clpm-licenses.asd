;;;; CLPM-Licenses System Definition

(defsystem #:clpm-licenses
  :description "System containing the licenses for CLPM and its dependencies."
  :license "BSD-2-Clause"
  :serial t
  :components ((:module "licenses"
                :components ((:static-file "alexandria")
                             (:static-file "anaphora")
                             (:static-file "archive")
                             (:static-file "asdf")
                             (:static-file "babel")
                             (:static-file "bordeaux-threads")
                             (:static-file "cffi")
                             (:static-file "chipz")
                             (:static-file "chunga")
                             (:static-file "cl-annot")
                             (:static-file "cl-base64")
                             (:static-file "cl-dbi")
                             (:static-file "cl-fad")
                             (:static-file "cl-plus-ssl")
                             (:static-file "cl-ppcre")
                             (:static-file "cl-reexport")
                             (:static-file "cl-sqlite")
                             (:static-file "cl-utilities")
                             (:static-file "clon")
                             (:static-file "closer-mop")
                             (:static-file "dissect")
                             (:static-file "drakma")
                             (:static-file "exit-hooks")
                             (:static-file "fast-http")
                             (:static-file "flexi-streams")
                             (:static-file "ironclad")
                             (:static-file "iterate")
                             (:static-file "log4cl")
                             (:static-file "log4cl.NOTICE")
                             (:static-file "mito")
                             (:static-file "named-readtables")
                             (:static-file "nibbles")
                             (:static-file "openssl")
                             (:static-file "optima")
                             (:static-file "proc-parse")
                             (:static-file "puri")
                             (:static-file "salza2")
                             (:static-file "sbcl")
                             (:static-file "smart-buffer")
                             (:static-file "split-sequence")
                             (:static-file "sxql")
                             (:static-file "trivial-features")
                             (:static-file "trivial-garbage")
                             (:static-file "trivial-gray-streams")
                             (:static-file "trivial-utf-8")
                             (:static-file "unix-opts")
                             (:static-file "usocket")
                             (:static-file "uuid")
                             (:static-file "xsubseq")))
               (:file "src/clpm-licenses/main")))
