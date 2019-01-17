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
                             (:static-file "cl-base64")
                             (:static-file "cl-fad")
                             (:static-file "cl-plus-ssl")
                             (:static-file "cl-ppcre")
                             (:static-file "clon")
                             (:static-file "drakma")
                             (:static-file "exit-hooks")
                             (:static-file "flexi-streams")
                             (:static-file "iterate")
                             (:static-file "log4cl")
                             (:static-file "log4cl.NOTICE")
                             (:static-file "named-readtables")
                             (:static-file "puri")
                             (:static-file "sbcl")
                             (:static-file "split-sequence")
                             (:static-file "trivial-features")
                             (:static-file "trivial-garbage")
                             (:static-file "trivial-gray-streams")
                             (:static-file "unix-opts")
                             (:static-file "usocket")))
               (:file "src/clpm-licenses/main")))
