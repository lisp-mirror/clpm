;;;; CLPM Client System Definition
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(defsystem #:clpm-client
  :version (:read-file-form "version.lisp" :at (1 2))
  :description "A client for CLPM"
  :license "BSD-2-Clause"
  :depends-on ((:version #:uiop "3.1.7.26"))
  :serial nil
  :components
  ((:file "asdf" :depends-on ("package" "cleanup" "context"))
   (:file "bundle" :depends-on ("package" "proc"))
   (:file "cleanup" :depends-on ("package"))
   (:file "context" :depends-on ("package" "proc" "cleanup"))
   (:file "diff" :depends-on ("package"))
   (:file "env" :depends-on ("package"))
   (:file "install" :depends-on ("package" "proc"))
   (:file "package")
   (:file "proc" :depends-on ("package"))
   (:file "sync" :depends-on ("package" "proc"))
   (:file "ui" :depends-on ("package"))
   (:file "update" :depends-on ("package" "proc"))
   (:file "version" :depends-on ("package" "proc"))))
