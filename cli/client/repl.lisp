;;;; clpm client repl
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/client/repl
    (:use #:cl
          #:clpm-cli/client/common
          #:clpm-cli/interface-defs)
  (:import-from #:uiop
                #:*stdout*))

(in-package #:clpm-cli/client/repl)

(defparameter *client-repl-ui*
  (adopt:make-interface
   :name "clpm client repl"
   :summary "Common Lisp Package Manager"
   :usage "client repl [options]"
   :help "Starts a REPL for interacting with CLPM. Intended to be used by clpm-client library, interface is not guaranteed to be stable yet."))

(define-cli-command (("client" "repl") *client-repl-ui*) (args options)
  (declare (ignore args options))
  (with-standard-io-syntax
    (let ((*package* (find-package :clpm)))
      (loop
        (print (eval (read)) *stdout*)
        (terpri *stdout*)
        (finish-output *stdout*))))
  t)
