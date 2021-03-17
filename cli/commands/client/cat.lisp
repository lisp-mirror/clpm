;;;; clpm client cat
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/client/cat
    (:use #:cl
          #:clpm-cli/common-args
          #:clpm-cli/commands/client/common
          #:clpm-cli/interface-defs)
  (:import-from #:uiop
                #:*stdout*)
  (:import-from #:clpm/client
                #:*clpm-client-concatenated-source*))

(in-package #:clpm-cli/commands/client/cat)

(defparameter *client-cat-ui*
  (adopt:make-interface
   :name "clpm client cat"
   :summary "Common Lisp Project Manager"
   :usage "client cat"
   :help "Print the CLPM client source code to stdout"
   :contents (list *group-common*)))

(define-cli-command (("client" "cat") *client-cat-ui*) (args options)
  (declare (ignore args options))
  (uiop:with-safe-io-syntax ()
    (let ((*print-case* :downcase))
      (write-string *clpm-client-concatenated-source* *stdout*)
      (terpri *stdout*)))
  t)
