;;;; Client CLI common functions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/client/common
    (:use #:cl
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt))

(in-package #:clpm-cli/commands/client/common)

(defparameter *default-ui*
  (adopt:make-interface
   :name "clpm client"
   :summary "Common Lisp Package Manager"
   :usage "client [options] subcommand"
   :help "Common Lisp Package Manager"
   :contents (list *group-common*)))

(define-cli-command-folder (("client") *default-ui*))
