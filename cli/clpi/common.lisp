;;;; CLPI CLI common functions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/clpi/common
    (:use #:cl
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt))

(in-package #:clpm-cli/clpi/common)

(defparameter *default-ui*
  (adopt:make-interface
   :name "clpm clpi"
   :summary "CLPM CLPI commands"
   :usage "clpi [options] subcommand"
   :help "Common Lisp Package Manager"
   :contents (list *group-common*)))

(define-cli-command-folder (("clpi") *default-ui*))
