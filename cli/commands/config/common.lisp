;;;; Config CLI common functions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/config/common
    (:use #:cl
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt))

(in-package #:clpm-cli/commands/config/common)

(defparameter *default-ui*
  (adopt:make-interface
   :name "clpm config"
   :summary "Common Lisp Package Manager"
   :usage "config [options] subcommand"
   :help "Common Lisp Package Manager"
   :contents (list *group-common*)))

(define-cli-command-folder (("config") *default-ui*))
