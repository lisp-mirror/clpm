;;;; Common context CLI functions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/context/common
    (:use #:cl
          #:alexandria
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt)
  (:import-from #:cl-ppcre))

(in-package #:clpm-cli/context/common)

(defparameter *default-ui*
  (adopt:make-interface
   :name "clpm context"
   :summary "Common Lisp Package Manager Context"
   :usage "context [options] subcommand"
   :help "Context commands"
   :contents (list *group-common*)))

(define-cli-command-folder (("context") *default-ui*))
