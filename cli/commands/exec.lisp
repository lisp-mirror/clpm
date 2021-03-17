;;;; clpm exec
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/exec
    (:use #:cl
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt)
  (:import-from #:clpm))

(in-package #:clpm-cli/commands/exec)

(defparameter *option-with-client*
  (adopt:make-option
   :exec-with-client
   :long "with-client"
   :help "Include the CLPM client in the source registry"
   :reduce (constantly t)))

(defparameter *exec-ui*
  (adopt:make-interface
   :name "clpm exec"
   :summary "Common Lisp Project Manager Exec"
   :usage "exec [options] [command]"
   :help "Execute a command with environment variables configured to use a context"
   :contents (list *group-common*
                   *option-context*
                   *option-with-client*)))

(define-cli-command (("exec") *exec-ui*) (args options)
  (clpm:exec (first args) (rest args) :with-client-p (gethash :exec-with-client options)))
