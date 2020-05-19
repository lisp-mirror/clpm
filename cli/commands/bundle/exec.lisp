;;;; clpm bundle exec
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/bundle/exec
    (:use #:cl
          #:clpm-cli/commands/bundle/common
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt)
  (:import-from #:clpm))

(in-package #:clpm-cli/commands/bundle/exec)

(defparameter *option-with-client*
  (adopt:make-option
   :bundle-exec-with-client
   :long "with-client"
   :help "Include the CLPM client in the source registry"
   :reduce (constantly t)))

(defparameter *bundle-exec-ui*
  (adopt:make-interface
   :name "clpm bundle exec"
   :summary "Common Lisp Package Manager Bundle Exec"
   :usage "bundle exec [options] [command]"
   :help "Execute a command in the contet of a bundle"
   :contents (list *group-common*
                   *group-bundle*
                   *option-with-client*)))

(define-cli-command (("bundle" "exec") *bundle-exec-ui*) (args options)
  (clpm:bundle-exec (first args) (rest args) :with-client-p (gethash :bundle-exec-with-client options))
  nil)
