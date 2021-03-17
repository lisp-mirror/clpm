;;;; Common bundle CLI functions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/bundle/common
    (:use #:cl
          #:alexandria
          #:clpm/bundle
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs
          #:clpm/config
          #:clpm/utils)
  (:import-from #:adopt)
  (:import-from #:cl-ppcre)
  (:export #:*group-bundle*
           #:*option-no-resolve*))

(in-package #:clpm-cli/commands/bundle/common)

(defparameter *option-file*
  (adopt:make-option
   :cli-config-bundle-clpmfile
   :short #\f
   :parameter "FILE"
   :help "The path to the clpmfile"
   :initial-value :missing
   :reduce #'adopt:last))

(defparameter *option-no-resolve*
  (adopt:make-option
   :bundle-no-resolve
   :long "no-resolve"
   :help "Do not attempt to resolve any requirements. Use requirements exactly as given in lock file."
   :reduce (constantly t)))

(defparameter *group-bundle*
  (adopt:make-group
   :bundle
   :title "Bundle options"
   :help "Common options for bundle commands"
   :options (list *option-file*)))

(defparameter *default-ui*
  (adopt:make-interface
   :name "clpm bundle"
   :summary "Common Lisp Project Manager Bundle"
   :usage "bundle [options] subcommand"
   :help "Bundle commands"
   :contents (list *group-common*
                   *group-bundle*)))

(define-cli-command-folder (("bundle") *default-ui*))
