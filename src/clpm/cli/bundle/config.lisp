;;;; clpm bundle config
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/config
    (:use #:cl
          #:clpm/cli/bundle/common
          #:clpm/cli/common-args
          #:clpm/cli/config/common
          #:clpm/cli/subcommands
          #:clpm/config
          #:clpm/log)
  (:import-from #:adopt))

(in-package #:clpm/cli/bundle/config)

(setup-logger)

(defparameter *bundle-config-ui*
  (adopt:make-interface
   :name "clpm bundle config"
   :summary "Common Lisp Package Manager Bundle Config"
   :usage "bundle config [options]"
   :help "Print the current configuration of the bundle"
   :contents (list *group-common*
                   *group-bundle*)))

(define-cli-command (("bundle" "config") *bundle-config-ui*) (args options)
  (format *standard-output* "Current configuration:~%~A~%---~%"
          (with-output-to-string (s) (print-config s)))
  t)
