;;;; clpm sync
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/sync
    (:use #:cl
          #:clpm/cli/common-args
          #:clpm/cli/defs
          #:clpm/cli/subcommands
          #:clpm/config
          #:clpm/log
          #:clpm/source)
  (:import-from #:adopt)
  (:import-from #:uiop
                #:*stdout*))

(in-package #:clpm/cli/sync)

(setup-logger)

(define-string *help-text*
  "Sync all sources.")

(defparameter *sync-ui*
  (adopt:make-interface
   :name "clpm-sync"
   :summary "Common Lisp Package Manager Sync"
   :usage "sync"
   :help *help-text*
   :manual *help-text*
   :contents (list *group-common*)))

(define-cli-command (("sync") *sync-ui*) (args options)
  (log:info "Sync")
  (mapc #'sync-source (sources))
  t)
