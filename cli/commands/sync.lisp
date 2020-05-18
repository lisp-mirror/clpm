;;;; clpm sync
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/sync
    (:use #:cl
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt)
  (:import-from #:clpm))

(in-package #:clpm-cli/commands/sync)

(define-string *help-text*
  "Sync all sources.")

(defparameter *sync-ui*
  (adopt:make-interface
   :name "clpm-sync"
   :summary "Common Lisp Package Manager Sync"
   :usage "sync [SOURCE-NAME*]"
   :help *help-text*
   :manual *help-text*
   :contents (list *group-common*)))

(define-cli-command (("sync") *sync-ui*) (args options)
  (declare (ignore options))
  (clpm:sync :sources args)
  t)
