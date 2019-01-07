;;;; clpm sync
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/sync
    (:use #:cl
          #:clpm/cli/entry
          #:clpm/config
          #:clpm/log
          #:clpm/source)
  (:import-from #:uiop
                #:*stdout*)
  (:import-from #:net.didierverna.clon
                #:defsynopsis
                #:make-context
                #:getopt
                #:remainder
                #:help))

(in-package #:clpm/cli/sync)

(setup-logger)

(defparameter *synopsis*
  (defsynopsis (:make-default nil)
    *common-arguments*))

(define-cli-entry sync (*synopsis*)
  ;; Unpack the command line arguments.
  (log:info "Sync")
  (mapc #'sync-source (load-sources))
  t)
