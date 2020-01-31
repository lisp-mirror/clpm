;;;; CLI entry point
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/entry
    (:use #:cl
          #:alexandria
          #:clpm/cli/common-args
          #:clpm/cli/subcommands
          #:clpm/log)
  (:import-from #:adopt)
  (:export #:*common-arguments*
           #:define-cli-entry
           #:main
           #:process-common-arguments
           #:register-command))

(in-package #:clpm/cli/entry)

(setup-logger)

(defparameter *default-ui*
  (adopt:make-interface
   :name "clpm"
   :summary "Common Lisp Package Manager"
   :usage "[options] subcommand"
   :help "Common Lisp Package Manager"
   :contents (list *group-common*)))

(defun main ()
  (handler-case
      (handler-bind
          ((adopt:unrecognized-option #'adopt:discard-option)
           (error (lambda (c) (uiop:print-condition-backtrace c))))
        (multiple-value-bind (arguments options)
            (adopt:parse-options *default-ui*)
          ;; Compute verbosity
          (case (gethash :verbose options)
            (0
             (log:config '(clpm) :warn
                         :this :stream *error-output* :immediate-flush :own
                         :pattern "%;<;;>;5p [%g{}{}{:downcase}] - %m%n"))
            (1
             (log:config '(clpm) :info
                         :this :stream *error-output* :immediate-flush :own
                         :pattern "%;<;;>;5p [%g{}{}{:downcase}] - %m%n")
             (log:debug "Setting CLPM log level to info"))
            (2
             (log:config '(clpm) :debug
                         :this :stream *error-output* :immediate-flush :own
                         :pattern "%;<;;>;5p [%g{}{}{:downcase}] - %m%n")
             (log:debug "Setting CLPM log level to debug"))
            (t
             (log:config '(clpm) :trace
                         :this :stream *error-output* :immediate-flush :own
                         :pattern "%;<;;>;5p [%g{}{}{:downcase}] - %m%n")
             (log:debug "Setting CLPM log level to trace")))
          (dispatch-subcommand *commands* arguments options *default-ui*)))
    (error (c)
      (adopt:print-error-and-exit c))))
