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

(defun parse-options-ignoring-unrecognized (ui)
  (handler-bind ((adopt:unrecognized-option #'adopt:discard-option))
    (adopt:parse-options ui)))

(defun main ()
  (handler-bind
      (;; Gracefully catch ctrl-c
       (#+sbcl sb-sys:interactive-interrupt
        #+ccl  ccl:interrupt-signal-condition
        #+clisp system::simple-interrupt-condition
        #+ecl ext:interactive-interrupt
        #+allegro excl:interrupt-signal
        (lambda (c) (declare (ignore c)) (uiop:quit 2)))
       ;; On warning, print it and optionally the backtrace.
       (warning (lambda (c)
                  (log:warn "~A" c)
                  (log:debug "~A" (with-output-to-string (s)
                                    (uiop:print-condition-backtrace c :stream s)))
                  ;; Invoke muffle warning so that we don't get a duplicate
                  ;; warning printed.
                  (muffle-warning c)))
       ;; On error, print the backtrace and quit.
       (error (lambda (c)
                (uiop:print-condition-backtrace c)
                (format *error-output* "~&~A~%" c)
                (uiop:quit 1))))
    (multiple-value-bind (arguments options)
        ;; We need to ignore unrecognized options here since we don't yet
        ;; know the correct set of options to use!
        (parse-options-ignoring-unrecognized *default-ui*)
      ;; Compute verbosity
      (case (gethash :verbose options)
        (0
         (log:config '(clpm) :warn
                     :this :stream *error-output* :immediate-flush :own
                     :pattern "%;<;;>;5p [%g{}{}{:downcase}] - %<{pretty}%m%>%n"))
        (1
         (log:config '(clpm) :info
                     :this :stream *error-output* :immediate-flush :own
                     :pattern "%;<;;>;5p [%g{}{}{:downcase}] - %<{pretty}%m%>%n")
         (log:debug "Setting CLPM log level to info"))
        (2
         (log:config '(clpm) :debug
                     :this :stream *error-output* :immediate-flush :own
                     :pattern "%;<;;>;5p [%g{}{}{:downcase}] - %<{pretty}%m%>%n")
         (log:debug "Setting CLPM log level to debug"))
        (t
         (log:config '(clpm) :trace
                     :this :stream *error-output* :immediate-flush :own
                     :pattern "%;<;;>;5p [%g{}{}{:downcase}] - %<{pretty}%m%>%n")
         (log:debug "Setting CLPM log level to trace")))
      (dispatch-subcommand *commands* arguments options *default-ui*))))
