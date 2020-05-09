;;;; CLI entry point
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/entry
    (:use #:cl
          #:alexandria
          #:clpm/cli/common-args
          #:clpm/cli/subcommands
          #:clpm/config
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

(defun clpm-debugger (c v)
  (declare (ignore v))
  (format *error-output* "Uncaught error. You should not have ended up here, ~
please report a bug and provide the stack trace.~%")
  (uiop:print-condition-backtrace c)
  (uiop:quit 1))

(defun main ()
  (let ((*debugger-hook* #'clpm-debugger))
    (handler-bind
        (;; Gracefully catch ctrl-c
         (#+sbcl sb-sys:interactive-interrupt
          #+ccl  ccl:interrupt-signal-condition
          #+clisp system::simple-interrupt-condition
          #+ecl ext:interactive-interrupt
          #+allegro excl:interrupt-signal
          (lambda (c)
            (when (or (eql (config-value :log :level) :debug)
                      (eql (config-value :log :level) :trace))
              (uiop:print-condition-backtrace c :stream *error-output*))
            (uiop:quit 2)))
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
                  (uiop:quit 1))))
      (multiple-value-bind (arguments options)
          ;; We need to ignore unrecognized options here since we don't yet
          ;; know the correct set of options to use!
          (parse-options-ignoring-unrecognized *default-ui*)
        (dispatch-subcommand *commands* arguments options *default-ui*)))))
