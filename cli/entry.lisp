;;;; CLI entry point
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/entry
    (:use #:cl
          #:alexandria
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs
          #:clpm/config
          #:clpm/log
          #:clpm/session)
  (:import-from #:adopt)
  (:export #:*common-arguments*
           #:define-cli-entry
           #:main
           #:process-common-arguments
           #:register-command))

(in-package #:clpm-cli/entry)

(setup-logger)

(defparameter *default-ui*
  (adopt:make-interface
   :name "clpm"
   :summary "Common Lisp Package Manager"
   :usage "[options] subcommand"
   :help "Common Lisp Package Manager"
   :contents (list *group-common*)))

(defun clpm-debugger (c v)
  (declare (ignore v))
  (format *error-output* "Uncaught error. You should not have ended up here, ~
please report a bug and provide the stack trace.~%")
  (uiop:print-condition-backtrace c)
  (uiop:quit 1))

(defun set-log-level ()
  (log:config '(clpm) (config-value :log :level)
              :this
              :stream *error-output*
              :immediate-flush :own
              :pattern "%;<;;>;5p [%g{}{}{:downcase}] - %<{pretty}%m%>%n")
  (log:debug "Log level set to ~S" (config-value :log :level)))

(define-cli-command-folder (() *default-ui*) (thunk ui args options)
  (declare (ignore args))
  ;; Print the help if requested.
  (when (gethash :help options)
    (adopt:print-help-and-exit ui))
  ;; Immediately start a CLPM session.
  (with-clpm-session ()
    ;; Augment the config with options from the CLI.
    (config-add-cli-source! options)
    ;; Set the log level.
    (set-log-level)
    ;; Give ourselves a reasonable default, since the spec doesn't *require*
    ;; this...
    (let ((*default-pathname-defaults* (uiop:pathname-directory-pathname (uiop:getcwd))))
      (funcall thunk))))


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
      (dispatch-command nil))))
