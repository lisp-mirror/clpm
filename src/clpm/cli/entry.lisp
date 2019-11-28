;;;; CLI entry point
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/entry
    (:use #:cl
          #:alexandria
          #:clpm/cli/common-args
          #:clpm/cli/subcommands
          #:clpm/log
          #:clpm/version)
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

(defun print-version (options)
  (if (plusp (gethash :verbose options))
      (progn
        (format *standard-output* "CLPM version ~A~%" (clpm-version))
        (format *standard-output* "~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
        (format *standard-output* "ASDF ~A~%" (asdf:asdf-version))
        (format *standard-output* "Software Type: ~A~%" (software-type))
        (format *standard-output* "Software Version: ~A~%" (software-version))
        (with-standard-io-syntax
          (let ((*print-pretty* t))
            (format *standard-output* "Features:~%~S~%" *features*))))
      (format *standard-output* "~A~%" (clpm-version))))

(defun main ()
  (handler-case
      (handler-bind
          ((adopt:unrecognized-option #'adopt:discard-option))
        (multiple-value-bind (arguments options)
            (adopt:parse-options *default-ui*)
          (when (gethash :version-flag options)
            (print-version options)
            (adopt:exit))
          ;; Compute verbosity
          (case (gethash :verbose options)
            (0)
            (1
             (log:config '(clpm) :info)
             (log:debug "Setting CLPM log level to info"))
            (2
             (log:config '(clpm) :debug)
             (log:debug "Setting CLPM log level to debug"))
            (t
             (log:config '(clpm) :trace)
             (log:debug "Setting CLPM log level to trace")))
          (dispatch-subcommand *commands* arguments options *default-ui*)
          ;;(format t "~S~%~S~%" arguments options)
          ))
    (error (c)
      (adopt:print-error-and-exit c))))
