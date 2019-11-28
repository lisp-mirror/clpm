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

;; (defvar *commands* nil)

;; (defun register-command (name function)
;;   (setf (assoc-value *commands* name :test 'equal) function))

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

;; (defun print-help ()
;;   (format *standard-output* "CLPM - Lisp Package Manager~%")
;;   (when net.didierverna.clon:*context*
;;     (help))
;;   (format *standard-output*
;;           "available commands: ~{~A~^ ~}~%"
;; 	      (mapcar #'car *commands*)))

(defun count-flag-occurances (&rest options)
  (loop
    :while (apply #'getopt options)
    :summing 1))

(defmacro define-cli-entry (name (synopsis) &body body)
  (let ((fun-name (intern
                   (concatenate 'string
                                (uiop:standard-case-symbol-name :cli-)
                                (uiop:standard-case-symbol-name name))))
        (cli-name (string-downcase (symbol-name name)))
        (args (gensym)))
    `(progn
       (defun ,fun-name (,args)
         (make-context :cmdline (list* (concatenate 'string (first (uiop:raw-command-line-arguments))
                                                    " "
                                                    ,cli-name)
                                       ,args)
                       :synopsis ,synopsis)
         ;;(process-common-arguments)
         (unless (progn ,@body)
           (uiop:quit 1)))
       ;;(register-command ,cli-name ',fun-name)
       )))

;; (defun dispatch-subcommand (arguments options ui)
;;   (let* ((subcommand-name (first arguments))
;;          (function (assoc-value *commands* subcommand-name :test 'equal)))
;;     (format t "~S~%" function)
;;     (cond
;;       (function
;;        (funcall function))
;;       ((gethash :help options)
;;        (adopt:print-help-and-exit ui :exit-code 0))
;;       (t
;;        (adopt:print-help-and-exit ui :exit-code 1)))))

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
