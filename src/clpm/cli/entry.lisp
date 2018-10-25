(uiop:define-package #:clpm/cli/entry
    (:use #:cl
          #:alexandria
          #:clpm/log
          #:clpm/version)
  (:import-from #:net.didierverna.clon
                #:defsynopsis
                #:make-context
                #:getopt
                #:remainder
                #:help
                #:defgroup)
  (:export #:*common-arguments*
           #:define-cli-entry
           #:main
           #:process-common-arguments
           #:register-command))

(in-package #:clpm/cli/entry)

(setup-logger)

(defparameter *common-arguments*
  (defgroup (:header "Common Options")
    (flag
     :long-name "help"
     :description "Print help and exit")
    (flag
     :long-name "version"
     :description "Print the version and exit")
    (flag
     :long-name "verbose"
     :short-name "V"
     :description "Make the output more verbose")))

(defparameter *synopsis*
  (defsynopsis (:postfix "[command] [arguments...]"
                :make-default nil)
    *common-arguments*))

(defvar *commands* nil)

(defun register-command (name function)
  (setf (assoc-value *commands* name :test 'equal) function))

(defun dispatch-command (args)
  (let* ((name (find-if (lambda (x)
                          (not (eql #\- (aref x 0))))
                        args))
         (args (remove name args)))
    (let ((function (assoc-value *commands* name :test 'equal)))
      (cond
        (function
         (funcall function args))
        ((member "--version" args :test #'string=)
         (print-version)
         (uiop:quit))
        (t
         (print-help)
         (uiop:quit))))))

(defun print-version ()
  (format *standard-output* "CLPM version ~A~%" (clpm-version))
  (format *standard-output* "~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
  (format *standard-output* "ASDF ~A~%" (asdf:asdf-version))
  (with-standard-io-syntax
    (let ((*print-pretty* t))
      (format *standard-output* "Features:~%~S~%" *features*))))

(defun print-help ()
  (format *standard-output* "CLPM - Lisp Package Manager~%")
  (when net.didierverna.clon:*context*
    (help))
  (format *standard-output*
          "available commands: ~{~A~^ ~}~%"
	      (mapcar #'car *commands*)))

(defun count-flag-occurances (&rest options)
  (loop
    :while (apply #'getopt options)
    :summing 1))

(defun process-common-arguments ()
  (when (getopt :long-name "help")
    (print-help)
    (uiop:quit))
  (when (getopt :long-name "version")
    (print-version)
    (uiop:quit))
  (case (count-flag-occurances :short-name "V")
    (0)
    (1
     (log:config '(clpm) :info))
    (2
     (log:config '(clpm) :debug)
     (log:debug "Setting log level to debug"))
    (t
     (log:config '(clpm) :trace)
     (log:debug "Setting log level to trace"))))

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
         (process-common-arguments)
         (unless (progn ,@body)
           (uiop:quit 1)))
       (register-command ,cli-name ',fun-name))))

(defun main ()
  (dispatch-command (uiop:command-line-arguments)))
