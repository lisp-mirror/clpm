(uiop:define-package #:clpm/cli/config/common
    (:use #:cl
          #:alexandria
          #:clpm/cli/entry)
  (:import-from #:net.didierverna.clon
                #:defsynopsis
                #:defgroup
                #:make-context
                #:getopt
                #:remainder
                #:help)
  (:export #:*config-arguments*
           #:define-config-entry))

(in-package #:clpm/cli/config/common)

(defparameter *config-arguments*
  nil)

(defvar *config-commands* nil)

(defun register-config-command (name function)
  (setf (assoc-value *config-commands* name :test 'equal) function))

(defun dispatch-config-command (args)
  (let* ((name (find-if (lambda (x)
                          (not (eql #\- (aref x 0))))
                        args))
         (args (remove name args)))
    (let ((function (assoc-value *config-commands* name :test 'equal)))
      (cond
        (function
         (funcall function args))
        (t
         (print-config-help)
         (uiop:quit))))))

(defun print-config-help ()
  (format *standard-output* "CLPM - Lisp Package Manager~%")
  (when net.didierverna.clon:*context*
    (help))
  (format *standard-output*
          "available config commands: ~{~A~^ ~}~%"
	      (mapcar #'car *config-commands*)))

(defun cli-config (args)
  (dispatch-config-command args)
  ;; ;; Before making the context, figure out which subcommand to run!
  ;; (make-context :cmdline (list* (concatenate 'string (first (uiop:raw-command-line-arguments))
  ;;                                            " config")
  ;;                               args)
  ;;               :synopsis *synopsis*)
  ;; (process-common-arguments)


  ;; (let ((packages-to-install (remainder))
  ;;       (sources (load-sources)))
  ;;   (dolist (clpm-package-name packages-to-install)
  ;;     (install-requirement sources
  ;;                          (make-instance 'requirement :clpm-package-name clpm-package-name)))
  ;;   t)
  )

(register-command "config" 'cli-config)

(defmacro define-config-entry (name (synopsis) &body body)
  (let ((fun-name (intern
                   (concatenate 'string
                                (uiop:standard-case-symbol-name :config-)
                                (uiop:standard-case-symbol-name name))))
        (config-name (string-downcase (symbol-name name)))
        (args (gensym)))
    `(progn
       (defun ,fun-name (,args)
         (make-context :cmdline (list* (concatenate 'string (first (uiop:raw-command-line-arguments))
                                                    " config "
                                                    ,config-name)
                                       ,args)
                       :synopsis ,synopsis)
         (process-common-arguments)
         (unless (progn ,@body)
           (uiop:quit 1)))
       (register-config-command ,config-name ',fun-name))))
