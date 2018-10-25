(uiop:define-package #:clpm/cli/config/info
    (:use #:cl
          #:clpm/cli/config/common
          #:clpm/cli/entry
          #:clpm/config
          #:clpm/log)
  (:import-from #:uiop
                #:*stdout*)
  (:import-from #:net.didierverna.clon
                #:defsynopsis
                #:make-context
                #:getopt
                #:remainder
                #:help))

(in-package #:clpm/cli/config/info)

(setup-logger)

(defparameter *synopsis*
  (defsynopsis (:make-default nil)
    *common-arguments*))

(define-config-entry info (*synopsis*)
  ;; Unpack the command line arguments.
  (log:info "Config")
  (format *stdout* "Config directories: ~A~%~%" *clpm-config-directories*)
  (format *stdout* "Current configuration:~%~A~%---~%" (with-output-to-string (s) (print-config s)))
  t)
