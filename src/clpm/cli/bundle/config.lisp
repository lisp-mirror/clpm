(uiop:define-package #:clpm/cli/bundle/config
    (:use #:cl
          #:clpm/cli/bundle/common
          #:clpm/cli/entry
          #:clpm/config
          #:clpm/log)
  (:import-from #:net.didierverna.clon
                #:defsynopsis
                #:make-context
                #:getopt
                #:remainder
                #:help)
  (:export #:cli-bundle-config))

(in-package #:clpm/cli/bundle/config)

(setup-logger)

(defparameter *synopsis*
  (defsynopsis (:make-default nil)
    (text :contents "bundle config")
    *bundle-arguments*
    *common-arguments*))

(define-bundle-entry config (*synopsis*)
  (format *standard-output* "Current configuration:~%~A~%---~%" (with-output-to-string (s) (print-config s)))
  t)
