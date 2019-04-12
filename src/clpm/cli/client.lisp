(uiop:define-package #:clpm/cli/client
    (:use #:cl
          #:alexandria
          #:clpm/cli/entry
          #:clpm/client
          #:clpm/log)
  (:import-from #:net.didierverna.clon
                #:defsynopsis
                #:make-context
                #:getopt
                #:remainder
                #:help))

(in-package #:clpm/cli/client)

(setup-logger)

(defparameter *synopsis*
  (defsynopsis (:make-default nil)
    (text :contents "Print the client library")
    *common-arguments*))

(define-cli-entry client (*synopsis*)
  (ensure-client-written)
  (format *standard-output* "Client located at:~%~A~%" (clpm-client-lib-location))
  t)
