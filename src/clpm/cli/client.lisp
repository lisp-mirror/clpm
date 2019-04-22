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
    (text :contents "Install the client library")
    *common-arguments*))

(define-cli-entry client (*synopsis*)
  (ensure-client-written)
  (format *standard-output* "Client library located at:~%~A~%" (clpm-client-lib-location))

  (let* ((contents (clpm-client-source-registry-conf.d-file-contents))
         (pathname (clpm-client-source-registry-conf.d-file-name))
         (write-p (yes-or-no-p "Would you like me to configure ASDF to find the client automatically?~%~%I will write the following to ~A:~%~%~A~%~%" pathname contents)))
    (when write-p
      (ensure-directories-exist pathname)
      (with-open-file (s pathname :direction :output
                                  :if-exists :supersede)
        (write-string contents s))))
  t)
