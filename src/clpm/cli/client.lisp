(uiop:define-package #:clpm/cli/client
    (:use #:cl
          #:alexandria
          #:clpm/cli/common-args
          #:clpm/cli/subcommands
          #:clpm/client
          #:clpm/log)
  (:import-from #:adopt))

(in-package #:clpm/cli/client)

(setup-logger)

(defparameter *client-ui*
  (adopt:make-interface
   :name "clpm client"
   :summary "Common Lisp Package Manager Client"
   :usage "client [options]"
   :help "Install the CLPM client"
   :contents (list *group-common*)))

(define-cli-command (("client") *client-ui*) (args options)
  (ensure-client-written)
  (format *standard-output* "Client library located at:~%~A~%" (clpm-client-lib-location))

  (let* ((contents (clpm-client-source-registry-conf.d-file-contents))
         (pathname (clpm-client-source-registry-conf.d-file-name))
         (write-p (yes-or-no-p "Would you like me to configure ASDF to find the client automatically?~%~%I will write the following to ~A:~%~%~A~%~%" pathname contents)))
    (when write-p
      (ensure-directories-exist pathname)
      (with-open-file (s pathname :direction :output
                                  :if-exists :supersede)
        (write-string contents s)))))
