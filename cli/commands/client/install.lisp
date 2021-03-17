;;;; clpm client install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/client/install
    (:use #:cl
          #:clpm-cli/common-args
          #:clpm-cli/commands/client/common
          #:clpm-cli/interface-defs)
  (:import-from #:uiop
                #:*stdout*)
  (:import-from #:clpm))

(in-package #:clpm-cli/commands/client/install)

(define-string *help-text*
  "Install the CLPM client to your user data directory. If you use the client
installed this way you will want to update your ASDF config every time you
upgrade CLPM.")

(defparameter *client-install-ui*
  (adopt:make-interface
   :name "clpm client install"
   :summary "Common Lisp Project Manager"
   :usage "client install [options]"
   :help *help-text*
   :contents (list *group-common*)))

(define-cli-command (("client" "install") *client-install-ui*) (args options)
  (declare (ignore args options))
  (let ((pathname (clpm:client-user-data-pathname)))
    (clpm:install-client-to-user-data)
    (format t "CLPM Client installed to ~A~%" (uiop:native-namestring
                                               (uiop:pathname-directory-pathname pathname)))
    t))
