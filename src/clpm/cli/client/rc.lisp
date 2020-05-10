;;;; clpm config info
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/client/rc
    (:use #:cl
          #:clpm/cli/common-args
          #:clpm/cli/client/common
          #:clpm/cli/interface-defs
          #:clpm/client
          #:named-readtables)
  (:import-from #:uiop
                #:*stdout*)
  (:import-from #:cl-interpol))

(in-package #:clpm/cli/client/rc)

(in-readtable :interpol-syntax)

(defparameter *option-quicklisp-alternative*
  (adopt:make-option
   :quicklisp-alternative
   :long "quicklisp-alternative"
   :help "Print configuration as a Quicklisp alternative."
   :reduce (constantly t)))

(defparameter *client-rc-ui*
  (adopt:make-interface
   :name "clpm client rc"
   :summary "Common Lisp Package Manager"
   :usage "client rc [options]"
   :help "Common Lisp Package Manager"
   :contents (list *group-common*
                   *option-quicklisp-alternative*)))

(defun quicklisp-alternative-rc ()
  #?";;; Use CLPM as a quicklisp alternative (missing systems are silently installed
;;; on demand).
(require \"asdf\")
#-clpm-client
(progn
  (asdf:load-asd #p\"${(client-asd-pathname)}\")
  (asdf:load-system \"clpm-client\")
  (setf (symbol-value (uiop:find-symbol* :*clpm-system-not-found-behavior* :clpm-client)) :install-with-deps)
  (uiop:symbol-call :clpm-client :activate-clpm-asdf-integration))")

(defun default-rc ()
  #?";;; Load clpm-client with default values by calling cl-user::load-clpm-client.
(require \"asdf\")
(defun load-clpm-client ()
  (asdf:load-asd #p\"${(client-asd-pathname)}\")
  (asdf:load-system \"clpm-client\")
  (uiop:symbol-call :clpm-client :activate-clpm-asdf-integration))")

(define-cli-command (("client" "rc") *client-rc-ui*) (args options)
  (declare (ignore args))
  (uiop:with-safe-io-syntax ()
    (let ((*print-case* :downcase))
      (if (gethash :quicklisp-alternative options)
          (write-string (quicklisp-alternative-rc) *stdout*)
          (write-string (default-rc) *stdout*))
      (terpri *stdout*)))
  t)
