;;;; clpm client rc
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/client/rc
    (:use #:cl
          #:clpm-cli/common-args
          #:clpm-cli/commands/client/common
          #:clpm-cli/interface-defs
          #:named-readtables)
  (:import-from #:uiop
                #:*stdout*)
  (:import-from #:cl-interpol)
  (:import-from #:clpm))

(in-package #:clpm-cli/commands/client/rc)

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
  (when (probe-file #p\"${(clpm:client-asd-pathname)}\")
    (asdf:load-asd #p\"${(clpm:client-asd-pathname)}\")
    (asdf:load-system \"clpm-client\")
    (setf (symbol-value (uiop:find-symbol* :*asdf-system-not-found-behavior* :clpm-client)) :install)
    (if (uiop:symbol-call :clpm-client :active-context)
        (uiop:symbol-call :clpm-client :activate-asdf-integration)
        (uiop:symbol-call :clpm-client :activate-context \"default\" :activate-asdf-integration t))))")

(defun default-rc ()
  #?";;; Use CLPM with default configuration to install systems on demand.

(require \"asdf\")

#-clpm-client
(progn
  (when (probe-file #p\"${(clpm:client-asd-pathname)}\")
    (asdf:load-asd #p\"${(clpm:client-asd-pathname)}\")
    (asdf:load-system \"clpm-client\")
    (if (uiop:symbol-call :clpm-client :active-context)
        (uiop:symbol-call :clpm-client :activate-asdf-integration)
        (uiop:symbol-call :clpm-client :activate-context \"default\" :activate-asdf-integration t))))")

(define-cli-command (("client" "rc") *client-rc-ui*) (args options)
  (declare (ignore args))
  (uiop:with-safe-io-syntax ()
    (let ((*print-case* :downcase))
      (if (gethash :quicklisp-alternative options)
          (write-string (quicklisp-alternative-rc) *stdout*)
          (write-string (default-rc) *stdout*))
      (terpri *stdout*)))
  t)
