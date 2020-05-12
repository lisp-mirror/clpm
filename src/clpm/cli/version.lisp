;;;; clpm version
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/version
    (:use #:cl
          #:clpm/cli/common-args
          #:clpm/cli/interface-defs
          #:clpm/log
          #:clpm/version)
  (:import-from #:adopt)
  (:import-from #:uiop
                #:*stdout*))

(in-package #:clpm/cli/version)

(setup-logger)

(define-string *help-text*
  "Display CLPM version information. The output is not guaranteed to remain
stable between releases of CLPM. When verbose mode is active, print more details
about the build, host, and features.")

(defparameter *version-ui*
  (adopt:make-interface
   :name "clpm-version"
   :summary "Common Lisp Package Manager Version"
   :usage "version [options]"
   :help *help-text*
   :contents (list *group-common*)))

(defun print-version (options)
  (if (plusp (gethash :cli-config-log-level options))
      (progn
        (format *stdout* "CLPM version ~A~%" (clpm-version))
        (format *stdout* "~A ~A~%" (lisp-implementation-type) (lisp-implementation-version))
        (format *stdout* "ASDF ~A~%" (asdf:asdf-version))
        (format *stdout* "Software Type: ~A~%" (software-type))
        (format *stdout* "Software Version: ~A~%" (software-version))
        (with-standard-io-syntax
          (let ((*print-pretty* t))
            (format *stdout* "Features:~%~S~%" *features*))))
      (format *stdout* "~A~%" (clpm-version))))


(define-cli-command (("version") *version-ui*) (args options)
  (declare (ignore args))
  (print-version options)
  t)
