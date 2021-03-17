;;;; clpm config info
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/config/info
    (:use #:cl
          #:clpm-cli/common-args
          #:clpm-cli/commands/config/common
          #:clpm-cli/interface-defs
          #:clpm/config)
  (:import-from #:uiop
                #:*stdout*))

(in-package #:clpm-cli/commands/config/info)

(defparameter *config-info-ui*
  (adopt:make-interface
   :name "clpm config info"
   :summary "Common Lisp Project Manager"
   :usage "config info [options]"
   :help "Common Lisp Project Manager"
   :contents (list *group-common*)))

(define-cli-command (("config" "info") *config-info-ui*) (args options)
  (declare (ignore args options))
  (format *stdout* "Config directories: ~A~%~%" *clpm-config-directories*)
  (format *stdout* "Current configuration:~%~A~%" (with-output-to-string (s) (print-config s)))
  t)
