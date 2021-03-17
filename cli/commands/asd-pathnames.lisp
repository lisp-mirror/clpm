;;;; clpm asd-pathnames
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/asd-pathnames
    (:use #:cl
          #:alexandria
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt)
  (:import-from #:clpm))

(in-package #:clpm-cli/commands/asd-pathnames)

(defparameter *asd-pathnames-ui*
  (adopt:make-interface
   :name "clpm asd-pathnames"
   :summary "Common Lisp Project Manager Asd-Pathnames"
   :usage "asd-pathnames [options]"
   :help "List the pathnames to all ASD files in the context, one per line."
   :contents (list *group-common*
                   *option-context*)))

(define-cli-command (("asd-pathnames") *asd-pathnames-ui*) (args options)
  (declare (ignore options args))
  (format t "~{~A~^~%~}~%" (clpm:asd-pathnames))
  t)
