;;;; clpm context pathnames
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/context/pathnames
    (:use #:cl
          #:alexandria
          #:clpm/context
          #:clpm/cli/context/common
          #:clpm/cli/common-args
          #:clpm/cli/subcommands)
  (:import-from #:adopt))

(in-package #:clpm/cli/context/pathnames)

(defparameter *context-pathnames-ui*
  (adopt:make-interface
   :name "clpm context pathnames"
   :summary "Common Lisp Package Manager Context Pathnames"
   :usage "context pathnames [options] CONTEXT-NAME"
   :help "List the pathnames to all ASD files in the context"
   :contents (list *group-common*
                   *option-output*)))

(define-cli-command (("context" "pathnames") *context-pathnames-ui*) (args options)
  (assert (length= 1 args))
  (let* ((context (get-context (first args)))
         (context-pathnames (context-asd-pathnames context)))
    (eswitch ((gethash :output options) :test 'equal)
      (nil
       (format t "~{~A~^~%~}" context-pathnames))
      ("sexp"
       (uiop:with-safe-io-syntax ()
         (prin1 context-pathnames)
         (terpri))))
    t))
