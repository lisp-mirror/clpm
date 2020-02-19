;;;; clpm context find
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/context/find
    (:use #:cl
          #:alexandria
          #:clpm/context
          #:clpm/cli/context/common
          #:clpm/cli/common-args
          #:clpm/cli/subcommands
          #:clpm/source)
  (:import-from #:adopt))

(in-package #:clpm/cli/context/find)

(defparameter *context-find-ui*
  (adopt:make-interface
   :name "clpm context find"
   :summary "Common Lisp Package Manager Context Find"
   :usage "context find [options] SYSTEM-NAME"
   :help "List the pathname to a system in the context"
   :contents (list *group-common*
                   *option-context*
                   *option-output*)))

(define-cli-command (("context" "find") *context-find-ui*) (args options)
  (assert (length= 1 args))
  (let* ((context-name (gethash :context options "default"))
         (context (get-context context-name))
         (system-release (find (first args) (context-system-releases context)
                               :key (compose #'system-name #'system-release-system)
                               :test #'equal)))
    (when system-release
      (eswitch ((gethash :output options) :test 'equal)
        (nil
         (format t "~A~%" (system-release-absolute-asd-pathname system-release)))
        ("sexp"
         (uiop:with-safe-io-syntax ()
           (prin1 (system-release-absolute-asd-pathname system-release))
           (terpri))))
      t)))
