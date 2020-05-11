;;;; clpm context find
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/context/find
    (:use #:cl
          #:alexandria
          #:clpm/config
          #:clpm/context
          #:clpm/cli/context/common
          #:clpm/cli/common-args
          #:clpm/cli/interface-defs
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
  (declare (ignore options))
  (assert (length= 1 args))
  (let* ((context-name (config-value :context))
         (pathname (context-find-system-asd-pathname context-name (first args))))
    (when pathname
      (format t "~A~%" pathname)
      t)))
