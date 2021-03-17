;;;; clpm find
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/find
    (:use #:cl
          #:alexandria
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt)
  (:import-from #:clpm))

(in-package #:clpm-cli/commands/find)

(defparameter *find-ui*
  (adopt:make-interface
   :name "clpm find"
   :summary "Common Lisp Project Manager Find"
   :usage "find [options] SYSTEM-NAME"
   :help "List the pathname to a system in the context"
   :contents (list *group-common*
                   *option-context*)))

(define-cli-command (("find") *find-ui*) (args options)
  (declare (ignore options))
  (assert (length= 1 args))
  (let ((pathname (clpm:find-system-asd-pathname (first args))))
    (when pathname
      (format t "~A~%" pathname)
      t)))
