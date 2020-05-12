;;;; clpm context visible-systems
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/context/visible-systems
    (:use #:cl
          #:alexandria
          #:clpm/context
          #:clpm/cli/context/common
          #:clpm/cli/common-args
          #:clpm/cli/interface-defs
          #:clpm/source)
  (:import-from #:adopt))

(in-package #:clpm/cli/context/visible-systems)

(defparameter *context-visible-systems-ui*
  (adopt:make-interface
   :name "clpm context visible-systems"
   :summary "Common Lisp Package Manager Context Visible-Systems"
   :usage "context visible-systems [options] CONTEXT-NAME"
   :help "List the primary names of all systems visible to ASDF in the context, one per line"
   :contents (list *group-common*)))

(define-cli-command (("context" "visible-systems") *context-visible-systems-ui*) (args options)
  (declare (ignore options))
  (assert (length= 1 args))
  (format t "~{~A~%~}" (sort (context-visible-primary-system-names (first args))
                             #'string<))
  t)
