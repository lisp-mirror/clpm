;;;; clpm context systems
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/context/systems
    (:use #:cl
          #:alexandria
          #:clpm/context
          #:clpm/cli/context/common
          #:clpm/cli/common-args
          #:clpm/cli/interface-defs
          #:clpm/source)
  (:import-from #:adopt))

(in-package #:clpm/cli/context/systems)

(defparameter *context-systems-ui*
  (adopt:make-interface
   :name "clpm context systems"
   :summary "Common Lisp Package Manager Context Systems"
   :usage "context systems [options] CONTEXT-NAME"
   :help "List the names of all systems installed in the context, one per line"
   :contents (list *group-common*)))

(define-cli-command (("context" "systems") *context-systems-ui*) (args options)
  (declare (ignore options))
  (assert (length= 1 args))
  (format t "~{~A~%~}" (sort (mapcar #'system-name (context-installed-systems (first args)))
                             #'string<))
  t)
