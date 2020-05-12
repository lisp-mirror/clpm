;;;; clpm context source-registry
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/context/source-registry
    (:use #:cl
          #:alexandria
          #:clpm/context
          #:clpm/cli/context/common
          #:clpm/cli/common-args
          #:clpm/cli/interface-defs)
  (:import-from #:adopt))

(in-package #:clpm/cli/context/source-registry)

(defparameter *context-source-registry-ui*
  (adopt:make-interface
   :name "clpm context source-registry"
   :summary "Common Lisp Package Manager Context Source-Registry"
   :usage "context source-registry [options] CONTEXT-NAME"
   :help "Print an ASDF source-registry form using the projects installed in a context"
   :contents (list *group-common*)))

(define-cli-command (("context" "source-registry") *context-source-registry-ui*) (args options)
  (declare (ignore options))
  (assert (length= 1 args))
  (with-standard-io-syntax
    (let ((*print-case* :downcase))
      (prin1 (context-to-asdf-source-registry-form (first args))))
    (terpri))
  t)
