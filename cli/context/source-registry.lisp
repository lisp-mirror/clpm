;;;; clpm context source-registry
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/context/source-registry
    (:use #:cl
          #:alexandria
          #:clpm/context
          #:clpm-cli/context/common
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt))

(in-package #:clpm-cli/context/source-registry)

(defparameter *option-context-source-registry-ignore-inherited-configuration*
  (adopt:make-option
   :context-source-registry-ignore-inherited
   :long "ignore-inherited-configuration"
   :short #\i
   :help "Print a source registry form configured to ignore inherited configuration"
   :reduce (constantly t)))

(defparameter *option-context-source-registry-inherit-env-var*
  (adopt:make-option
   :context-source-registry-inherit-env-var
   :long "splice-environment"
   :short #\e
   :help "Print a source registry form that has the contents of the CL_SOURCE_REGISTRY environment variable spliced in. No effect if inherited configuration is ignored."
   :reduce (constantly t)))

(defparameter *option-context-source-registry-with-client*
  (adopt:make-option
   :context-source-registry-with-client
   :long "with-client"
   :help "Print a source registry form that has the CLPM client included."
   :reduce (constantly t)))

(defparameter *context-source-registry-ui*
  (adopt:make-interface
   :name "clpm context source-registry"
   :summary "Common Lisp Package Manager Context Source-Registry"
   :usage "context source-registry [options] CONTEXT-NAME"
   :help "Print an ASDF source-registry form using the projects installed in a context"
   :contents (list *group-common*
                   *option-context-source-registry-ignore-inherited-configuration*
                   *option-context-source-registry-inherit-env-var*
                   *option-context-source-registry-with-client*)))

(define-cli-command (("context" "source-registry") *context-source-registry-ui*) (args options)
  (assert (length= 1 args))
  (with-standard-io-syntax
    (let ((*print-case* :downcase))
      (prin1 (context-to-asdf-source-registry-form
              (first args)
              :with-client (gethash :context-source-registry-with-client options)
              :ignore-inherited (gethash :context-source-registry-ignore-inherited options)
              :splice-inherited (and (gethash :context-source-registry-inherit-env-var options)
                                     (uiop:getenv "CL_SOURCE_REGISTRY")))))
    (terpri))
  t)
