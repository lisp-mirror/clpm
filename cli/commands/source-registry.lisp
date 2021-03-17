;;;; clpm source-registry
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/source-registry
    (:use #:cl
          #:alexandria
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt)
  (:import-from #:clpm))

(in-package #:clpm-cli/commands/source-registry)

(defparameter *option-source-registry-ignore-inherited-configuration*
  (adopt:make-option
   :source-registry-ignore-inherited
   :long "ignore-inherited-configuration"
   :short #\i
   :help "Print a source registry form configured to ignore inherited configuration"
   :reduce (constantly t)))

(defparameter *option-source-registry-inherit-env-var*
  (adopt:make-option
   :source-registry-inherit-env-var
   :long "splice-environment"
   :short #\e
   :help "Print a source registry form that has the contents of the CL_SOURCE_REGISTRY environment variable spliced in. No effect if inherited configuration is ignored."
   :reduce (constantly t)))

(defparameter *option-source-registry-with-client*
  (adopt:make-option
   :source-registry-with-client
   :long "with-client"
   :help "Print a source registry form that has the CLPM client included."
   :reduce (constantly t)))

(defparameter *source-registry-ui*
  (adopt:make-interface
   :name "clpm source-registry"
   :summary "Common Lisp Project Manager Source-Registry"
   :usage "source-registry [options]"
   :help "Print an ASDF source-registry form using the projects installed in a context"
   :contents (list *group-common*
                   *option-context*
                   *option-source-registry-ignore-inherited-configuration*
                   *option-source-registry-inherit-env-var*
                   *option-source-registry-with-client*)))

(define-cli-command (("source-registry") *source-registry-ui*) (args options)
  (declare (ignore args))
  (with-standard-io-syntax
    (let ((*print-case* :downcase))
      (format t "~S~%" (clpm:source-registry
                        :with-client-p (gethash :source-registry-with-client options)
                        :ignore-inherited-source-registry (gethash :source-registry-ignore-inherited options)
                        :splice-inherited (and (gethash :source-registry-inherit-env-var options)
                                               (uiop:getenv "CL_SOURCE_REGISTRY"))))))
  t)
