;;;; clpm bundle source-registry
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/bundle/source-registry
    (:use #:cl
          #:clpm-cli/commands/bundle/common
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt)
  (:import-from #:clpm))

(in-package #:clpm-cli/commands/bundle/source-registry)

(defparameter *option-with-client*
  (adopt:make-option
   :bundle-exec-with-client
   :long "with-client"
   :help "Include the CLPM client in the source registry"
   :reduce (constantly t)))

(defparameter *bundle-source-registry-ui*
  (adopt:make-interface
   :name "clpm bundle source-registry"
   :summary "Common Lisp Project Manager Bundle Source-registry"
   :usage "bundle source-registry [options]"
   :help "Print the source registry for a bundle."
   :contents (list *group-common*
                   *group-bundle*
                   *option-with-client*)))

(define-cli-command (("bundle" "source-registry") *bundle-source-registry-ui*) (args options)
  (declare (ignore args))
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (source-registry-form
            (clpm:bundle-source-registry :with-client-p (gethash :bundle-exec-with-client options))))
      (format t "~S~%" source-registry-form)))
  t)
