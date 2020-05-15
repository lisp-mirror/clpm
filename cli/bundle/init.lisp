;;;; clpm bundle init
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/bundle/init
    (:use #:cl
          #:clpm/bundle
          #:clpm-cli/bundle/common
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs
          #:clpm/log)
  (:import-from #:adopt))

(in-package #:clpm-cli/bundle/init)

(setup-logger)

(defparameter *option-bundle-init-asd*
  (adopt:make-option
   :bundle-init-asds
   :long "asd"
   :short #\a
   :parameter "ASD-FILE"
   :help "Include this .asd file in the bundle."
   :reduce (adopt:flip #'cons)))

(defparameter *bundle-init-ui*
  (adopt:make-interface
   :name "clpm bundle init"
   :summary "Common Lisp Package Manager Bundle Init"
   :usage "bundle init [options]"
   :help "Create a clpmfile"
   :contents (list *group-common*
                   *group-bundle*
                   *option-bundle-init-asd*)))

(define-cli-command (("bundle" "init") *bundle-init-ui*) (args options)
  (declare (ignore args))
  (let* ((clpmfile-pathname (bundle-clpmfile-pathname))
         (asds (gethash :bundle-init-asds options)))
    (bundle-init clpmfile-pathname :asds asds)
    t))
