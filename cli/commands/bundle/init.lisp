;;;; clpm bundle init
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/bundle/init
    (:use #:cl
          #:clpm-cli/commands/bundle/common
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt)
  (:import-from #:clpm))

(in-package #:clpm-cli/commands/bundle/init)

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
   :summary "Common Lisp Project Manager Bundle Init"
   :usage "bundle init [options]"
   :help "Create a clpmfile"
   :contents (list *group-common*
                   *group-bundle*
                   *option-bundle-init-asd*)))

(define-cli-command (("bundle" "init") *bundle-init-ui*) (args options)
  (declare (ignore args))
  (let ((asds (gethash :bundle-init-asds options)))
    (clpm:bundle-init :asds asds)
    t))
