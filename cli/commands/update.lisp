;;;; clpm update
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/update
    (:use #:cl
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs
          #:clpm/config
          #:clpm/context
          #:clpm/install
          #:clpm/log
          #:clpm/update)
  (:import-from #:adopt))

(in-package #:clpm-cli/commands/update)

(setup-logger)

(defparameter *option-update-yes*
  (adopt:make-option
   :update-yes
   :short #\y
   :long "yes"
   :help "Answer yes to all questions"
   :reduce (constantly t)))

(defparameter *option-update-project*
  (adopt:make-option
   :update-projects
   :short #\p
   :long "project"
   :parameter "PROJECT-NAME"
   :help "Update a project instead of a system."
   :reduce (adopt:flip #'cons)))

(defparameter *update-ui*
  (adopt:make-interface
   :name "clpm update"
   :summary "Common Lisp Package Manager Update"
   :usage "update [options] SYSTEM-NAMES*"
   :help "Common Lisp Package Manager"
   :contents (list *group-common*
                   *option-update-project*
                   *option-update-yes*
                   *option-context*)))

(define-cli-command (("update") *update-ui*) (args options)
  (let ((system-names args)
        (project-names (gethash :update-projects options))
        (context-name (config-value :context))
        (yes-p (gethash :update-yes options)))
    (update :validate (make-diff-validate-fun :yesp yes-p)
            :context context-name
            :update-systems system-names
            :update-projects project-names)
    t))
