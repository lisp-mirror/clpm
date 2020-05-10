;;;; clpm update
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/update
    (:use #:cl
          #:clpm/cli/common-args
          #:clpm/cli/interface-defs
          #:clpm/context
          #:clpm/install
          #:clpm/log
          #:clpm/update)
  (:import-from #:adopt))

(in-package #:clpm/cli/update)

(setup-logger)

(defparameter *option-update-yes*
  (adopt:make-option
   :install-yes
   :short #\y
   :long "yes"
   :help "Answer yes to all questions"
   :reduce (constantly t)))

(defparameter *update-ui*
  (adopt:make-interface
   :name "clpm update"
   :summary "Common Lisp Package Manager Update"
   :usage "update [options] <PROJECTS*>"
   :help "Common Lisp Package Manager"
   :contents (list *group-common*
                   *option-update-yes*
                   *option-context*)))

(defun make-validate-fun (yes-p)
  (lambda (diff)
    (print-context-diff diff *standard-output*)
    (or yes-p (y-or-n-p "Proceed?"))))

(define-cli-command (("update") *update-ui*) (args options)
  (let ((project-names args)
        (context-name (or (gethash :context options)
                          "default"))
        (yes-p (gethash :install-yes options)))
    (update :validate (make-validate-fun yes-p)
            :context context-name
            :update-projects project-names)
    t))
