;;;; clpm exec
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/exec
    (:use #:cl
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs
          #:clpm/client
          #:clpm/config
          #:clpm/context
          #:clpm/execvpe
          #:clpm/log
          #:clpm/source)
  (:import-from #:adopt))

(in-package #:clpm-cli/commands/exec)

(setup-logger)

(defparameter *option-with-client*
  (adopt:make-option
   :exec-with-client
   :long "with-client"
   :help "Include the CLPM client in the source registry"
   :reduce (constantly t)))

(defparameter *exec-ui*
  (adopt:make-interface
   :name "clpm exec"
   :summary "Common Lisp Package Manager Exec"
   :usage "exec [options] [command]"
   :help "Execute a command with environment variables configured to use a context"
   :contents (list *group-common*
                   *option-context*
                   *option-with-client*)))

(define-cli-command (("exec") *exec-ui*) (args options)
  (let* ((context-name (config-value :context))
         (with-client (gethash :exec-with-client options))
         (ignore-inherited (config-value :contexts context-name :ignore-inherited-source-registry))
         (splice-inherited (uiop:getenvp "CL_SOURCE_REGISTRY"))
         (source-registry (context-to-asdf-source-registry-form
                           context-name
                           :with-client with-client
                           :ignore-inherited ignore-inherited
                           :splice-inherited splice-inherited))
         (output-translations (context-output-translations context-name))
         (installed-system-names (sort (mapcar #'system-name (context-installed-systems context-name)) #'string<))
         (visible-primary-system-names (sort (context-visible-primary-system-names context-name) #'string<)))
    (with-standard-io-syntax
      (execvpe (first args) (rest args)
               `(("CL_SOURCE_REGISTRY" . ,(prin1-to-string source-registry))
                 ,@(when output-translations
                     `(("ASDF_OUTPUT_TRANSLATIONS" . ,(prin1-to-string output-translations))))
                 ("CLPM_EXEC_CONTEXT" . ,context-name)
                 ("CLPM_EXEC_INSTALLED_SYSTEMS" . ,(format nil "~{~A~^ ~}" installed-system-names))
                 ("CLPM_EXEC_VISIBLE_PRIMARY_SYSTEMS" . ,(format nil "~{~A~^ ~}" visible-primary-system-names))
                 ,@(when ignore-inherited
                     '(("CLPM_EXEC_IGNORE_INHERITED_SOURCE_REGISTRY" . "t")))
                 ,@(when (and (not ignore-inherited) splice-inherited)
                     `(("CLPM_EXEC_SPLICE_INHERITED_SOURCE_REGISTRY" . ,splice-inherited))))
               t))))
