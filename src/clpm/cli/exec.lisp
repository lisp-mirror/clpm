;;;; clpm exec
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/exec
    (:use #:cl
          #:clpm/cli/common-args
          #:clpm/cli/interface-defs
          #:clpm/client
          #:clpm/config
          #:clpm/context
          #:clpm/execvpe
          #:clpm/log)
  (:import-from #:adopt))

(in-package #:clpm/cli/exec)

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
  (let ((source-registry (context-to-asdf-source-registry-form
                          (config-value :context)
                          (when (gethash :exec-with-client options)
                            `((:directory ,(uiop:pathname-directory-pathname (client-asd-pathname)))))))
        (output-translations (context-output-translations (config-value :context))))
    (with-standard-io-syntax
      (execvpe (first args) (rest args)
               `(("CL_SOURCE_REGISTRY" . ,(prin1-to-string source-registry))
                 ,@(when output-translations
                     `(("ASDF_OUTPUT_TRANSLATIONS" . ,(prin1-to-string output-translations))))
                 ("CLPM_EXEC_CONTEXT" . ,(config-value :context)))
               t))))
