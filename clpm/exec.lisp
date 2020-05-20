;;;; Exec'ing commands in a context
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/exec
    (:use #:cl
          #:clpm/config
          #:clpm/context
          #:clpm/execvpe
          #:clpm/session
          #:clpm/source)
  (:export #:exec))

(in-package #:clpm/exec)

(defun exec (command args &key context
                            with-client-p)
  "exec(3) (or approximate if system doesn't have exec) a COMMAND in a CONTEXT.

COMMAND must be a string naming the command to run.

ARGS must be a list of strings containing the arguments to pass to the command.

If WITH-CLIENT-P is non-NIL, the clpm-client system is available."
  (unless (stringp command)
    (error "COMMAND must be a string."))
  (with-clpm-session ()
    (with-sources-using-installed-only ()
      (with-context (context)
        (let* ((context-name (context-name context))
               (ignore-inherited-source-registry
                 (or (context-anonymous-p context)
                     (config-value :contexts context-name :ignore-inherited-source-registry)))
               (splice-inherited (uiop:getenvp "CL_SOURCE_REGISTRY"))
               (source-registry (context-to-asdf-source-registry-form
                                 context
                                 :with-client with-client-p
                                 :ignore-inherited ignore-inherited-source-registry
                                 :splice-inherited splice-inherited))
               (output-translations (context-output-translations context))
               (installed-system-names (sort (mapcar #'system-name (context-installed-systems context)) #'string<))
               (installed-primary-system-names (remove-duplicates (mapcar #'asdf:primary-system-name installed-system-names)
                                                                  :test #'equal))
               (visible-primary-system-names (sort (context-visible-primary-system-names context) #'string<))
               (editable-primary-system-names (context-editable-primary-system-names context)))
          (with-standard-io-syntax
            (let ((*print-case* :downcase))
              (execvpe command args
                       `(("CL_SOURCE_REGISTRY" . ,(format nil "~S" source-registry))
                         ,@(when output-translations
                             `(("ASDF_OUTPUT_TRANSLATIONS" . ,(format nil "~S" output-translations))))
                         ,(if (context-anonymous-p context)
                              `("CLPM_EXEC_CLPMFILE" . ,(uiop:native-namestring context-name))
                              `("CLPM_EXEC_CONTEXT" . ,context-name))
                         ("CLPM_EXEC_EDITABLE_PRIMARY_SYSTEMS" . ,(format nil "~S" editable-primary-system-names))
                         ("CLPM_EXEC_INSTALLED_PRIMARY_SYSTEMS" . ,(format nil "~S" installed-primary-system-names))
                         ("CLPM_EXEC_INSTALLED_SYSTEMS" . ,(format nil "~S" installed-system-names))
                         ("CLPM_EXEC_VISIBLE_PRIMARY_SYSTEMS" . ,(format nil "~S" visible-primary-system-names))
                         ,@(when ignore-inherited-source-registry
                             '(("CLPM_EXEC_IGNORE_INHERITED_SOURCE_REGISTRY" . "t")))
                         ,@(when (and (not ignore-inherited-source-registry) splice-inherited)
                             `(("CLPM_EXEC_SPLICE_INHERITED_SOURCE_REGISTRY" . ,splice-inherited))))
                       t))))))))
