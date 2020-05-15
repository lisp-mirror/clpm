;;;; clpm bundle exec
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/bundle/exec
    (:use #:cl
          #:clpm/bundle
          #:clpm-cli/commands/bundle/common
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs
          #:clpm/clpmfile
          #:clpm/config
          #:clpm/context
          #:clpm/execvpe
          #:clpm/log
          #:clpm/source
          #:clpm/utils)
  (:import-from #:adopt))

(in-package #:clpm-cli/commands/bundle/exec)

(setup-logger)

(defparameter *option-with-client*
  (adopt:make-option
   :bundle-exec-with-client
   :long "with-client"
   :help "Include the CLPM client in the source registry"
   :reduce (constantly t)))

(defparameter *bundle-exec-ui*
  (adopt:make-interface
   :name "clpm bundle exec"
   :summary "Common Lisp Package Manager Bundle Exec"
   :usage "bundle exec [options] [command]"
   :help "Execute a command in the contet of a bundle"
   :contents (list *group-common*
                   *group-bundle*
                   *option-with-client*)))

(define-cli-command (("bundle" "exec") *bundle-exec-ui*) (args options)
  (let* ((clpmfile-pathname (bundle-clpmfile-pathname))
         (clpmfile (get-clpmfile clpmfile-pathname :installed-only-p t))
         (lockfile-pathname (clpmfile-lockfile-pathname clpmfile))
         (include-client-p (gethash :bundle-exec-with-client options))
         (cl-source-registry-form (bundle-source-registry clpmfile-pathname
                                                          :include-client-p include-client-p
                                                          :installed-only-p t))
         (output-translations-form (bundle-output-translations clpmfile-pathname))
         (lockfile (bundle-context clpmfile))
         (installed-system-names (sort (mapcar #'system-name (context-installed-systems lockfile)) #'string<))
         (visible-primary-system-names (sort (context-visible-primary-system-names lockfile) #'string<))
         (command args))
    (log:debug "Computed CL_SOURCE_REGISTRY:~%~S" cl-source-registry-form)
    (with-standard-io-syntax
      (execvpe (first command) (rest command)
               `(("CL_SOURCE_REGISTRY" . ,(prin1-to-string cl-source-registry-form))
                 ,@(when output-translations-form
                     `(("ASDF_OUTPUT_TRANSLATIONS" . ,(prin1-to-string output-translations-form))))
                 ,@(if *live-script-location*
                       `(("CLPM_BUNDLE_BIN_LIVE_SCRIPT" . ,(uiop:native-namestring *live-script-location*))
                         ("CLPM_BUNDLE_BIN_LISP_IMPLEMENTATION" . ,(lisp-implementation-type)))
                       `(("CLPM_BUNDLE_BIN" . ,(uiop:argv0))))
                 ("CLPM_BUNDLE_INSTALLED_SYSTEMS" . ,(format nil "~{~A~^ ~}" installed-system-names))
                 ("CLPM_BUNDLE_VISIBLE_PRIMARY_SYSTEMS" . ,(format nil "~{~A~^ ~}" visible-primary-system-names))
                 ("CLPM_BUNDLE_CLPMFILE" . ,(uiop:native-namestring clpmfile-pathname))
                 ("CLPM_BUNDLE_CLPMFILE_LOCK" . ,(uiop:native-namestring lockfile-pathname)))
               t))
    ;; We got here, there is some .asd file not present. Tell the user!
    ;; (format *error-output* "The following system files are missing! Please run `clpm bundle install` and try again!~%~A" missing-pathnames)
    nil))
