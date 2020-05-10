;;;; clpm bundle exec
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/exec
    (:use #:cl
          #:clpm/bundle
          #:clpm/cache
          #:clpm/cli/bundle/common
          #:clpm/cli/common-args
          #:clpm/cli/interface-defs
          #:clpm/clpmfile
          #:clpm/config
          #:clpm/execvpe
          #:clpm/log
          #:clpm/utils
          #:do-urlencode)
  (:import-from #:adopt))

(in-package #:clpm/cli/bundle/exec)

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

(defun compute-output-translations (clpmfile-pathname)
  (let ((config-value (config-value :bundle :output-translation)))
    (case config-value
      ((nil)
       nil)
      ((t)
       `(:output-translations
         :ignore-inherited-configuration
         (t (:root ,@(rest (pathname-directory (clpm-cache-pathname '("bundle" "fasl-cache")
                                                                    :ensure-directory t)))
                   ,(urlencode (format nil "~{~A~^/~}" (rest (pathname-directory clpmfile-pathname))))
                   :implementation :**/ :*.*.*))))
      (:local
       `(:output-translations
         :ignore-inherited-configuration
         (t (,(uiop:pathname-directory-pathname clpmfile-pathname) ".clpm" "fasl-cache"
             :implementation :**/ :*.*.*)))))))

(define-cli-command (("bundle" "exec") *bundle-exec-ui*) (args options)
  (let* ((clpmfile-pathname (bundle-clpmfile-pathname))
         (clpmfile (get-clpmfile clpmfile-pathname :installed-only-p t))
         (lockfile-pathname (clpmfile-lockfile-pathname clpmfile))
         (*default-pathname-defaults* (uiop:pathname-directory-pathname clpmfile-pathname))
         (include-client-p (gethash :bundle-exec-with-client options))
         (cl-source-registry-form (bundle-source-registry clpmfile-pathname
                                                          :include-client-p include-client-p
                                                          :installed-only-p t))
         (cl-source-registry-value (format nil "~S" cl-source-registry-form))
         (output-translations-form (compute-output-translations clpmfile-pathname))
         (command args))
    (log:debug "Computed CL_SOURCE_REGISTRY:~%~S" cl-source-registry-form)
    (execvpe (first command) (rest command)
             `(("CL_SOURCE_REGISTRY" . ,cl-source-registry-value)
               ,@(when output-translations-form
                   `(("ASDF_OUTPUT_TRANSLATIONS" . ,(with-output-to-string (s) (prin1 output-translations-form s)))))
               ,@(if *live-script-location*
                     `(("CLPM_BUNDLE_BIN_LIVE_SCRIPT" . ,(uiop:native-namestring *live-script-location*))
                       ("CLPM_BUNDLE_BIN_LISP_IMPLEMENTATION" . ,(lisp-implementation-type)))
                     `(("CLPM_BUNDLE_BIN" . ,(uiop:argv0))))
               ("CLPM_BUNDLE_CLPMFILE" . ,(uiop:native-namestring clpmfile-pathname))
               ("CLPM_BUNDLE_CLPMFILE_LOCK" . ,(uiop:native-namestring lockfile-pathname)))
             t)
    ;; We got here, there is some .asd file not present. Tell the user!
    ;; (format *error-output* "The following system files are missing! Please run `clpm bundle install` and try again!~%~A" missing-pathnames)
    nil))
