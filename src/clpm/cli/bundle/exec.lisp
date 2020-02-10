;;;; clpm bundle exec
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/exec
    (:use #:cl
          #:clpm/cli/bundle/common
          #:clpm/cli/common-args
          #:clpm/cli/subcommands
          #:clpm/clpmfile
          #:clpm/context
          #:clpm/execvpe
          #:clpm/log
          #:clpm/source
          #:clpm/utils)
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

(define-cli-command (("bundle" "exec") *bundle-exec-ui*) (args options)
  (let* ((clpmfile-pathname (merge-pathnames (gethash :bundle-file options)
                                             (uiop:getcwd)))
         (lockfile-pathname (merge-pathnames (make-pathname :type "lock")
                                             clpmfile-pathname))
         (lockfile (load-anonymous-context-from-pathname lockfile-pathname))
         (cl-source-registry-form (context-to-asdf-source-registry-form lockfile))
         (cl-source-registry-value (format nil "~S" cl-source-registry-form))
         (command args))
    (log:debug "Computed CL_SOURCE_REGISTRY:~%~S" cl-source-registry-form)
    (execvpe (first command) (rest command)
             `(("CL_SOURCE_REGISTRY" . ,cl-source-registry-value)
               ("CLPM_BUNDLE_BIN_PATH" . ,(or *live-script-location*
                                              (uiop:argv0)))
               ("CLPM_BUNDLE_CLPMFILE" . ,(uiop:native-namestring clpmfile-pathname))
               ("CLPM_BUNDLE_CLPMFILE_LOCK" . ,(uiop:native-namestring lockfile-pathname)))
             t)
    ;; We got here, there is some .asd file not present. Tell the user!
    ;; (format *error-output* "The following system files are missing! Please run `clpm bundle install` and try again!~%~A" missing-pathnames)
    nil))
