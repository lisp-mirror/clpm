;;;; clpm bundle exec
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/exec
    (:use #:cl
          #:clpm/cli/bundle/common
          #:clpm/cli/common-args
          #:clpm/cli/config/common
          #:clpm/cli/subcommands
          #:clpm/client
          #:clpm/clpmfile
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
         (lockfile (read-lockfile lockfile-pathname))
         (system-files (lockfile/system-files lockfile))
         (asdf-pathnames (mapcar #'system-file-absolute-asd-pathname system-files))
         (missing-pathnames (remove-if #'probe-file asdf-pathnames))
         (client-location (when (gethash :bundle-exec-with-client options)
                            (ensure-client-written)
                            (clpm-client-lib-location)))
         (extra-source-registry (uiop:getenv "CLPM_BUNDLE_EXTRA_SOURCE_REGISTRY"))
         (cl-source-registry-value (format nil
                                           #-os-windows "~@[~A:~]~{~A~^:~}~@[:~A~]"
                                           #+os-windows "~@[~A;~]~{~A~^;~}~@[;~A~]"
                                           client-location
                                           (mapcar #'uiop:pathname-directory-pathname asdf-pathnames)
                                           extra-source-registry))
         (command args))
    (unless missing-pathnames
      (log:debug "asdf pathnames available in new process:~%~A" asdf-pathnames)
      (execvpe (first command) (rest command)
               `(("CL_SOURCE_REGISTRY" . ,cl-source-registry-value)
                 ("CLPM_BUNDLE_BIN_PATH" . ,(or *live-script-location*
                                                (uiop:argv0)))
                 ("CLPM_BUNDLE_CLPMFILE" . ,(uiop:native-namestring clpmfile-pathname))
                 ("CLPM_BUNDLE_CLPMFILE_LOCK" . ,(uiop:native-namestring lockfile-pathname)))
               t))
    ;; We got here, there is some .asd file not present. Tell the user!
    (format *error-output* "The following system files are missing! Please run `clpm bundle install` and try again!~%~A" missing-pathnames)
    nil))
