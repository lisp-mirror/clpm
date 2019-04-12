;;;; clpm bundle exec
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/exec
    (:use #:cl
          #:clpm/cli/bundle/common
          #:clpm/cli/entry
          #:clpm/client
          #:clpm/clpmfile
          #:clpm/execvpe
          #:clpm/log
          #:clpm/source
          #:clpm/utils)
  (:import-from #:net.didierverna.clon
                #:defsynopsis
                #:make-context
                #:getopt
                #:remainder
                #:help)
  (:export #:cli-bundle-exec))

(in-package #:clpm/cli/bundle/exec)

(setup-logger)

(defparameter *synopsis*
  (defsynopsis (:make-default nil
                :postfix "command")
    (text :contents "bundle exec")
    (flag :long-name "with-client"
          :description "Include the CLPM client in the source registry.")
    *bundle-arguments*
    *common-arguments*))

(define-bundle-entry exec (*synopsis*)
  (let* ((clpmfile-pathname (merge-pathnames (getopt :short-name "f")
                                             (uiop:getcwd)))
         (lockfile-pathname (merge-pathnames (make-pathname :type "lock")
                                             clpmfile-pathname))
         (lockfile (read-lockfile lockfile-pathname))
         (system-files (lockfile/system-files lockfile))
         (asdf-pathnames (mapcar #'system-file/absolute-asd-pathname system-files))
         (missing-pathnames (remove-if #'probe-file asdf-pathnames))
         (client-location (when (getopt :long-name "with-client")
                            (ensure-client-written)
                            (clpm-client-lib-location)))
         (extra-source-registry (uiop:getenv "CLPM_BUNDLE_EXTRA_SOURCE_REGISTRY"))
         (cl-source-registry-value (format nil
                                           #-os-windows "~@[~A:~]~{~A~^:~}~@[:~A~]"
                                           #+os-windows "~@[~A;~]~{~A~^;~}~@[;~A~]"
                                           client-location
                                           (mapcar #'uiop:pathname-directory-pathname asdf-pathnames)
                                           extra-source-registry))
         (command (remainder)))
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
