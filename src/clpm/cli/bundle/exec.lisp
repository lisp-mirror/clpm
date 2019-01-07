;;;; clpm bundle exec
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/exec
    (:use #:cl
          #:clpm/cli/bundle/common
          #:clpm/cli/entry
          #:clpm/clpmfile
          #:clpm/execvpe
          #:clpm/log
          #:clpm/source)
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
         (cl-source-registry-value (format nil "~{~A~^:~}" asdf-pathnames))
         (command (remainder)))
    (unless missing-pathnames
      (log:debug "asdf pathnames available in new process:~%~A" asdf-pathnames)
      (execvpe (first command) (rest command)
               `(("CL_SOURCE_REGISTRY" . ,cl-source-registry-value))
               t))
    ;; We got here, there is some .asd file not present. Tell the user!
    (format *error-output* "The following system files are missing! Please run `clpm bundle install` and try again!~%~A" missing-pathnames)
    nil))
