;;;; clpm bundle install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/install
    (:use #:cl
          #:clpm/cli/bundle/common
          #:clpm/cli/common-args
          #:clpm/cli/subcommands
          #:clpm/clpmfile
          #:clpm/context
          #:clpm/install
          #:clpm/log
          #:clpm/resolve
          #:clpm/source)
  (:import-from #:adopt))

(in-package #:clpm/cli/bundle/install)

(setup-logger)

(defparameter *bundle-install-ui*
  (adopt:make-interface
   :name "clpm bundle install"
   :summary "Common Lisp Package Manager Bundle Install"
   :usage "bundle install [options]"
   :help "Install a bundle"
   :contents (list *group-common*
                   *group-bundle*)))

(defun build-lockfile (clpmfile)
  "Given a clpmfile instance, make a lockfile instance for it."
  (let* ((sources (clpmfile/sources clpmfile))
         (reqs (append
                (clpmfile/all-requirements clpmfile)))
         (context (make-instance 'context
                                 :sources sources
                                 :requirements reqs)))
    ;; Resolve the requirements!
    (resolve-requirements context)))

(define-cli-command (("bundle" "install") *bundle-install-ui*) (args options)
  (let* ((clpmfile-pathname (merge-pathnames (gethash :bundle-file options)
                                             (uiop:getcwd)))
         (lockfile-pathname (merge-pathnames (make-pathname :type "lock")
                                             clpmfile-pathname))
         (clpmfile (read-clpmfile clpmfile-pathname))
         lockfile)
    (if (probe-file lockfile-pathname)
        (handler-bind
            ((source-no-such-object
               (lambda (c)
                 (when (find-restart 'sync-and-retry)
                   (log:info "Syncing source and retrying")
                   (invoke-restart 'sync-and-retry c)))))
          (setf lockfile (load-anonymous-context-from-pathname lockfile-pathname)))
        (progn
          ;; The lock file doesn't exist. Create it!
          (log:info "syncing sources")
          (mapc #'sync-source (clpmfile/sources clpmfile))
          (log:info "Creating lockfile")
          (setf lockfile (build-lockfile clpmfile))
          (log:info "context: ~S" lockfile)
          (with-open-file (stream lockfile-pathname
                                  :direction :output)
            (serialize-context-to-stream lockfile stream))))
    (mapc #'install-release (context-releases lockfile))
    t))
