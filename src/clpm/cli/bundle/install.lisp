;;;; clpm bundle install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/install
    (:use #:cl
          #:clpm/cli/bundle/common
          #:clpm/cli/common-args
          #:clpm/cli/config/common
          #:clpm/cli/subcommands
          #:clpm/clpmfile
          #:clpm/install
          #:clpm/log
          #:clpm/resolve
          #:clpm/requirement
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
         (reqs (clpmfile/all-requirements clpmfile)))

    ;; Resolve the requirements!
    (multiple-value-bind (releases-to-install system-releases)
        (resolve-requirements reqs sources)
      (declare (ignore releases-to-install))
      ;; Make the lock file and return it.
      (make-lockfile clpmfile (remove-duplicates
                               (mapcar #'system-release-system-file system-releases))))))

(define-cli-command (("bundle" "install") *bundle-install-ui*) (args options)
  (let* ((clpmfile-pathname (merge-pathnames (gethash :bundle-file options)
                                             (uiop:getcwd)))
         (lockfile-pathname (merge-pathnames (make-pathname :type "lock")
                                             clpmfile-pathname))
         (clpmfile (read-clpmfile clpmfile-pathname))
         (sources (clpmfile/user-global-sources clpmfile))
         lockfile)
    (log:info "clpmfile located at ~S" clpmfile-pathname)
    ;; Get the lock file
    (if (probe-file lockfile-pathname)
        (handler-bind
            ((source-no-such-object
               (lambda (c)
                 (when (find-restart 'sync-and-retry)
                   (log:info "Syncing source and retrying")
                   (invoke-restart 'sync-and-retry c)))))
          (setf lockfile (read-lockfile lockfile-pathname)))
        (progn
          ;; The lock file doesn't exist. Create it!
          (log:info "syncing sources")
          (mapc #'sync-source sources)
          (log:info "Creating lockfile")
          (setf lockfile (build-lockfile clpmfile))
          (with-open-file (s lockfile-pathname
                             :direction :output
                             :if-exists :supersede)
            (write-lockfile-to-stream lockfile s))))

    (let* ((system-files (lockfile/system-files lockfile))
           (releases (mapcar #'system-file-release system-files)))
      (log:info "Installing releases")
      (mapc #'install-release (remove-if #'release-installed-p releases)))
    t))
