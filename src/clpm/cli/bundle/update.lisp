;;;; clpm bundle update
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/update
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
  (:export #:cli-bundle-update))

(in-package #:clpm/cli/bundle/update)

(setup-logger)

(defparameter *bundle-update-ui*
  (adopt:make-interface
   :name "clpm bundle update"
   :summary "Common Lisp Package Manager Bundle Update"
   :usage "bundle update [options]"
   :help "Update a bundle"
   :contents (list *group-common*
                   *group-bundle*)))

(defun build-lockfile (clpmfile)
  "Given a clpmfile instance, make a lockfile instance for it."
  (let* ((sources (clpmfile/sources clpmfile))
         (user-global-sources (clpmfile/user-global-sources clpmfile))
         (reqs (clpmfile/all-requirements clpmfile)))
    (mapc #'sync-source user-global-sources)

    ;; Resolve the requirements!
    (multiple-value-bind (releases-to-install system-releases)
        (resolve-requirements reqs sources)
      (declare (ignore releases-to-install))
      ;; Make the lock file and return it.
      (make-lockfile clpmfile (remove-duplicates
                               (mapcar #'system-release/system-file system-releases))))))

(define-cli-command (("bundle" "update") *bundle-update-ui*) (args options)
  (let* ((clpmfile-pathname (merge-pathnames (gethash :bundle-file options)
                                             (uiop:getcwd)))
         (lockfile-pathname (merge-pathnames (make-pathname :type "lock")
                                             clpmfile-pathname))
         (clpmfile (read-clpmfile clpmfile-pathname))
         (lockfile (build-lockfile clpmfile)))
    (log:info "clpmfile located at ~S" clpmfile-pathname)
    ;; Get the lock file
    (with-open-file (s lockfile-pathname
                       :direction :output
                       :if-exists :supersede)
      (write-lockfile-to-stream lockfile s))
    (let* ((system-files (lockfile/system-files lockfile))
           (releases (mapcar #'system-file/release system-files)))
      (log:info "Installing releases")
      (mapc #'install-release (remove-if #'release-installed-p releases)))
    t))
