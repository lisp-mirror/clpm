;;;; clpm bundle install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/install
    (:use #:cl
          #:clpm/cli/bundle/common
          #:clpm/cli/entry
          #:clpm/clpmfile
          #:clpm/install
          #:clpm/log
          #:clpm/resolve
          #:clpm/requirement
          #:clpm/source)
  (:import-from #:net.didierverna.clon
                #:defsynopsis
                #:make-context
                #:getopt
                #:remainder
                #:help)
  (:export #:cli-bundle-install))

(in-package #:clpm/cli/bundle/install)

(setup-logger)

(defparameter *synopsis*
  (defsynopsis (:make-default nil)
    (text :contents "bundle install")
    *bundle-arguments*
    *common-arguments*))

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
                               (mapcar #'system-release/system-file system-releases))))))

(define-bundle-entry install (*synopsis*)
  (let* ((clpmfile-pathname (merge-pathnames (getopt :short-name "f")
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
                 (declare (ignore c))
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
           (releases (mapcar #'system-file/release system-files)))
      (log:info "Installing releases")
      (mapc #'install-release (remove-if #'release-installed-p releases)))
    t))
