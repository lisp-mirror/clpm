;;;; bundle functionality
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/bundle
    (:use #:cl
          #:clpm/clpmfile
          #:clpm/context
          #:clpm/install
          #:clpm/log
          #:clpm/resolve
          #:clpm/source)
  (:export #:bundle-install
           #:bundle-update))

(in-package #:clpm/bundle)

(setup-logger)

(defun create-empty-lockfile (clpmfile)
  (make-instance 'context
                 :sources (clpmfile-sources clpmfile)
                 :requirements (clpmfile-all-requirements clpmfile)))

(defun build-lockfile (clpmfile)
  "Given a clpmfile instance, make a lockfile context for it."
  (let* ((lockfile (create-empty-lockfile clpmfile))
         (lockfile-pathname (clpmfile-lockfile-pathname clpmfile)))
    (log:info "syncing sources")
    (mapc #'sync-source (clpmfile-sources clpmfile))
    (log:info "Resolving requirements")
    (setf lockfile (resolve-requirements lockfile))
    (with-open-file (stream lockfile-pathname
                            :direction :output)
      (serialize-context-to-stream lockfile stream))
    lockfile))

(defun load-lockfile (pathname)
  (handler-bind
      ((source-no-such-object
         (lambda (c)
           (when (find-restart 'sync-and-retry)
             (log:info "Syncing source and retrying")
             (invoke-restart 'sync-and-retry c)))))
    (load-anonymous-context-from-pathname pathname)))

(defun bundle-install (clpmfile-designator)
  "Given a clpmfile instance, install all releases from its lock file, creating
the lock file if necessary."
  (let* ((clpmfile (get-clpmfile clpmfile-designator))
         (lockfile-pathname (clpmfile-lockfile-pathname clpmfile))
         (lockfile nil))
    (if (probe-file lockfile-pathname)
        (setf lockfile (load-lockfile lockfile-pathname))
        (setf lockfile (build-lockfile clpmfile)))
    (mapc #'install-release (context-releases lockfile))))

(defun bundle-update (clpmfile-designator &key
                                            update-projects (validate (constantly t)))
  (let* ((clpmfile (get-clpmfile clpmfile-designator))
         (lockfile-pathname (clpmfile-lockfile-pathname clpmfile))
         (lockfile nil))
    (log:info "syncing sources")
    (mapc #'sync-source (clpmfile-sources clpmfile))
    (if (probe-file lockfile-pathname)
        ;; Load the existing lockfile
        (setf lockfile (load-lockfile lockfile-pathname))
        ;; No lock file is present. Create an empty context.
        (setf lockfile (create-empty-lockfile clpmfile)))
    ;; Resolve the requirements, allowing for updates.
    (let* ((new-lockfile (resolve-requirements lockfile :update-projects (or update-projects t)))
           (diff (context-diff lockfile new-lockfile)))
      (when (funcall validate diff)
        (mapc #'install-release (context-releases new-lockfile))
        (with-open-file (stream lockfile-pathname
                                :direction :output :if-exists :supersede)
          (serialize-context-to-stream new-lockfile stream))))))
