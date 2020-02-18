;;;; bundle functionality
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/bundle
    (:use #:cl
          #:alexandria
          #:anaphora
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

(defun build-lockfile (clpmfile &key localp)
  "Given a clpmfile instance, make a lockfile context for it."
  (let* ((lockfile (create-empty-lockfile clpmfile))
         (lockfile-pathname (clpmfile-lockfile-pathname clpmfile)))
    (unless localp
      (log:info "syncing sources")
      (mapc #'sync-source (clpmfile-sources clpmfile)))
    (log:info "Resolving requirements")
    (setf lockfile (resolve-requirements lockfile))
    (with-open-file (stream lockfile-pathname
                            :direction :output)
      (serialize-context-to-stream lockfile stream))
    lockfile))

(defun load-lockfile (pathname &key localp)
  (handler-bind
      ((source-no-such-object
         (lambda (c)
           (when (and (find-restart 'sync-and-retry) (not localp))
             (log:info "Syncing source and retrying")
             (invoke-restart 'sync-and-retry c)))))
    (load-anonymous-context-from-pathname pathname)))

(defun bundle-install (clpmfile-designator &key localp (validate (constantly t)))
  "Given a clpmfile instance, install all releases from its lock file, creating
the lock file if necessary."
  (let* ((clpmfile (get-clpmfile clpmfile-designator))
         (lockfile-pathname (clpmfile-lockfile-pathname clpmfile))
         (lockfile nil)
         (changedp nil))
    (if (probe-file lockfile-pathname)
        (setf lockfile (load-lockfile lockfile-pathname :localp localp))
        (setf lockfile (create-empty-lockfile clpmfile)))
    (unless localp
      (mapc #'sync-source (clpmfile-sources clpmfile)))
    (setf lockfile (install-requirements (clpmfile-all-requirements clpmfile)
                                         :context lockfile
                                         :validate (lambda (diff)
                                                     (aprog1 (funcall validate diff)
                                                       (setf changedp it)))))
    (when changedp
      (with-open-file (stream lockfile-pathname
                              :direction :output
                              :if-exists :supersede)
        (serialize-context-to-stream lockfile stream)))))

(defun bundle-update (clpmfile-designator &key
                                            update-projects (validate (constantly t))
                                            update-systems
                                            localp)
  (let* ((clpmfile (get-clpmfile clpmfile-designator))
         (lockfile-pathname (clpmfile-lockfile-pathname clpmfile))
         (lockfile nil)
         (changedp nil))
    (unless (probe-file lockfile-pathname)
      ;; There is no lock file currently. Just fall back to BUNDLE-INSTALL.
      (return-from bundle-update
        (bundle-install clpmfile :localp localp :validate validate)))
    ;; Load the existing lockfile
    (setf lockfile (load-lockfile lockfile-pathname :localp localp))
    (unless localp
      (mapc #'sync-source (clpmfile-sources clpmfile)))
    ;; Map all update systems to their projects.
    (dolist (system update-systems)
      (when-let* ((system-release (find system (context-system-releases lockfile)
                                        :key (compose #'system-name #'system-release-system)
                                        :test #'equal))
                  (release (system-release-release system-release))
                  (project-name (project-name (release-project release))))
        (pushnew project-name update-projects :test #'equal)))
    (setf lockfile (install-requirements (clpmfile-all-requirements clpmfile)
                                         :context lockfile
                                         :validate (lambda (diff)
                                                     (aprog1 (funcall validate diff)
                                                       (setf changedp it)))
                                         :update-projects (or update-projects t)))
    (when changedp
      (with-open-file (stream lockfile-pathname
                              :direction :output
                              :if-exists :supersede)
        (serialize-context-to-stream lockfile stream)))))
