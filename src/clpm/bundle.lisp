;;;; bundle functionality
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/bundle
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/client
          #:clpm/clpmfile
          #:clpm/config
          #:clpm/context
          #:clpm/install
          #:clpm/log
          #:clpm/repos
          #:clpm/resolve
          #:clpm/source)
  (:export #:bundle-clpmfile-pathname
           #:bundle-install
           #:bundle-source-registry
           #:bundle-update))

(in-package #:clpm/bundle)

(setup-logger)

(defun bundle-clpmfile-pathname ()
  (merge-pathnames (config-value :bundle :clpmfile)
                   (uiop:getcwd)))

(defun create-empty-lockfile (clpmfile)
  (make-instance 'context
                 :sources (clpmfile-sources clpmfile)
                 :requirements (clpmfile-all-requirements clpmfile)))

(defun make-vcs-override-fun (clpmfile-pathname)
  (let ((clpmfile-directory (uiop:pathname-directory-pathname clpmfile-pathname)))
    (lambda (project-name)
      (let ((override (config-value :bundle :local project-name)))
        (when override
          (merge-pathnames override clpmfile-directory))))))

(defun load-lockfile (pathname &key installed-only-p)
  (load-anonymous-context-from-pathname pathname :installed-only-p installed-only-p))

(defun bundle-install (clpmfile-designator &key (validate (constantly t)) no-resolve)
  "Given a clpmfile instance, install all releases from its lock file, creating
the lock file if necessary."
  (let* ((*fetch-repo-automatically* (not (config-value :local)))
         (clpmfile (get-clpmfile clpmfile-designator))
         (lockfile-pathname (clpmfile-lockfile-pathname clpmfile))
         (*vcs-project-override-fun* (make-vcs-override-fun (clpmfile-pathname clpmfile)))
         (lockfile nil)
         (changedp nil))
    (unless (config-value :local)
      (dolist (s (clpmfile-sources clpmfile))
        (unless (source-can-lazy-sync-p s)
          (sync-source s))))
    (if (probe-file lockfile-pathname)
        (setf lockfile (load-lockfile lockfile-pathname))
        (setf lockfile (create-empty-lockfile clpmfile)))
    (if no-resolve
        (mapc #'install-release (context-releases lockfile))
        (progn
          ;; Nuke the lockfile's requirements so that we pick up deletions from the
          ;; clpmfile.
          (setf (context-requirements lockfile) nil)
          (setf lockfile (install-requirements (clpmfile-all-requirements clpmfile)
                                               :context lockfile
                                               :validate (lambda (diff)
                                                           (aprog1 (funcall validate diff)
                                                             (setf changedp it)))
                                               :update-projects (config-table-keys :bundle :local)))
          (when changedp
            (with-open-file (stream lockfile-pathname
                                    :direction :output
                                    :if-exists :supersede)
              (serialize-context-to-stream lockfile stream)))))
    changedp))

(defun bundle-source-registry (clpmfile-designator
                               &key include-client-p ignore-missing-releases
                                 installed-only-p)
  (let* ((*fetch-repo-automatically* nil)
         (clpmfile (get-clpmfile clpmfile-designator))
         (lockfile-pathname (clpmfile-lockfile-pathname clpmfile))
         (*vcs-project-override-fun* (make-vcs-override-fun (clpmfile-pathname clpmfile)))
         lockfile)
    (unless (probe-file lockfile-pathname)
      (error "Lockfile ~A does not exist" lockfile-pathname))
    (setf lockfile (load-lockfile lockfile-pathname :installed-only-p installed-only-p))
    (unless ignore-missing-releases
      (let* ((releases (context-releases lockfile))
             (missing-releases (remove-if #'release-installed-p releases)))
        (when missing-releases
          (error "The following releases are not installed: ~{~S~^, ~}"
                 (mapcar (compose #'project-name #'release-project) missing-releases)))))
    (context-to-asdf-source-registry-form
     lockfile
     (when include-client-p
       `((:directory ,(uiop:pathname-directory-pathname (client-asd-pathname))))))))

(defun bundle-update (clpmfile-designator &key
                                            update-projects (validate (constantly t))
                                            update-systems)
  (let* ((*fetch-repo-automatically* (not (config-value :local)))
         (clpmfile (get-clpmfile clpmfile-designator))
         (lockfile-pathname (clpmfile-lockfile-pathname clpmfile))
         (*vcs-project-override-fun* (make-vcs-override-fun (clpmfile-pathname clpmfile)))
         (lockfile nil)
         (changedp nil))
    (unless (probe-file lockfile-pathname)
      ;; There is no lock file currently. Just fall back to BUNDLE-INSTALL.
      (return-from bundle-update
        (bundle-install clpmfile :validate validate)))
    ;; Load the existing lockfile
    (setf lockfile (load-lockfile lockfile-pathname))
    (unless (config-value :local)
      (dolist (s (clpmfile-sources clpmfile))
        (unless (source-can-lazy-sync-p s)
          (sync-source s))))
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
