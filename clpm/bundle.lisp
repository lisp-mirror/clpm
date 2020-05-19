;;;; bundle functionality
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/bundle
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/cache
          #:clpm/client
          #:clpm/clpmfile
          #:clpm/config
          #:clpm/context
          #:clpm/execvpe
          #:clpm/install
          #:clpm/log
          #:clpm/repos
          #:clpm/resolve
          #:clpm/session
          #:clpm/source
          #:do-urlencode)
  (:export #:bundle-exec
           #:bundle-init
           #:bundle-install
           #:bundle-output-translations
           #:bundle-source-registry
           #:bundle-update
           #:with-bundle-session))

(in-package #:clpm/bundle)

(setup-logger)

(defun call-with-bundle-session (thunk &key clpmfile)
  (with-clpm-session ()
    (with-config-source (:pathname (merge-pathnames ".clpm/bundle.conf"
                                                    (uiop:pathname-directory-pathname
                                                     (clpmfile-pathname clpmfile))))
      (let ((*default-pathname-defaults* (uiop:pathname-directory-pathname (clpmfile-pathname clpmfile)))
            (*vcs-project-override-fun* (make-vcs-override-fun (clpmfile-pathname clpmfile))))
        (funcall thunk)))))

(defmacro with-bundle-session ((clpmfile) &body body)
  `(call-with-bundle-session (lambda () ,@body) :clpmfile ,clpmfile))

(defun create-empty-lockfile (clpmfile)
  (aprog1 (copy-context clpmfile)
    (setf (context-name it) (clpmfile-lockfile-pathname clpmfile))))

(defun make-vcs-override-fun (clpmfile-pathname)
  (let ((clpmfile-directory (uiop:pathname-directory-pathname clpmfile-pathname)))
    (lambda (project-name)
      (let ((override (config-value :bundle :local project-name)))
        (when override
          (merge-pathnames override clpmfile-directory))))))

(defun load-lockfile (pathname)
  (load-anonymous-context-from-pathname pathname))

(defun bundle-init (&key clpmfile (if-exists :error) asds)
  (with-standard-io-syntax
    (let ((*print-case* :downcase))
      (with-open-file (s (clpmfile-pathname clpmfile) :direction :output :if-exists if-exists)
        (write-string ";;; -*- Mode: common-lisp; -*-" s)
        (terpri s)
        (prin1 '(:api-version "0.3") s)
        (terpri s)
        (terpri s)
        (dolist (source (sources))
          (prin1 `(:source ,@(source-to-form source)) s)
          (terpri s))
        (terpri s)
        (dolist (asd asds)
          (prin1 `(:asd ,asd) s)
          (terpri s)))))
  (clpmfile-pathname clpmfile))

(defun bundle-install (&key clpmfile (validate (constantly t)) no-resolve)
  "Given a clpmfile instance, install all releases from its lock file, creating
the lock file if necessary."
  (with-bundle-session (clpmfile)
    (let* ((*fetch-repo-automatically* (not (config-value :local)))
           (clpmfile (get-clpmfile clpmfile))
           (lockfile-pathname (clpmfile-lockfile-pathname clpmfile))
           (lockfile nil)
           (changedp nil))
      (unless (config-value :local)
        (dolist (s (context-sources clpmfile))
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
            (setf lockfile (install-requirements (context-requirements clpmfile)
                                                 :context lockfile
                                                 :validate (lambda (diff)
                                                             (aprog1 (funcall validate diff)
                                                               (setf changedp it)))
                                                 :update-projects (config-table-keys :bundle :local)))
            (when changedp
              (save-context lockfile))))
      lockfile)))


(defun bundle-source-registry (&key clpmfile with-client-p ignore-missing-releases)
  (with-bundle-session (clpmfile)
    (with-sources-using-installed-only ()
      (let* ((*fetch-repo-automatically* nil)
             (lockfile-pathname (clpmfile-lockfile-pathname clpmfile))
             (*vcs-project-override-fun* (make-vcs-override-fun (clpmfile-pathname clpmfile)))
             lockfile)
        (unless (probe-file lockfile-pathname)
          (error "Lockfile ~A does not exist" lockfile-pathname))
        (setf lockfile (load-lockfile lockfile-pathname))
        (unless ignore-missing-releases
          (let* ((releases (context-releases lockfile))
                 (missing-releases (remove-if #'release-installed-p releases)))
            (when missing-releases
              (error "The following releases are not installed: ~{~S~^, ~}"
                     (mapcar (compose #'project-name #'release-project) missing-releases)))))
        (context-to-asdf-source-registry-form
         lockfile
         :with-client with-client-p
         :ignore-inherited t)))))

(defun bundle-update (&key clpmfile
                        update-projects (validate (constantly t))
                        update-systems)
  (with-bundle-session (clpmfile)
    (let* ((*fetch-repo-automatically* (not (config-value :local)))
           (clpmfile (get-clpmfile clpmfile))
           (lockfile-pathname (clpmfile-lockfile-pathname clpmfile))
           (*vcs-project-override-fun* (make-vcs-override-fun (clpmfile-pathname clpmfile)))
           (lockfile nil)
           (changedp nil))
      (unless (probe-file lockfile-pathname)
        ;; There is no lock file currently. Just fall back to BUNDLE-INSTALL.
        (return-from bundle-update
          (bundle-install :clpmfile clpmfile :validate validate)))
      ;; Load the existing lockfile
      (setf lockfile (load-lockfile lockfile-pathname))
      (unless (config-value :local)
        (dolist (s (context-sources clpmfile))
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
      (setf lockfile (install-requirements (context-requirements clpmfile)
                                           :context lockfile
                                           :validate (lambda (diff)
                                                       (aprog1 (funcall validate diff)
                                                         (setf changedp it)))
                                           :update-projects (or update-projects t)))
      (when changedp
        (save-context lockfile)))))

(defun bundle-exec (command args &key clpmfile
                                   with-client-p)
  "exec(3) (or approximate if system doesn't have exec) a COMMAND in bundle's context

COMMAND must be a string naming the command to run.

ARGS must be a list of strings containing the arguments to pass to the command.

If WITH-CLIENT-P is non-NIL, the clpm-client system is available."
  (unless (stringp command)
    (error "COMMAND must be a string."))
  (with-bundle-session (clpmfile)
    (with-sources-using-installed-only ()
      (let* ((*fetch-repo-automatically* nil)
             (clpmfile-pathname (clpmfile-pathname clpmfile))
             (lockfile-pathname (clpmfile-lockfile-pathname clpmfile))
             (lockfile (load-lockfile lockfile-pathname))
             (cl-source-registry-form (context-to-asdf-source-registry-form lockfile
                                                                            :with-client with-client-p
                                                                            :ignore-inherited t))
             (output-translations (context-output-translations lockfile))
             (installed-system-names (sort (mapcar #'system-name (context-installed-systems lockfile)) #'string<))
             (visible-primary-system-names (sort (context-visible-primary-system-names lockfile) #'string<)))
        (with-standard-io-syntax
          (execvpe command args
                   `(("CL_SOURCE_REGISTRY" . ,(format nil "~S" cl-source-registry-form))
                     ,@(when output-translations
                         `(("ASDF_OUTPUT_TRANSLATIONS" . ,(format nil "~S" output-translations))))
                     ("CLPM_EXEC_INSTALLED_SYSTEMS" . ,(format nil "~S" installed-system-names))
                     ("CLPM_EXEC_VISIBLE_PRIMARY_SYSTEMS" . ,(format nil "~S" visible-primary-system-names))
                     ("CLPM_EXEC_CLPMFILE" . ,(uiop:native-namestring clpmfile-pathname))
                     ("CLPM_EXEC_IGNORE_INHERITED_SOURCE_REGISTRY" . "t"))
                   t))))))
