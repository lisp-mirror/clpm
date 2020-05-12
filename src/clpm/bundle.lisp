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
          #:clpm/install
          #:clpm/log
          #:clpm/repos
          #:clpm/resolve
          #:clpm/source
          #:do-urlencode)
  (:export #:bundle-clpmfile-pathname
           #:bundle-init
           #:bundle-install
           #:bundle-output-translations
           #:bundle-source-registry
           #:bundle-update
           #:with-bundle-default-pathname-defaults
           #:with-bundle-local-config))

(in-package #:clpm/bundle)

(setup-logger)

(defun bundle-clpmfile-pathname ()
  (merge-pathnames (config-value :bundle :clpmfile)
                   (uiop:getcwd)))

(defun call-with-bundle-local-config (thunk pn)
  "Call THUNK in a dynamic environment that has the local config for the
clpmfile located at PN added to the config."
  (with-config-file-source-added ((merge-pathnames ".clpm/bundle.conf"
                                                   (uiop:pathname-directory-pathname pn)))
    (funcall thunk)))

(defmacro with-bundle-local-config ((&optional (clpmfile-pn '(bundle-clpmfile-pathname))) &body body)
  `(call-with-bundle-local-config (lambda () ,@body) ,clpmfile-pn))

(defun call-with-bundle-default-pathname-defaults (thunk pn)
  "Call THUNK in a dynamic environment that has *DEFAULT-PATHNAME-DEFAULTS*
bound to PN's folder."
  (let ((*default-pathname-defaults* (uiop:pathname-directory-pathname pn)))
    (funcall thunk)))

(defmacro with-bundle-default-pathname-defaults ((&optional (clpmfile-pn '(bundle-clpmfile-pathname)))
                                                 &body body)
  `(call-with-bundle-default-pathname-defaults (lambda () ,@body) ,clpmfile-pn))

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

(defun bundle-init (clpmfile-pathname &key (if-exists :error) asds)
  (with-standard-io-syntax
    (let ((*print-case* :downcase))
      (with-open-file (s clpmfile-pathname :direction :output :if-exists if-exists)
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
  clpmfile-pathname)

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
    lockfile))


(defun bundle-output-translations (clpmfile-designator)
  (let ((clpmfile-pathname (clpmfile-pathname (get-clpmfile clpmfile-designator))))
    (case (config-value :bundle :output-translation)
      ((t)
       `(:output-translations
         :ignore-inherited-configuration
         (t (:root ,@(rest (pathname-directory (clpm-cache-pathname '("bundle" "fasl-cache")
                                                                    :ensure-directory t)))
                   ,(urlencode (format nil "~{~A~^/~}" (rest (pathname-directory clpmfile-pathname))))
                   :implementation :**/ :*.*.*))))
      (:local
       `(:output-translations
         :ignore-inherited-configuration
         (t (,(uiop:pathname-directory-pathname clpmfile-pathname) ".clpm" "fasl-cache"
             :implementation :**/ :*.*.*))))
      (t
       nil))))

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
     :extra-forms
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
