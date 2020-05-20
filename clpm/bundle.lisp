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
          #:clpm/context-queries
          #:clpm/exec
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
  (with-clpm-session ()
    (with-context (lockfile (clpmfile-pathname clpmfile))
      (let ((clpmfile (get-clpmfile clpmfile)))
        (unless (config-value :local)
          (dolist (s (context-sources clpmfile))
            (unless (source-can-lazy-sync-p s)
              (sync-source s))))
        (if no-resolve
            (mapc #'install-release (context-releases lockfile))
            (progn
              ;; Nuke the lockfile's requirements so that we pick up deletions
              ;; from the clpmfile.
              (setf (context-requirements lockfile) nil)
              (setf (context-user-sources lockfile) (context-user-sources clpmfile))
              (setf lockfile (install-requirements (context-requirements clpmfile)
                                                   :context lockfile
                                                   :validate validate
                                                   :update-projects (config-table-keys :bundle :local)
                                                   :save-context-p t)))))
      lockfile)))


(defun bundle-source-registry (&key clpmfile with-client-p)
  (source-registry :with-client-p with-client-p :ignore-inherited-source-registry t
                   :context (clpmfile-pathname clpmfile)))

(defun bundle-update (&key clpmfile
                        update-projects (validate (constantly t))
                        update-systems)
  (with-clpm-session ()
    (with-context (lockfile (clpmfile-pathname clpmfile))
      (let ((clpmfile (get-clpmfile clpmfile)))
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
        ;; Nuke the lockfile's requirements so that we pick up deletions
        ;; from the clpmfile.
        (setf (context-requirements lockfile) nil)
        (setf (context-user-sources lockfile) (context-user-sources clpmfile))
        (install-requirements (context-requirements clpmfile)
                              :context lockfile
                              :validate validate
                              :update-projects (or update-projects t)
                              :save-context-p t)))))

(defun bundle-exec (command args &key clpmfile
                                   with-client-p)
  "exec(3) (or approximate if system doesn't have exec) a COMMAND in bundle's context

COMMAND must be a string naming the command to run.

ARGS must be a list of strings containing the arguments to pass to the command.

If WITH-CLIENT-P is non-NIL, the clpm-client system is available."
  (exec command args :with-client-p with-client-p
                     :context (clpmfile-pathname clpmfile)))
