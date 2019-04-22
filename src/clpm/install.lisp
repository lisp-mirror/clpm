;;;; Basic installation support
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/install
    (:use #:cl
          #:alexandria
          #:clpm/log
          #:clpm/sources/defs
          #:clpm/sources/vcs)
  (:import-from #:puri)
  (:export #:activate-release-globally!
           #:install-release
           #:register-project-path-globally!))

(in-package #:clpm/install)

(setup-logger)

(defun clpm-source-registry.conf.d-pathname ()
  (uiop:xdg-config-home "common-lisp" "source-registry.conf.d" "50-clpm.conf"))

(defun clpm-source-registry.conf.d.meta-pathname ()
  (uiop:xdg-config-home "common-lisp" "source-registry.conf.d" "50-clpm.conf.meta"))

(defun read-current-conf ()
  (let ((pathname (clpm-source-registry.conf.d-pathname)))
    (when (probe-file pathname)
      (uiop:read-file-forms pathname))))

(defun read-current-conf-meta ()
  (let ((pathname (clpm-source-registry.conf.d.meta-pathname)))
    (when (probe-file pathname)
      (uiop:read-file-forms pathname))))

(defun write-conf (conf)
  (with-open-file (s (clpm-source-registry.conf.d-pathname)
                     :direction :output
                     :if-exists :supersede)
    (write-line ";; -*- mode: common-lisp;" s)
    (write-line ";; Autogenerated by clpm do not edit" s)
    (let ((*print-case* :downcase))
      (dolist (entry conf)
        (prin1 entry s)
        (terpri s)))))

(defun write-meta (meta)
  (with-open-file (s (clpm-source-registry.conf.d.meta-pathname)
                     :direction :output
                     :if-exists :supersede)
    (write-line ";; -*- mode: common-lisp;" s)
    (write-line ";; Autogenerated by clpm do not edit" s)
    (let ((*print-case* :downcase))
      (dolist (entry meta)
        (prin1 entry s)
        (terpri s)))))

(defun register-project-path-globally! (project-name path)
  (let* ((path (pathname path))
         (current-config (read-current-conf))
         (current-meta (read-current-conf-meta))
         (old-path (assoc-value current-meta project-name :test 'equalp)))
    (when old-path
      ;; We need to remove the old path from the config file before adding the
      ;; new one.
      (removef current-config old-path :test 'equalp :key 'second))
    ;; Add the new path to both the meta data and the conf file.
    (setf (assoc-value current-meta project-name :test 'equalp) path)
    (push `(:tree ,path) current-config)
    (write-conf current-config)
    (write-meta current-meta)))

(defgeneric activate-release-globally! (release)
  (:documentation
   "Add ~release~ to the global list of installed and activated releases."))

(defgeneric install-release (release &key activate-globally)
  (:documentation "Install a ~release~ and optinally activate it globally."))

(defmethod install-release ((release git-release) &key activate-globally)
  (assert (null activate-globally))
  (ensure-git-release-installed! release))
