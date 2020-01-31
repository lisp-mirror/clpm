;;;; clpm install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/install
    (:use #:cl
          #:clpm/cli/common-args
          #:clpm/cli/subcommands
          #:clpm/context
          #:clpm/install
          ;; #:clpm/interface
          #:clpm/log
          #:clpm/requirement
          #:clpm/resolve-2
          #:clpm/version-strings)
  (:import-from #:adopt))

(in-package #:clpm/cli/install)

(setup-logger)

(adopt:define-string *help-text*
  "Install a package or system, optionally making it available in an ASDF
  context. This command first computes a list of releases that need to be
  installed to satisfy")

(adopt:define-string *manual-text*
  "Install a package or system, optionally making it available in an ASDF
  context. This command first computes a list of releases that need to be
  installed to satisfy the user's constraints (provided with -v and -n). When
  computing the releases, preference is given to reusing already installed
  releases instead of upgrading them. Additionally, snapshots will not be
  installed unless they are the only version of the project that is available or
  they are allowed.")

(defparameter *option-install-version*
  (adopt:make-option
   :install-version
   :short #\v
   :help "The version to install"
   :parameter "VERSION"
   :initial-value ">=0"
   :reduce #'adopt:last))

(defparameter *option-install-system*
  (adopt:make-option
   :install-system
   :short #\s
   :help "Install a system instead of a package"
   :reduce (constantly t)))

(defparameter *option-install-no-deps*
  (adopt:make-option
   :install-no-deps
   :short #\n
   :long "no-deps"
   :help "Do not install dependencies"
   :reduce (constantly t)))

(defparameter *install-ui*
  (adopt:make-interface
   :name "clpm install"
   :summary "Common Lisp Package Manager Install"
   :usage "install [options] PACKAGE-OR-SYSTEM"
   :help "Common Lisp Package Manager"
   :contents (list *group-common*
                   *option-install-version*
                   *option-install-system*
                   *option-install-no-deps*
                   *option-context*)))

(define-cli-command (("install") *install-ui*) (args options)
  (let* ((version-string (gethash :install-version options))
         (package-name (first args))
         (install-system-p (gethash :install-system options))
         (no-deps-p (gethash :install-no-deps options))
         (context-name (or (gethash :context options)
                           "default"))
         (original-context (load-global-context context-name nil))
         (new-context (copy-context original-context)))
    (unless package-name
      (error "A package or system name is required"))
    (log:debug "Installing version ~S of ~:[project~;system~] ~S"
               version-string
               install-system-p
               package-name)

    (let ((req (if install-system-p
                   (make-instance 'system-requirement
                                  :name package-name
                                  :version-spec (parse-version-specifier version-string)
                                  :no-deps-p no-deps-p
                                  :why t)
                   (make-instance 'project-requirement
                                  :name package-name
                                  :version-spec (parse-version-specifier version-string)
                                  :no-deps-p no-deps-p
                                  :why t))))
      (push req (context-requirements new-context))
      (let ((new-context (resolve-requirements new-context)))
        (mapc #'install-release (context-releases new-context))
        (print (format nil "~S" (context-to-asdf-source-registry new-context)))
        (terpri)
        (save-global-context new-context)
        ;;(format t "~%~%~S~%" (multiple-value-list (context-diff original-context new-context)))
        )
      ;;(serialize-context-to-stream (resolve-requirements new-context) *standard-output*)
      ;; (format t "~A~%" )
      ;; (format t "~S~S~S~S")
      ;; (write-context-to-stream )
      )

    ;; (if install-system-p
    ;;     (install-project package-name (parse-version-specifier version-string) :no-deps-p no-deps-p)
    ;;     (install-system package-name (parse-version-specifier version-string) :no-deps-p no-deps-p))
    t))
