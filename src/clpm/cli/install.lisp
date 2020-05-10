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
          #:clpm/log)
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
   :reduce #'adopt:last))

(defparameter *option-install-source*
  (adopt:make-option
   :install-source
   :long "source"
   :parameter "SOURCE-NAME"
   :help "The name of the source to install from"
   :reduce #'adopt:last))

(defparameter *option-install-project*
  (adopt:make-option
   :install-project
   :short #\p
   :help "Install a project instead of a system"
   :reduce (constantly t)))

(defparameter *option-install-no-deps*
  (adopt:make-option
   :install-no-deps
   :short #\n
   :long "no-deps"
   :help "Do not install dependencies"
   :reduce (constantly t)))

(defparameter *option-install-commit*
  (adopt:make-option
   :install-commit
   :short #\c
   :long "commit"
   :parameter "COMMIT"
   :help "Install the project from source control, at the given commit"
   :reduce #'adopt:last))

(defparameter *option-install-branch*
  (adopt:make-option
   :install-branch
   :short #\b
   :long "branch"
   :parameter "BRANCH"
   :help "Install the project from source control, using the given branch"
   :reduce #'adopt:last))

(defparameter *option-install-tag*
  (adopt:make-option
   :install-tag
   :short #\t
   :long "tag"
   :parameter "TAG"
   :help "Install the project from source control, at the given tag"
   :reduce #'adopt:last))

(defparameter *install-ui*
  (adopt:make-interface
   :name "clpm install"
   :summary "Common Lisp Package Manager Install"
   :usage "install [options] PACKAGE-OR-SYSTEM"
   :help "Common Lisp Package Manager"
   :contents (list *group-common*
                   *option-install-version*
                   *option-install-source*
                   *option-install-project*
                   *option-install-no-deps*
                   *option-yes*
                   *option-local*
                   *option-install-branch*
                   *option-install-tag*
                   *option-install-commit*
                   *option-context*
                   *option-output*)))

(defun make-validate-fun (yes-p output)
  (lambda (diff)
    (unless (equal output "sexp")
      ;; We can't print this in a sexp format at the moment.
      (print-context-diff diff *standard-output*))
    (or yes-p (y-or-n-p "Proceed?"))))

(define-cli-command (("install") *install-ui*) (args options)
  (let* ((version-string (gethash :install-version options))
         (name (first args))
         (source-name (gethash :install-source options))
         (install-project-p (gethash :install-project options))
         (no-deps-p (gethash :install-no-deps options))
         (context-name (or (gethash :context options)
                           "default"))
         (yes-p (gethash :yes options))
         (commit (gethash :install-commit options))
         (branch (gethash :install-branch options))
         (tag (gethash :install-tag options))
         (output (gethash :output options)))
    (unless name
      (error "A project or system name is required"))

    (log:debug "Installing version ~S of ~:[system~;project~] ~S"
               version-string
               install-project-p
               name)
    (install (if install-project-p
                 :project
                 :system)
             name
             :version-spec version-string
             :source source-name
             :context context-name
             :no-deps-p no-deps-p
             :commit commit
             :branch branch
             :tag tag
             :validate (make-validate-fun yes-p output)
             :save-context-p t)
    t))
