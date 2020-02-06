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
   :help "The name of the source to install from."
   :reduce #'adopt:last))

(defparameter *option-install-system*
  (adopt:make-option
   :install-system
   :short #\s
   :help "Install a system instead of a project"
   :reduce (constantly t)))

(defparameter *option-install-yes*
  (adopt:make-option
   :install-yes
   :short #\y
   :long "yes"
   :help "Answer yes to all questions"
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
                   *option-install-source*
                   *option-install-system*
                   *option-install-no-deps*
                   *option-install-yes*
                   *option-context*)))

(defun validate-fun (diff)
  (print-context-diff diff *standard-output*)
  (y-or-n-p "Proceed?"))

(define-cli-command (("install") *install-ui*) (args options)
  (let* ((version-string (gethash :install-version options))
         (package-name (first args))
         (source-name (gethash :install-source options))
         (install-system-p (gethash :install-system options))
         (no-deps-p (gethash :install-no-deps options))
         (context-name (or (gethash :context options)
                           "default"))
         (yes-p (gethash :install-yes options)))
    (unless package-name
      (error "A package or system name is required"))

    (log:debug "Installing version ~S of ~:[project~;system~] ~S"
               version-string
               install-system-p
               package-name)
    (install (if install-system-p
                 :system
                 :project)
             package-name
             :version-spec version-string
             :source source-name
             :context context-name
             :no-deps-p no-deps-p
             :validate (if yes-p (constantly t) #'validate-fun))
    t))
