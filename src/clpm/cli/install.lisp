;;;; clpm install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/install
    (:use #:cl
          #:clpm/cli/common-args
          #:clpm/cli/subcommands
          #:clpm/interface
          #:clpm/log
          #:clpm/version-strings)
  (:import-from #:adopt))

(in-package #:clpm/cli/install)

(setup-logger)

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
                   *option-install-no-deps*)))


(define-cli-command (("install") *install-ui*) (args options)
  (let* ((version-string (gethash :install-version options))
         (package-name (first args))
         (install-system-p (gethash :install-system options))
         (no-deps-p (gethash :install-no-deps options)))
    (unless package-name
      (error "A package or system name is required"))
    (log:debug "Installing version ~S of ~:[project~;system~] ~S"
               version-string
               install-system-p
               package-name)
    (if install-system-p
        (install-project package-name (parse-version-specifier version-string) :no-deps-p no-deps-p)
        (install-system package-name (parse-version-specifier version-string) :no-deps-p no-deps-p))
    t))
