;;;; clpm install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/install
    (:use #:cl
          #:alexandria
          #:clpm/cli/common-args
          #:clpm/cli/subcommands
          #:clpm/install
          #:clpm/log
          #:clpm/requirement
          #:clpm/resolve
          #:clpm/source
          #:clpm/version-strings
          )
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
  (let* ((version-string (gethash :install-verison options))
         (package-name (first args))
         (install-system-p (gethash :install-system options))
         (no-deps-p (gethash :install-no-deps options)))
    (unless package-name
      (error "A package or system name is required"))
    (log:debug "Installing version ~S of ~:[project~;system~] ~S"
               version-string
               install-system-p
               package-name)
    (let* ((sources (load-sources))
           (version-spec (parse-version-specifier version-string))
           (reqs (mapcar (lambda (vs)
                           (make-instance (if install-system-p
                                              'system-requirement
                                              'project-requirement)
                                          :version-spec vs
                                          :name package-name))
                         version-spec))
           (releases-to-install (resolve-requirements reqs sources :no-deps no-deps-p)))
      (log:debug "Reqs: ~S" reqs)
      (log:debug "releases: ~S" releases-to-install)
      (mapc (rcurry #'install-release :activate-globally t) releases-to-install)
      ;; (when (and install-system-p
      ;;            (getopt :short-name "x"))
      ;;   (let* ((releases (reverse releases-to-install))
      ;;          (requested-system-release (release/system-release (first releases) package-name))
      ;;          (system-releases (list* requested-system-release
      ;;                                  (mapcan #'release/system-releases releases))))
      ;;     (format t "~S~%" (remove-duplicates
      ;;                       (mapcar #'system-release/absolute-asd-pathname system-releases)
      ;;                       :from-end t
      ;;                       :test 'uiop:pathname-equal))))
      )
    t))
