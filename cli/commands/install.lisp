;;;; clpm install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/install
    (:use #:cl
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt)
  (:import-from #:clpm))

(in-package #:clpm-cli/commands/install)

(adopt:define-string *help-text*
  "Install systems or projects into a context.")

(adopt:define-string *manual-text*
  "Install systems or projects into a context. This command first computes a
list of releases that need to be installed to satisfy the user's constraints and
then prompts the user to approve before taking action. When computing the
releases, preference is given to reusing already installed releases instead of
upgrading them.

Constraints can be placed on the version to install, the ref to install (from a
VCS), or the source to install from. These constraints can be specified
once (using -v, --source, or -r) in which case they are used as the defaults, or
they can be provided as part of the dependency specifier (which takes precedence
over the flags).

Version constraints consist of a symbol (one of =, !=, >=, >, <=, <) followed by
a version string. If no symbol is provided, = is assumed. Multiple version
constraints can be conjoined by separating them with a comma.

A project or system specifier consists of the following pieces, in this order:

+ The project or system name (required).

+ A colon (:) followed by a version constraint (optional, mutually exclusive
with ref specifier).

+ An at sign (@) followed by a VCS ref specifier (optional, mutually exclusive
with version constraint).

+ Two colons (::) followed by a source name (optional).")

(defparameter *option-install-version*
  (adopt:make-option
   :install-version
   :short #\v
   :help "A version constraint applied to all objects (unless overridden)"
   :parameter "VERSION"
   :reduce #'adopt:last))

(defparameter *option-install-source*
  (adopt:make-option
   :install-source
   :long "source"
   :parameter "SOURCE-NAME"
   :help "The name of the source to install from"
   :reduce #'adopt:last))

(defparameter *option-install-asd*
  (adopt:make-option
   :install-asds
   :long "asd"
   :parameter "ASD-PATH"
   :help "Install an .asd file into the context"
   :reduce (adopt:flip #'cons)))

(defparameter *option-install-project*
  (adopt:make-option
   :install-projects
   :short #\p
   :long "project"
   :parameter "PROJECT-SPECIFIER"
   :help "Install a project instead of a system"
   :reduce (adopt:flip #'cons)))

(defparameter *option-install-no-deps*
  (adopt:make-option
   :install-no-deps
   :short #\n
   :long "no-deps"
   :help "Do not install dependencies"
   :reduce (constantly t)))

(defparameter *option-install-ref*
  (adopt:make-option
   :install-ref
   :short #\r
   :long "ref"
   :parameter "REF"
   :help "Install the project from source control, at the given ref"
   :reduce #'adopt:last))

(defparameter *install-ui*
  (adopt:make-interface
   :name "clpm install"
   :summary "Common Lisp Project Manager Install"
   :usage "install [options] SYSTEM-SPECIFIER*"
   :help *help-text*
   :manual *manual-text*
   :contents (list *group-common*
                   *option-install-version*
                   *option-install-source*
                   *option-install-project*
                   *option-install-asd*
                   *option-install-no-deps*
                   *option-yes*
                   *option-local*
                   *option-install-ref*
                   *option-context*)))

(define-cli-command (("install") *install-ui*) (args options)
  (let* ((version-string (gethash :install-version options))
         (system-specifiers args)
         (project-specifiers (gethash :install-projects options))
         (asd-files (gethash :install-asds options))
         (source-name (gethash :install-source options))
         (no-deps-p (gethash :install-no-deps options))
         (yes-p (gethash :yes options))
         (ref (gethash :install-ref options)))
    (clpm:install :projects project-specifiers
                  :systems system-specifiers
                  :asds (mapcar #'merge-pathnames asd-files)
                  :version version-string
                  :source source-name
                  :no-deps-p no-deps-p
                  :ref ref
                  :validate (make-diff-validate-fun :yesp yes-p)
                  :save-context-p t)
    t))
