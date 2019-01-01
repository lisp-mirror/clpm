;;;; Install CLI

(uiop:define-package #:clpm/cli/install
    (:use #:cl
          #:alexandria
          #:clpm/cli/entry
          #:clpm/install
          #:clpm/log
          #:clpm/requirement
          #:clpm/resolve
          #:clpm/source
          #:clpm/version-strings)
  (:import-from #:net.didierverna.clon
                #:defsynopsis
                #:make-context
                #:getopt
                #:remainder
                #:help))

(in-package #:clpm/cli/install)

(setup-logger)

(defparameter *synopsis*
  (defsynopsis (:postfix "[PACAKGE-OR-SYSTEM]"
                :make-default nil)
    (text :contents "Install a package")
    (stropt :argument-name "VERSION"
            :short-name "v"
            :default-value ">=0"
            :description "The version of the package to install")
    (flag :short-name "s"
          :description "Install a system instead of a package")
    (flag :short-name "n"
          :long-name "no-deps"
          :description "Do not install dependencies.")
    (path :argument-name "PROJECT-PATH"
          :short-name "p"
          :long-name "project")
    *common-arguments*))

(define-cli-entry install (*synopsis*)
  ;; Unpack the command line arguments.
  (let* ((version-string (getopt :short-name "v"))
         (remainder (remainder))
         (package-name (first remainder))
         (install-system-p (getopt :short-name "s"))
         (no-deps-p (getopt :short-name "n")))
    (log:info "Installing version ~S of package ~S" version-string package-name)
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

      (log:info "Installing version ~S of package ~S" version-spec package-name)
      (log:info "Reqs: ~S" reqs)
      (log:info "releases: ~S" releases-to-install)
      (mapc (rcurry #'install-release :activate-globally nil) releases-to-install))
    t))
