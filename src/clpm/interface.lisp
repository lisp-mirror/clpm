;;;; Common Lisp Package Manager - CLPM.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/interface
    (:nicknames #:clpm)
  (:use #:cl
        #:alexandria
        #:clpm/install
        #:clpm/log
        #:clpm/requirement
        #:clpm/resolve
        #:clpm/source)
  (:export #:install-project
           #:install-system
           #:sources
           #:sync-source))

(in-package #:clpm/interface)

(setup-logger)

(defun install-project (project-name version-spec &key no-deps-p)
  (let* ((reqs (mapcar (lambda (vs)
                         (make-instance 'project-requirement
                                        :version-spec vs
                                        :name project-name))
                       version-spec))
         (releases-to-install (resolve-requirements reqs (sources) :no-deps no-deps-p)))
    (log:debug "Requirements: ~S" reqs)
    (log:debug "Releases: ~S" releases-to-install)
    (mapc (rcurry #'install-release :activate-globally t) releases-to-install)))

(defun install-system (system-name version-spec &key no-deps-p)
  (let* ((reqs (mapcar (lambda (vs)
                         (make-instance 'system-requirement
                                        :version-spec vs
                                        :name system-name))
                       version-spec))
         (releases-to-install (resolve-requirements reqs (sources) :no-deps no-deps-p)))
    (log:debug "Requirements: ~S" reqs)
    (log:debug "Releases: ~S" releases-to-install)
    (mapc (rcurry #'install-release :activate-globally t) releases-to-install)))
