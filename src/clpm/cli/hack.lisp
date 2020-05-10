;;;; clpm hack
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/hack
    (:use #:cl
          #:clpm/cli/common-args
          #:clpm/cli/interface-defs
          #:clpm/log
          #:clpm/repos
          #:clpm/source)
  (:import-from #:adopt))

(in-package #:clpm/cli/hack)

(setup-logger)

(adopt:define-string *help-text*
  "Given a system or project name, clone its repository to start hacking on it.")

(defparameter *option-hack-source*
  (adopt:make-option
   :hack-source
   :long "source"
   :parameter "SOURCE-NAME"
   :help "The name of the source to get repo data from"
   :reduce #'adopt:last))

(defparameter *option-hack-project*
  (adopt:make-option
   :hack-project
   :short #\p
   :help "If provided, the argument is assumed to be a project name instead of a system name"
   :reduce (constantly t)))

(defparameter *option-hack-local*
  (adopt:make-option
   :hack-local
   :long "local"
   :help "If provided, the repo will be cloned from CLPM's cached version of the repo"
   :reduce (constantly t)))

(defparameter *hack-ui*
  (adopt:make-interface
   :name "clpm hack"
   :summary "Common Lisp Package Manager Hack"
   :usage "hack [options] SYSTEM-OR-PROJECT"
   :help *help-text*
   :contents (list *group-common*
                   *option-hack-source*
                   *option-hack-project*
                   *option-hack-local*)))

(defun find-project-source (project-name sources)
  (loop
    :for source :in sources
    :when (source-project source project-name nil)
      :do (return source)))

(defun find-system-source (system-name sources)
  (loop
    :for source :in sources
    :when (source-system source system-name nil)
      :do (return source)))

(define-cli-command (("hack") *hack-ui*) (args options)
  (let* ((name (first args))
         (dir-name (second args))
         (dir-pathname)
         (source-name (gethash :hack-source options))
         (hack-project-p (gethash :hack-project options))
         (sources (sources))
         (source (when source-name (find source-name sources :test #'equal :key #'source-name)))
         project)
    (unless name
      (error "A project or system name is required"))

    (if hack-project-p
        (setf source (find-project-source name sources))
        (setf source (find-system-source name sources)))

    (unless source
      (error "unable to find a source to provide the repo!"))

    (if hack-project-p
        (setf project (source-project source name))
        ;; TODO: Need to add something like system-projects.
        (setf project (release-project (first (system-releases (source-system source name))))))

    (unless dir-name
      (setf dir-name (project-name project)))
    (setf dir-pathname (uiop:ensure-directory-pathname (merge-pathnames dir-name)))

    (when (probe-file dir-pathname)
      (error "Directory already exists. Aborting."))

    (let ((repo (project-repo project)))
      (clone-repo! repo :pathname dir-pathname :not-bare-p t))

    t))
