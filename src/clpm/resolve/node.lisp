;;;; Requirement resolution search node
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/resolve/node
    (:use #:cl
          #:alexandria
          #:clpm/groveler
          #:clpm/log
          #:clpm/source)
  (:export #:copy-node
           #:node
           #:node-activated-releases
           #:node-activated-system-releases
           #:node-add-resolution!
           #:node-add-unresolved-grovel-req!
           #:node-add-unresolved-reqs!
           #:node-child-generator
           #:node-find-project-if-active
           #:node-find-system-if-active
           #:node-groveler
           #:node-load-asd-in-groveler!
           #:node-register-asd-loaded-in-groveler!
           #:node-system-files-pending-groveling
           #:node-unresolved-grovel-reqs
           #:node-unresolved-reqs))

(in-package #:clpm/resolve/node)

(setup-logger)

(defclass node ()
  ((unresolved-reqs
    :initarg :unresolved-reqs
    :initform nil
    :accessor node-unresolved-reqs
    :documentation
    "A list of requirements that have not yet been resolved.")
   (unresolved-grovel-req
    :initarg :unresolved-grovel-reqs
    :initform nil
    :accessor node-unresolved-grovel-reqs
    :documentation
    "A list of requirements that need to be resolved before groveling can be
    attempted again.")
   (activated-releases
    :initarg :activated-releases
    :initform nil
    :accessor node-activated-releases
    :documentation
    "An alist of releases that are activated and the requirements they
    satisfy.")
   (activated-system-releases
    :initarg :activated-system-releases
    :initform nil
    :accessor node-activated-system-releases
    :documentation
    "A list of system releases that are activated.")
   (system-files-pending-groveling
    :initarg :system-files-pending-groveling
    :initform nil
    :accessor node-system-files-pending-groveling
    :documentation
    "An alist that maps system file objects to system names defined by the file
    that need groveling.")
   (grovler-loaded-asds
    :initarg :groveler-loaded-asds
    :initform nil
    :accessor node-groveler-loaded-asds
    :documentation
    "A list of pathnames for system files loaded into the groveler.")

   (groveler
    :initarg :groveler
    :accessor node-groveler
    :documentation
    "A slot used to cache the latest groveler used by this search node or its
    ancestors.")
   (node-child-generator
    :accessor node-child-generator
    :documentation
    "Holds the function used to generate new children of this node"))
  (:documentation
   "Describes the state of the search at any point in time."))

(defun copy-node (node
                  &key
                    (unresolved-reqs nil unresolved-reqs-p)
                    (unresolved-grovel-reqs nil unresolved-grovel-reqs-p)
                    (system-files-pending-groveling nil system-files-pending-groveling-p))
  "Given a search node, return a shallow copy of it."
  (make-instance 'node
                 :unresolved-reqs (if unresolved-reqs-p
                                      unresolved-reqs
                                      (node-unresolved-reqs node))
                 :unresolved-grovel-reqs (if unresolved-grovel-reqs-p
                                             unresolved-grovel-reqs
                                             (node-unresolved-grovel-reqs node))
                 :activated-releases (copy-alist (node-activated-releases node))
                 :activated-system-releases (node-activated-system-releases node)
                 :system-files-pending-groveling (copy-alist
                                                  (if system-files-pending-groveling-p
                                                      system-files-pending-groveling
                                                      (node-system-files-pending-groveling node)))
                 :groveler-loaded-asds (node-groveler-loaded-asds node)
                 :groveler (node-groveler node)))

(defun collapse-system-files-needing-groveling (left right)
  (if (or (eql left t)
          (eql right t))
      t
      (union left right :test #'equalp)))

(defun node-add-resolution! (node release system-releases system-files
                             reason)
  (push reason (assoc-value (node-activated-releases node) release))

  (dolist (sr system-releases)
    (unless (member sr (node-activated-system-releases node))
      (push sr (node-activated-system-releases node))))

  (dolist (sf-desc system-files)
    (setf (assoc-value (node-system-files-pending-groveling node)
                       (car sf-desc))
          (collapse-system-files-needing-groveling
           (assoc-value (node-system-files-pending-groveling node)
                        (car sf-desc))
           (cdr sf-desc))))

  node)

(defun node-add-unresolved-reqs! (node new-unresolved-reqs)
  (setf (node-unresolved-reqs node)
        (append new-unresolved-reqs
                (node-unresolved-reqs node))))

(defun node-add-unresolved-grovel-req! (node new-unresolved-req)
  (push new-unresolved-req (node-unresolved-grovel-reqs node)))

(defun node-find-project-if-active (node project-name)
  "Find a release object for a project, if it is activated in the search node."
  (car (find project-name (node-activated-releases node)
             :test #'string-equal
             :key (compose #'project-name #'release-project #'car))))

(defun node-find-system-if-active (node system)
  "Find a system object, if it is activated in the search node."
  (find system (node-activated-system-releases node)
        :test #'string-equal
        :key (compose #'system-name #'system-release-system)))

(defun node-load-asd-in-groveler! (node asd-pathname)
  "Load the asd file into the groveler. If the current groveler is
incompatible, a new one is created."
  (let* ((asd-pathname (uiop:ensure-absolute-pathname asd-pathname))
         (asd-namestring (namestring asd-pathname)))
    (log:debug "Attempting to add ~S to groveler" asd-pathname)
    (cond
      ((set-equal (node-groveler-loaded-asds node)
                  (groveler-loaded-asds (node-groveler node))
                  :test #'equal)
       ;; This groveler is compatible, load the asd.
       (groveler-load-asd! (node-groveler node) asd-pathname)
       (push asd-namestring (node-groveler-loaded-asds node)))
      ((set-equal (list* asd-namestring (node-groveler-loaded-asds node))
                  (groveler-loaded-asds (node-groveler node))
                  :test #'equal)
       ;; This groveler already has the target system loaded. Don't need to do
       ;; anything.
       (push asd-namestring (node-groveler-loaded-asds node)))
      (t
       ;; Grovelers are incompatible. Need to launch a new one.
       (log:debug "Starting new groveler.~%search node asds: ~S~%groveler asds:~S"
                  (node-groveler-loaded-asds node)
                  (groveler-loaded-asds (node-groveler node)))
       (setf (node-groveler node) (make-groveler))
       (dolist (f (reverse (node-groveler-loaded-asds node)))
         (groveler-load-asd! (node-groveler node) f))
       (groveler-load-asd! (node-groveler node) asd-pathname)
       (push asd-namestring (node-groveler-loaded-asds node))))))

(defun node-register-asd-loaded-in-groveler! (node asd-pathname)
  (pushnew (namestring asd-pathname) (node-groveler-loaded-asds node)
           :test #'equal))
