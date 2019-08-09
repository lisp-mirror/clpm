;;;; This file defines the various requirements used during dependency
;;;; resolution.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/requirement
    (:use #:cl)
  (:export #:convert-asd-system-spec-to-req
           #:fs-system-file-requirement
           #:fs-system-requirement
           #:project-requirement
           #:requirement/branch
           #:requirement/commit
           #:requirement/name
           #:requirement/pathname
           #:requirement/repo
           #:requirement/source
           #:requirement/systems
           #:requirement/system-files
           #:requirement/tag
           #:requirement/version-spec
           #:system-requirement
           #:vcs-project-requirement
           #:vcs-requirement))

(in-package #:clpm/requirement)

(defclass requirement ()
  ((name
    :initarg :name
    :reader requirement/name
    :documentation
    "The name of the thing being required. Typically a string, but may be other
objects (like pathnames).")
   (source
    :initarg :source
    :initform nil
    :reader requirement/source
    :documentation
    "If non-NIL, the requirement must be satisfied using releases provided by
this source."))
  (:documentation
   "Base class for all requirements."))

(defclass versioned-requirement (requirement)
  ((version-spec
    :initarg :version-spec
    :initform nil
    :reader requirement/version-spec
    :documentation
    "The version specifier that must be satisfied."))
  (:documentation
   "A requirement that can specify a version that needs to be satisfied."))

(defclass vcs-requirement (requirement)
  ((repo
    :initarg :repo
    :initform (error "Repo must be provided")
    :reader requirement/repo
    :documentation
    "The repo object describing the upstream repo for this requirement.")
   (branch
    :initarg :branch
    :initform nil
    :reader requirement/branch
    :documentation
    "If non-NIL, string naming the required branch.")
   (commit
    :initarg :commit
    :initform nil
    :reader requirement/commit
    :documentation
    "If non-NIL, string naming the required commit.")
   (tag
    :initarg :tag
    :initform nil
    :reader requirement/tag
    :documentation
    "If non-NIL, string naming the required tag."))
  (:documentation
   "A requirement on something that must be fetched from a vcs."))

(defclass project-requirement (versioned-requirement)
  ()
  (:documentation
   "A requirement on the presence a project."))

(defclass system-requirement (versioned-requirement)
  ()
  (:documentation
   "A requirement on the presence of a system."))

(defclass vcs-project-requirement (vcs-requirement)
  ((systems
    :initarg :systems
    :initform nil
    :reader requirement/systems
    :documentation
    "A list of system names to require.")
   (system-files
    :initarg :system-files
    :initform nil
    :reader requirement/system-files
    :documentation
    "A list of system files to require."))
  (:documentation
   "A requirement on the presence of a project retrieved from a VCS. If both
systems and system-files are NIL, requires all systems in all asd files defined
in the project."))

(defclass fs-system-requirement (requirement)
  ((pathname
    :initarg :pathname
    :accessor requirement/pathname
    :documentation
    "The pathname to the asd file containing this system."))
  (:documentation
   "Requirement stating that a system must come from an asd file located on the
local filesystem."))

(defclass fs-system-file-requirement (requirement)
  ()
  (:documentation
   "Requirement stating that all systems from an asd file located on the local
filesystem must be included. NAME is the pathname to the asd file."))


(defmethod print-object ((req requirement) stream)
  (print-unreadable-object (req stream :type t :identity t)
    (format stream "~S ~S" :name (requirement/name req))))

(defmethod print-object ((req versioned-requirement) stream)
  (print-unreadable-object (req stream :type t :identity t)
    (format stream "~S ~S" :name (requirement/name req))
    (when (requirement/version-spec req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (format stream "~S ~S" :version-spec (requirement/version-spec req)))))

(defgeneric convert-asd-system-spec-to-req (dep)
  (:documentation
   "Given an ASD dependency specification, return the requirement object it
represents."))

(defmethod convert-asd-system-spec-to-req ((dep string))
  "A string simply maps to a system requirement."
  (make-instance 'system-requirement
                 :name (string-downcase dep)))

(defmethod convert-asd-system-spec-to-req ((dep list))
  "Handles version and feature specifications."
  (ecase (first dep)
    (:version
     (make-instance 'system-requirement
                    :name (string-downcase (second dep))
                    :version-spec `(>= . ,(third dep))))
    (:feature
     (convert-asd-system-spec-to-req (third dep)))))
