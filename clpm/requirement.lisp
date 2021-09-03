;;;; This file defines the various requirements used during dependency
;;;; resolution.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/requirement
    (:use #:cl
          #:anaphora
          #:clpm/sources/defs)
  (:export #:convert-asd-system-spec-to-req
           #:fs-system-file-requirement
           #:fs-system-requirement
           #:project-requirement
           #:requirement-branch
           #:requirement-commit
           #:requirement-name
           #:requirement-no-deps-p
           #:requirement-pathname
           #:requirement-ref
           #:requirement-repo
           #:requirement-source
           #:requirement-systems
           #:requirement-system-files
           #:requirement-tag
           #:requirement-to-plist
           #:requirement-version-spec
           #:requirement-why
           #:system-requirement
           #:vcs-project-requirement
           #:vcs-requirement
           #:versioned-requirement))

(in-package #:clpm/requirement)

(defclass requirement ()
  ((name
    :initarg :name
    :reader requirement-name
    :documentation
    "The name of the thing being required. Typically a string, but may be other
objects (like pathnames).")
   (source
    :initarg :source
    :initform nil
    :accessor requirement-source
    :documentation
    "If non-NIL, the requirement must be satisfied using releases provided by
this source.")
   (no-deps-p
    :initarg :no-deps-p
    :initform nil
    :accessor requirement-no-deps-p
    :documentation
    "If non-NIL, the requirement's dependencies do not need to be satisfied.")
   (why
    :initarg :why
    :initform nil
    :accessor requirement-why
    :documentation
    "The reason this requirement was generated."))
  (:documentation
   "Base class for all requirements."))

(defclass versioned-requirement (requirement)
  ((version-spec
    :initarg :version-spec
    :initform nil
    :accessor requirement-version-spec
    :documentation
    "The version specifier that must be satisfied."))
  (:documentation
   "A requirement that can specify a version that needs to be satisfied."))

(defclass vcs-requirement (requirement)
  ((repo
    :initarg :repo
    :initform nil
    :reader requirement-repo
    :documentation
    "The repo object describing the upstream repo for this requirement.")
   (ref
    :initarg :ref
    :initform nil
    :reader requirement-ref
    :Documentation
    "The ref to install.")
   (branch
    :initarg :branch
    :initform nil
    :reader requirement-branch
    :documentation
    "If non-NIL, string naming the required branch.")
   (commit
    :initarg :commit
    :initform nil
    :reader requirement-commit
    :documentation
    "If non-NIL, string naming the required commit.")
   (tag
    :initarg :tag
    :initform nil
    :reader requirement-tag
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
    :reader requirement-systems
    :documentation
    "A list of system names to require.")
   (system-files
    :initarg :system-files
    :initform nil
    :reader requirement-system-files
    :documentation
    "A list of system files to require."))
  (:documentation
   "A requirement on the presence of a project retrieved from a VCS. If both
systems and system-files are NIL, requires all systems in all asd files defined
in the project."))

(defmethod initialize-instance :after ((self vcs-project-requirement) &rest initargs &key commit)
  (assert (or (null commit) (stringp commit))))

(defclass fs-system-requirement (requirement)
  ((pathname
    :initarg :pathname
    :accessor requirement-pathname
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
  (pprint-logical-block (stream nil)
    (print-unreadable-object (req stream :type t :identity t)
      (pprint-newline :linear stream)
      (prin1 :name stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (write (requirement-name req) :stream stream)
      ;; version
      (when (and (typep req 'versioned-requirement)
                 (requirement-version-spec req))
        (write-char #\Space stream)
        (pprint-newline :linear stream)
        (prin1 :version stream)
        (write-char #\Space stream)
        (pprint-newline :miser stream)
        (write (requirement-version-spec req) :stream stream))
      ;; why
      (when (requirement-why req)
        (write-char #\Space stream)
        (pprint-newline :linear stream)
        (prin1 :why stream)
        (write-char #\Space stream)
        (pprint-newline :miser stream)
        (write (requirement-why req) :stream stream))
      ;; ID
      (pprint-newline :linear stream))))

(defgeneric convert-asd-system-spec-to-req (dep &key why)
  (:documentation
   "Given an ASD dependency specification, return the requirement object it
represents."))

(defmethod convert-asd-system-spec-to-req ((dep string) &key why)
  "A string simply maps to a system requirement."
  (make-instance 'system-requirement
                 :name (string-downcase dep)
                 :why why))

(defmethod convert-asd-system-spec-to-req ((dep list) &key why)
  "Handles version and feature specifications."
  (ecase (first dep)
    (:version
     (make-instance 'system-requirement
                    :name (string-downcase (second dep))
                    :version-spec `((>= ,(third dep)))
                    :why why))
    (:feature
     (convert-asd-system-spec-to-req (third dep) :why why))))

(defgeneric requirement-to-plist (req))

;; TODO: context version 0.4, get rid of :implicit-vcs.
(defun backward-compatible-source-name (source)
  (typecase source
    (clpm-source
     ;; HACK to avoid circular dep.
     (if (string-equal 'vcs-source (class-name (class-of source)))
         :implicit-vcs
         (source-name source)))
    (t
     source)))

(defmethod requirement-to-plist ((req versioned-requirement))
  (append
   (list :name (requirement-name req))
   (awhen (requirement-version-spec req)
     (list :version it))
   (awhen (requirement-source req)
     (list :source (backward-compatible-source-name it)))
   (awhen (requirement-no-deps-p req)
     (list :no-deps-p it))))

(defmethod requirement-to-plist ((req vcs-project-requirement))
  (append
   (list :name (requirement-name req))
   (awhen (requirement-branch req)
     (list :branch it))
   (awhen (requirement-commit req)
     (list :commit it))
   (awhen (requirement-tag req)
     (list :tag it))
   (awhen (requirement-ref req)
     (list :ref it))
   (awhen (requirement-source req)
     (list :source (backward-compatible-source-name it)))
   (awhen (requirement-no-deps-p req)
     (list :no-deps-p it))))

(defmethod requirement-to-plist ((req fs-system-requirement))
  (append
   (list :pathname (namestring (requirement-pathname req))
         :name (requirement-name req))
   (awhen (requirement-no-deps-p req)
     (list :no-deps-p it))))

(defmethod requirement-to-plist ((req fs-system-file-requirement))
  (append
   (list :name (namestring (requirement-name req)))
   (awhen (requirement-no-deps-p req)
     (list :no-deps-p it))))
