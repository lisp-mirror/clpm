;;;; Source that is located on the file system. Represents a single .asd file.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

;; * Define package
(uiop:define-package #:clpm/sources/fs
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/groveler
          #:clpm/install/defs
          #:clpm/requirement
          #:clpm/session
          #:clpm/sources/defs
          #:clpm/sources/dotted-versioned-project
          #:clpm/utils)
  (:export #:fs-source))

(in-package #:clpm/sources/fs)


;; * Source

(defclass fs-source (clpm-source)
  ((name
    :initarg :name
    :accessor source-name)
   (project
    :accessor fs-source-project
    :documentation "The single project for the source. Has the same name as the
    source.")
   (project-release
    :accessor fs-source-project-release)
   (system-file
    :accessor fs-source-system-file
    :documentation "The single system file for the source. Has the same name as
    the source.")
   (systems-by-name
    :initform (make-hash-table :test 'equalp)
    :accessor fs-source-systems-by-name
    :documentation "A hash table that maps system names to fs-system objects."))
  (:documentation
   "A source that contains systems located in a single .asd file on the file
system. Contains a single project and release (:NEWEST)."))

(defmethod make-source ((type (eql 'fs-source)) &rest initargs
                        &key name)
  (setf name (namestring name))
  (with-clpm-session (:key `(make-source ,type ,name))
    (apply #'make-instance
           type
           initargs)))

(defmethod initialize-instance :after ((source fs-source) &key name)
  "Construct the singleton project and release."
  (setf (fs-source-project source)
        (make-instance 'fs-project :source source))
  (setf (fs-source-project-release source)
        (make-instance 'fs-release
                       :version :newest
                       :source source))
  (setf (fs-source-system-file source)
        (make-instance 'fs-system-file
                       :enough-namestring name
                       :source source)))

(defmethod source-can-lazy-sync-p ((source fs-source))
  t)

(defmethod source-project ((source fs-source) project-name &optional (error t))
  (if (equal project-name (source-name source))
      (fs-source-project source)
      (when error
        (error 'source-missing-project
               :source source
               :project-name project-name))))

(defmethod source-system ((source fs-source) system-name &optional (error t))
  "If this system already exists, return it. Otherwise see if we have an asd
file with the same primary name and construct a new system for it."
  (unless (gethash system-name (fs-source-systems-by-name source))
    ;; We haven't seen this particular system before. See if we could plausibly
    ;; contain it.
    (when (equal (pathname-name (source-name source))
                 (asdf:primary-system-name system-name))
      ;; We can plausibly contain it.
      (let* ((system (make-instance 'fs-system
                                    :name system-name
                                    :source source))
             (system-release (make-instance 'fs-system-release
                                            :source source
                                            :system system)))
        (setf (fs-system-system-release system) system-release)
        (setf (gethash system-name (fs-source-systems-by-name source))
              system))))
  (or (gethash system-name (fs-source-systems-by-name source))
      (when error
        (error 'source-missing-system
               :source source
               :system-name system-name))))

(defmethod source-to-form ((source fs-source))
  `(:implicit-file
    :type :file-system
    :system-files (,(source-name source))))

(defmethod sync-source ((source fs-source))
  nil)


;; * Project

(defclass fs-project (clpm-project)
  ((source
    :initarg :source
    :reader project-source))
  (:documentation "A project on the filesystem. Contains a singleton
release (that is constructed by the source.)"))

(defmethod project-name ((project fs-project))
  (source-name (project-source project)))

(defmethod project-release ((project fs-project) version-string &optional (error t))
  "If the version is :newest, return our singleton release, otherwise nil."
  (cond
    ((equal version-string :newest)
     (fs-source-project-release (project-source project)))
    (error
     (error 'project-missing-version
            :source (project-source project)
            :project project
            :version version-string))
    (t nil)))

(defmethod project-releases ((project fs-project))
  (list (fs-source-project-release (project-source project))))


;; * Release

(defclass fs-release (clpm-release)
  ((source
    :initarg :source
    :reader release-source)
   (version
    :initarg :version
    :reader release-version))
  (:documentation "A release on the filesystem. Each fs-source has one instance
of this created upon instantiation."))

(defmethod release-project ((release fs-release))
  (fs-source-project (release-source release)))

(defmethod release-system-file ((release fs-release) system-file-namestring)
  "Look at the fs-source to get the system file."
  (fs-source-system-file (release-source release)))

(defmethod release-system-files ((release fs-release))
  "Projects (and releases) correspond to a single system file."
  (list (fs-source-system-file (release-source release))))

(defmethod release-system-release ((release fs-release) system-name &optional (error t))
  "Get the system object from the source and look up its singleton system
release."
  (let* ((source (release-source release))
         (system (source-system source system-name error)))
    (fs-system-system-release system)))

(defmethod release-system-releases ((release fs-release))
  "Get every system file and append their system releases."
  (let* ((system-files (release-system-files release)))
    (apply #'append (mapcar #'system-file-system-releases system-files))))

(defmethod install-release ((release fs-release))
  t)

(defmethod release-installed-p ((release fs-release))
  (every (lambda (x)
           (probe-file (system-file-absolute-asd-pathname x)))
         (release-system-files release)))


;; * System files

(defclass fs-system-file (clpm-system-file)
  ((source
    :initarg :source
    :reader system-file-source)
   (enough-namestring
    :initarg :enough-namestring
    :accessor system-file-asd-enough-namestring)
   (groveled-p
    :initform nil
    :accessor fs-system-file/groveled-p))
  (:documentation "A single system file located on the file system."))

(defmethod system-file-release ((system-file fs-system-file))
  (fs-source-project-release (system-file-source system-file)))

(defmethod system-file-absolute-asd-pathname ((system-file fs-system-file))
  "Merge the enough pathname with the source's root dir."
  (merge-pathnames (system-file-asd-enough-namestring system-file)))

(defmethod system-file-system-releases ((system-file fs-system-file))
  "Grovel over the system file if necessary to determine every system it
contains."
  (unless (fs-system-file/groveled-p system-file)
    ;; Sigh. We need to grovel the file to make sure we know everything it
    ;; defines.
    (active-groveler-ensure-asd-loaded (system-file-absolute-asd-pathname system-file))
    (let ((systems-in-file (active-groveler-systems-in-file (system-file-absolute-asd-pathname system-file)))
          (source (system-file-source system-file)))
      (dolist (system-name systems-in-file)
        ;; Call these for effect to make sure they're created.
        (let* ((system (source-system source system-name))
               (system-release (fs-system-system-release system)))
          system-release)))

    (setf (fs-system-file/groveled-p system-file) t))

  ;; Iterate over all systems the source knows about and return the ones with
  ;; the same primary name as this system file.
  (let ((out nil)
        (source (system-file-source system-file)))
    (maphash (lambda (k v)
               (when (equal (asdf:primary-system-name k)
                            (pathname-name (system-file-asd-enough-namestring system-file)))
                 (push (fs-system-system-release v) out)))
             (fs-source-systems-by-name source))
    out))


;; * System objects

(defclass fs-system (clpm-system)
  ((source
    :initarg :source
    :reader system-source)
   (name
    :initarg :name
    :reader system-name)
   (system-release
    :accessor fs-system-system-release))
  (:documentation "A single system located on the file system."))

(defmethod system-system-releases ((system fs-system))
  (list (fs-system-system-release system)))

(defmethod system-releases ((system fs-system))
  (list (system-file-release (fs-source-system-file (system-source system)))))


;; * System release

(defclass fs-system-release (dotted-versioned-system-release clpm-system-release)
  ((source
    :initarg :source
    :accessor system-release-source)
   (system
    :initarg :system
    :accessor system-release-system)
   (version
    :accessor system-release-system-version
    :documentation "The version of this system. Groveled on request.")
   (reqs
    :accessor system-release-requirements
    :documentation "The dependencies of this system. Groveled on request."))
  (:documentation "A system release for a system located on the file system."))

(defmethod system-release-release ((system-release fs-system-release))
  (fs-source-project-release (system-release-source system-release)))

(defun parse-system-release-info-from-groveler! (system-release info)
  "Take the info provided by the groveler and modify system-release in place to
include it."
  (destructuring-bind (&key version depends-on defsystem-depends-on loaded-systems
                       &allow-other-keys)
      info
    (setf (system-release-system-version system-release) version)
    (setf (system-release-requirements system-release)
          (mapcar #'convert-asd-system-spec-to-req
                  (append depends-on defsystem-depends-on loaded-systems)))))

(defun grovel-system-release! (system-release)
  "Invoke the groveler to get info on this system."
  (active-groveler-ensure-asd-loaded (system-release-absolute-asd-pathname system-release))
  (let ((info (active-groveler-system-deps (system-name (system-release-system system-release)))))
    (parse-system-release-info-from-groveler! system-release info)))

(defmethod slot-unbound (class (system-release fs-system-release) (slot-name (eql 'version)))
  "Grovel and return the version"
  (grovel-system-release! system-release)
  (system-release-system-version system-release))

(defmethod slot-unbound (class (system-release fs-system-release) (slot-name (eql 'reqs)))
  "Grovel and return the requirements"
  (grovel-system-release! system-release)
  (system-release-requirements system-release))

(defmethod system-release-system-file ((system-release fs-system-release))
  "Get the system object using the primary name and the source."
  (fs-source-system-file (system-release-source system-release)))

(defmethod system-release-absolute-asd-pathname ((system-release fs-system-release))
  "Get the absolute pathname using the system-file"
  (system-file-absolute-asd-pathname (system-release-system-file system-release)))
