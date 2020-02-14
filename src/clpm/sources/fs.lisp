;;;; Sources that are located on the file system.
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
          #:clpm/sources/defs
          #:clpm/sources/semantic-versioned-project)
  (:export #:fs-source
           #:fs-source-from-form
           #:fs-source-register-asd
           #:fs-source-system-release-class
           #:fs-project
           #:fs-release
           #:fs-system
           #:fs-system-file
           #:fs-system-release))

(in-package #:clpm/sources/fs)


;; * Source

(defclass fs-source (clpm-source)
  ((name
    :initarg :name
    :accessor source-name)
   (root-pathname
    :initarg :root-pathname
    :initform #p""
    :accessor fs-source-root-pathname
    :documentation "The root directory of this source. If relative, it is
resolved with respect to *DEFAULT-PATHNAME-DEFAULTS*.")
   (project
    :accessor fs-source-project
    :documentation "The single project in this source.")
   (system-files-by-primary-name
    :initform (make-hash-table :test 'equalp)
    :accessor fs-source-system-files-by-primary-name
    :documentation "A hash table that maps system primary names to
fs-system-file objects.")
   (system-files-by-namestring
    :initform (make-hash-table :test 'equal)
    :accessor fs-source-system-files-by-namestring
    :documentation "A hash table that maps system namestrings to fs-system-file
objects.")
   (systems-by-name
    :initform (make-hash-table :test 'equalp)
    :accessor fs-source-systems-by-name
    :documentation "A hash table that maps system names to fs-system objects."))
  (:documentation
   "A source that contains systems located on the file system. Contains one
project (:ALL) and one release (:NEWEST). Does not do any autodiscovery of
system files, system files must be registered with the source using
FS-SOURCE-REGISTER-ASD."))

(defmethod make-source ((type (eql 'fs-source)) &rest initargs
                        &key system-files name root-pathname)
  (aprog1 (ensure-gethash (list type name root-pathname) *source-cache*
                          (apply #'make-instance
                                 type
                                 initargs))
    (dolist (system-file system-files)
      (fs-source-register-asd it system-file))))

(defmethod initialize-instance :after ((source fs-source)
                                       &rest initargs
                                       &key
                                         system-files)
  "Construct the singleton project and release."
  (declare (ignore initargs))
  (assert (uiop:directory-pathname-p (fs-source-root-pathname source)))
  (let* ((project (make-instance 'fs-project
                                 :name :all
                                 :source source))
         (release (make-instance 'fs-release
                                 :version :newest
                                 :project project
                                 :source source)))
    (setf (fs-project-release project) release)
    (setf (fs-source-project source) project)
    (dolist (system-file system-files)
      (fs-source-register-asd source system-file))))

(defmethod source-project ((source fs-source) project-name &optional (error t))
  "If the project name is :ALL, return the singleton project. Otherwise return
nil or error."
  (cond
    ((eql :all project-name)
     (fs-source-project source))
    (error
     (error 'source-missing-project
            :source source
            :project-name project-name))
    (t
     nil)))

(defmethod source-system ((source fs-source) system-name &optional (error t))
  "If this system already exists, return it. Otherwise see if we have an asd
file with the same primary name and construct a new system for it."
  (unless (gethash system-name (fs-source-systems-by-name source))
    ;; We haven't seen this particular system before. See if we have a system
    ;; file that could plausibly contain it.
    (when (gethash (asdf:primary-system-name system-name)
                   (fs-source-system-files-by-primary-name source))
      ;; We do. Construct a new system object for it.
      (let* ((system (make-instance 'fs-system
                                    :name system-name
                                    :source source))
             (system-release (make-instance 'fs-system-release
                                            :source source
                                            :system system
                                            :release (fs-project-release (fs-source-project source)))))
        (setf (fs-system-system-release system) system-release)
        (setf (gethash system-name (fs-source-systems-by-name source))
              system))))
  (or (gethash system-name (fs-source-systems-by-name source))
      (when error
        (error 'source-missing-system
               :source source
               :system-name system-name))))

(defun fs-source-register-asd (fs-source asd-pathname)
  "Given a pathname to an asd file, register it with the source. asd-pathname
can be relative (to the source's root dir) or absolute."
  (unless (uiop:relative-pathname-p asd-pathname)
    (setf asd-pathname (enough-namestring asd-pathname (merge-pathnames (fs-source-root-pathname fs-source)))))
  (let* ((primary-name-ht (fs-source-system-files-by-primary-name fs-source))
         (namestring-ht (fs-source-system-files-by-namestring fs-source))
         (namestring (namestring asd-pathname)))
    (aprog1 (ensure-gethash (pathname-name namestring) primary-name-ht
                            (make-instance 'fs-system-file
                                           :source fs-source
                                           :release (fs-project-release (fs-source-project fs-source))
                                           :enough-namestring namestring))

      (setf (gethash namestring namestring-ht) it))))

(defmethod source-to-form ((source fs-source))
  (let* ((project (source-project source :all))
         (release (project-release project :newest))
         (system-files (release-system-files release)))
    `(,(source-name source)
      :type :file-system
      ,@(unless (uiop:pathname-equal (fs-source-root-pathname source)
                                     (uiop:make-pathname* :directory '(:relative)))
          `(:root-pathname ,(namestring (fs-source-root-pathname source))))
            :system-files ,(mapcar #'system-file-asd-enough-namestring system-files))))

(defun fs-source-from-form (form)
  (destructuring-bind (type &key root-pathname system-files) form
    (assert (eql type :file-system))
    (aprog1 (make-instance 'fs-source
                           :root-pathname (if root-pathname
                                              (pathname root-pathname)
                                              (uiop:pathname-directory-pathname
                                               (uiop:make-pathname* :directory '(:relative)))))
      (dolist (system-file system-files)
        (fs-source-register-asd it system-file)))))

(defmethod sync-source ((source fs-source))
  nil)


;; * Project

(defclass fs-project (clpm-project)
  ((source
    :initarg :source
    :reader project-source)
   (name
    :initarg :name
    :reader project-name)
   (release
    :initarg :release
    :accessor fs-project-release))
  (:documentation "A project on the filesystem. Contains a singleton
release (that is constructed by the source.)"))

(defmethod project-release ((project fs-project) version-string &optional (error t))
  "If the version is :newest, return our singleton release, otherwise nil."
  (cond
    ((equal version-string :newest)
     (fs-project-release project))
    (error
     (error 'project-missing-version
            :source (project-source project)
            :project project
            :version version-string))
    (t nil)))

(defmethod project-releases ((project fs-project))
  (list (fs-project-release project)))


;; * Release

(defclass fs-release (clpm-release)
  ((source
    :initarg :source
    :reader release-source)
   (project
    :initarg :project
    :reader release-project)
   (version
    :initarg :version
    :reader release-version))
  (:documentation "A release on the filesystem. Each fs-source has one instance
of this created upon instantiation."))

(defmethod release-system-file ((release fs-release) system-file-namestring)
  "Look at the fs-source to get the system file."
  (let* ((source (release-source release))
         (ht (fs-source-system-files-by-namestring source)))
    (gethash system-file-namestring ht)))

(defmethod release-system-files ((release fs-release))
  "Look at the source and get all its system files."
  (let* ((source (release-source release))
         (ht (fs-source-system-files-by-namestring source)))
    (hash-table-values ht)))

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
         (hash-table-values (fs-source-system-files-by-namestring (release-source release)))))


;; * System files

(defclass fs-system-file (clpm-system-file)
  ((source
    :initarg :source
    :reader system-file-source)
   (release
    :initarg :release
    :reader system-file-release)
   (enough-namestring
    :initarg :enough-namestring
    :accessor system-file-asd-enough-namestring)
   (groveled-p
    :initform nil
    :accessor fs-system-file/groveled-p))
  (:documentation "A single system file located on the file system."))

(defmethod system-file-absolute-asd-pathname ((system-file fs-system-file))
  "Merge the enough pathname with the source's root dir."
  (merge-pathnames (system-file-asd-enough-namestring system-file)
                   (merge-pathnames (fs-source-root-pathname (system-file-source system-file)))))

(defmethod system-file-system-releases ((system-file fs-system-file))
  "Grovel over the system file if necessary to determine every system it
contains."
  (unless (fs-system-file/groveled-p system-file)
    ;; Sigh. We need to grovel the file to make sure we know everything it
    ;; defines.
    (active-groveler-ensure-asd-loaded! (system-file-absolute-asd-pathname system-file))
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
  (list (fs-project-release (fs-source-project (system-source system)))))


;; * System release

(defclass fs-system-release (semantic-versioned-system-release clpm-system-release)
  ((source
    :initarg :source
    :accessor system-release-source)
   (system
    :initarg :system
    :accessor system-release-system)
   (release
    :initarg :release
    :accessor system-release-release)
   (version
    :accessor system-release-system-version
    :documentation "The version of this system. Groveled on request.")
   (reqs
    :accessor system-release-requirements
    :documentation "The dependencies of this system. Groveled on request."))
  (:documentation "A system release for a system located on the file system."))

(defun parse-system-release-info-from-groveler! (system-release info)
  "Take the info provided by the groveler and modify system-release in place to
include it."
  (destructuring-bind (system-name
                       &key version depends-on defsystem-depends-on loaded-systems
                       &allow-other-keys)
      info
    (declare (ignore system-name))
    (setf (system-release-system-version system-release) version)
    (setf (system-release-requirements system-release)
          (mapcar #'convert-asd-system-spec-to-req
                  (append depends-on defsystem-depends-on loaded-systems)))))

(defun grovel-system-release! (system-release)
  "Invoke the groveler to get info on this system."
  (active-groveler-ensure-asd-loaded! (system-release-absolute-asd-pathname system-release))
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
  (let* ((source (system-release-source system-release))
         (ht (fs-source-system-files-by-primary-name source))
         (primary-name (asdf:primary-system-name
                        (system-name (system-release-system system-release)))))
    (gethash primary-name ht)))

(defmethod system-release-absolute-asd-pathname ((system-release fs-system-release))
  "Get the absolute pathname using the system-file"
  (system-file-absolute-asd-pathname (system-release-system-file system-release)))
