;;;; Flat file source definitions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

;; * define-package
(uiop:define-package #:clpm/sources/flat-file
    (:use #:cl
          #:alexandria
          #:clpm/cache
          #:clpm/data
          #:clpm/sources/defs
          #:do-urlencode
          #:puri
          #:split-sequence)
  (:export #:flat-file-project
           #:flat-file-release
           #:flat-file-source
           #:flat-file-source-project-class
           #:flat-file-source-release-class
           #:flat-file-source-root-pathname
           #:flat-file-system-release
           #:flat-file-system-release-dependencies
           #:flat-file-source-system-release-class))

(in-package #:clpm/sources/flat-file)

(defclass flat-file-source ()
  ((projects-map
    :initform (make-hash-table :test 'equal)
    :reader flat-file-source-projects-map
    :documentation "Maps project names to project instances")
   (systems-map
    :initform (make-hash-table :test 'equal)
    :reader flat-file-source-systems-map
    :documentation "Maps project names to project instances")))

(defgeneric flat-file-source-project-class (source))

(defmethod flat-file-source-project-class ((source flat-file-source))
  'flat-file-project)

(defgeneric flat-file-source-system-class (source))

(defmethod flat-file-source-system-class ((source flat-file-source))
  'flat-file-system)

(defgeneric flat-file-source-system-file-class (source))

(defmethod flat-file-source-system-file-class ((source flat-file-source))
  'flat-file-system-file)

(defgeneric flat-file-source-system-release-class (source))

(defmethod flat-file-source-system-release-class ((source flat-file-source))
  'flat-file-system-release)

(defgeneric flat-file-source-release-class (source))

(defmethod flat-file-source-release-class ((source flat-file-source))
  'flat-file-release)

(defgeneric flat-file-source-root-pathname (source))

(defun %projects-root-pathname (source)
  (merge-pathnames "projects/" (flat-file-source-root-pathname source)))

(defun %systems-root-pathname (source)
  (merge-pathnames "systems/" (flat-file-source-root-pathname source)))

(defmethod source-project ((source flat-file-source) project-name &optional (error t))
  (ensure-gethash
   project-name (flat-file-source-projects-map source)

   (let ((pathname (merge-pathnames project-name (%projects-root-pathname source))))
     (unless (uiop:probe-file* pathname)
       (if error
           (error 'source-missing-project
                  :source source
                  :project-name project-name)
           ;; Do not cache a negative hit.
           (return-from source-project nil)))
     (make-instance (flat-file-source-project-class source)
                    :source source
                    :name project-name))))

(defmethod source-system ((source flat-file-source) system-name &optional (error t))
  (ensure-gethash
   system-name (flat-file-source-systems-map source)

   (let ((pathname (merge-pathnames (urlencode system-name) (%systems-root-pathname source))))
     (unless (probe-file pathname)
       (if error
           (error 'source-missing-system
                  :source source
                  :system-name system-name)
           ;; Do not cache a negative hit.
           (return-from source-system nil)))
     (make-instance (flat-file-source-system-class source)
                    :source source
                    :name system-name))))

(defmethod source-projects ((source flat-file-source))
  (let ((dir (directory (merge-pathnames (make-pathname :directory '(:relative :wild))
                                         (%projects-root-pathname source)))))
    (mapcar (lambda (pn)
              (source-project source (last-elt (pathname-directory pn))))
            dir)))

(defmethod source-systems ((source flat-file-source))
  (let ((dir (directory (uiop:wilden (%systems-root-pathname source)))))
    (mapcar (lambda (pn)
              (source-system source (urldecode (if (pathname-type pn)
                                                   (concatenate 'string
                                                                (pathname-name pn)
                                                                "."
                                                                (pathname-type pn))
                                                   (pathname-name pn)))))
            dir)))


;; * Projects

(defclass flat-file-project ()
  ((source
    :initarg :source
    :reader project-source
    :documentation "The source that provides this project.")
   (name
    :initarg :name
    :reader project-name
    :documentation "The name of the project.")
   (releases-map
    :accessor flat-file-project-releases-map)))

(defun %populate-project-releases! (project)
  "Open the releases file, read it, and instantiate release objects for each
  release."
  (let ((releases-map (make-hash-table :test 'equal)))
    (uiop:with-safe-io-syntax ()
      (with-open-file (s (%project-releases-pathname project))
        (loop
          (let ((form (read s nil :eof)))
            (when (eql form :eof)
              (return))
            (setf (gethash (first form) releases-map)
                  (apply #'make-instance (flat-file-source-release-class (project-source project))
                         :source (project-source project)
                         :project project
                         :version form))))))
    (setf (flat-file-project-releases-map project) releases-map)))

(defmethod slot-unbound (class (project flat-file-project) (slot-name (eql 'releases-map)))
  (%populate-project-releases! project)
  (flat-file-project-releases-map project))

(defun %project-root-pathname (project)
  (uiop:ensure-directory-pathname
   (merge-pathnames (project-name project)
                    (%projects-root-pathname (project-source project)))))

(defun %project-releases-pathname (project)
  (merge-pathnames "releases" (%project-root-pathname project)))

(defmethod project-release ((project flat-file-project) version &optional (error t))
  (or (gethash version (flat-file-project-releases-map project))
      (when error
        (error 'project-missing-version
               :source (project-source project)
               :project project
               :version version))))

(defmethod project-releases ((project flat-file-project))
  (hash-table-values (flat-file-project-releases-map project)))


;; * Releases

(defclass flat-file-release (clpm-release)
  ((source
    :initarg :source
    :reader release-source
    :documentation "The source that provides this release.")
   (project
    :initarg :project
    :reader release-project
    :documentation "The project this release is a part of.")

   (version
    :initarg :version
    :reader release-version
    :documentation "The version of this release.")
   (system-files
    :initarg :system-files
    :initform nil
    :documentation "List of paths to system files in this release.")
   (system-names
    :initarg :systems
    :documentation "List of system names provided by this release.")
   (systems-map
    :reader flat-file-release-systems-map
    :documentation "Map of system names to system objects provided by this
    release.")))

(defmethod slot-unbound (class (release flat-file-release) (slot-name (eql 'systems-map)))
  (let ((out (make-hash-table :test 'equal)))
    (dolist (system-name (slot-value release 'system-names))
      (setf (gethash system-name out) (source-system (release-source release) system-name)))
    (setf (slot-value release slot-name) out)))

(defmethod release-system-release ((release flat-file-release) system-name &optional (error t))
  (let ((system (gethash system-name (flat-file-release-systems-map release))))
    (when (and error (not system))
      (error 'release-missing-system-release
             :source (release-source release)
             :release release
             :system-name system-name))
    (when system
      (let ((system-release (gethash (list (project-name (release-project release))
                                           (release-version release))
                                     (flat-file-system-releases-map system))))
        (unless system-release
          (error "Unknown error! Cannot find the correct system release!"))
        system-release))))

(defmethod release-systems ((release flat-file-release))
  (hash-table-values (flat-file-release-systems-map release)))


;; * Systems

(defclass flat-file-system ()
  ((source
    :initarg :source
    :reader system-source
    :documentation "The source that provides this system.")
   (name
    :initarg :name
    :reader system-name
    :documentation "The name of the system.")

   (system-releases-map
    :reader flat-file-system-releases-map
    :documentation "Maps release specs to system-release objects.")))

(defmethod slot-unbound (class (system flat-file-system) (slot-name (eql 'system-releases-map)))
  "Read the system file, instantiating system-release objects for each release
  of the system."

  (let ((map (make-hash-table :test 'equal)))
    (uiop:with-safe-io-syntax ()
      (with-open-file (s (merge-pathnames (urlencode (system-name system))
                                          (%systems-root-pathname (system-source system))))
        (loop
          (let ((form (read s nil :eof)))
            (when (eql form :eof)
              (return))
            (destructuring-bind (release-spec &rest initargs &key &allow-other-keys) form
              (setf (gethash release-spec map)
                    (apply #'make-instance
                           (flat-file-source-system-release-class (system-source system))
                           :source (system-source system)
                           :system-name (system-name system)
                           :release-spec release-spec
                           :system system
                           initargs)))))))
    (setf (slot-value system slot-name) map)))

(defmethod system-releases ((system flat-file-system))
  (mapcar #'system-release-release (system-system-releases system)))

(defmethod system-system-releases ((system flat-file-system))
  (hash-table-values (flat-file-system-releases-map system)))


;; * System releases

(defclass flat-file-system-release (clpm-system-release)
  ((source
    :initarg :source
    :reader system-release-source
    :documentation "The source that provides this system release.")
   (system-name
    :initarg :system-name
    :reader flat-file-system-release-system-name
    :documentation "The name of the system this system release is for.")
   (release-spec
    :initarg :release-spec
    :reader flat-file-system-release-release-spec
    :documentation "A specification of which release this system release belongs to.")

   (system
    :initarg :system
    :reader system-release-system
    :documentation "The system this system release belongs to.")
   (release
    :initarg :release
    :reader system-release-release
    :documentation "The release this system release belongs to.")

   (asd-enough-namestring
    :initarg :system-file
    :reader system-release-asd-pathname)
   (dependencies
    :initarg :dependencies
    :initform nil
    :reader flat-file-system-release-dependencies)
   (system-file
    :reader system-release-system-file)))

(defmethod slot-unbound (class (system-release flat-file-system-release) (slot-name (eql 'system)))
  (setf (slot-value system-release slot-name)
        (source-system (system-release-source system-release)
                       (flat-file-system-release-system-name system-release))))

(defmethod slot-unbound (class (system-release flat-file-system-release) (slot-name (eql 'release)))
  (destructuring-bind (project-name version)
      (flat-file-system-release-release-spec system-release)
    (setf (slot-value system-release slot-name)
          (source-project-release (system-release-source system-release) project-name version))))

(defmethod slot-unbound (class (system-release flat-file-system-release) (slot-name (eql 'system-file)))
  (setf (slot-value system-release slot-name)
        (make-instance (flat-file-source-system-file-class (system-release-source system-release))
                       :source (system-release-source system-release)
                       :release (system-release-release system-release)
                       :asd-enough-namestring (system-release-asd-pathname system-release))))


;; * System files

(defclass flat-system-file ()
  ((source
    :initarg :source
    :reader system-file-source
    :documentation "The source that provides this system file.")

   (release
    :initarg :release
    :reader system-file-release)
   (asd-enough-namestring
    :initarg :asd-enough-namestring
    :reader system-file-asd-enough-namestring
    :documentation "The namestring pointing to the asd file within the release.")))

(defmethod system-file-system-releases ((system-file flat-system-file))
  (release-system-releases (system-file-release system-file)))
