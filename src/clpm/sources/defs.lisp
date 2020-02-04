;;;; Source definitions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

;; * define-package
(uiop:define-package #:clpm/sources/defs
    (:use #:cl
          #:alexandria
          #:clpm/cache
          #:clpm/data
          #:puri
          #:split-sequence)
  (:export #:clpm-project
           #:clpm-release
           #:clpm-system-release
           #:clpm-source
           #:clpm-system
           #:project
           #:project-missing-version
           #:project-name
           #:project-release
           #:project-releases
           #:project-repo
           #:project-source
           #:release->
           #:release-lib-pathname
           #:release-project
           #:release-requirements
           #:release-system-release
           #:release-source
           #:release-system-file
           #:release-system-files
           #:release-system-releases
           #:release-systems
           #:release-version
           #:release-installed-p
           #:release-missing-system-release
           #:release-satisfies-version-spec-p
           #:repo
           #:source
           #:source-args
           #:source-cache-directory
           #:source-lib-directory
           #:source-name
           #:source-project
           #:source-project-release
           #:source-projects
           #:source-system
           #:source-systems
           #:source-url
           #:source-context-pathname
           #:source-missing-project
           #:source-missing-system
           #:source-no-such-object
           #:source-source-registry-cache-pathname
           #:source-to-form
           #:source-type-keyword
           #:sync-and-retry
           #:sync-source
           #:system-name
           #:system-system-releases
           #:system-releases
           #:system-source
           #:clpm-system-file
           #:system-file-absolute-asd-pathname
           #:system-file-asd-enough-namestring
           #:system-file-release
           #:system-file-source
           #:system-file-system-releases
           #:system-release-absolute-asd-pathname
           #:system-release-asd-pathname
           #:system-release-release
           #:system-release-requirements
           #:system-release-source
           #:system-release-system
           #:system-release-system-file
           #:system-release-system-version
           #:system-release->
           #:system-release-satisfies-version-spec-p
           #:system-version
           #:version))

(in-package #:clpm/sources/defs)


;; * Conditions

(define-condition source-no-such-object ()
  ((source
    :initarg :source)))

(define-condition source-missing-project (source-no-such-object)
  ((project-name
    :initarg :project-name)))

(define-condition source-missing-system (source-no-such-object)
  ((system-name
    :initarg :system-name)))

(define-condition project-missing-version (source-no-such-object)
  ((project
    :initarg :project)
   (version
    :initarg :version)))

(define-condition release-missing-system-release (source-no-such-object)
  ((release
    :initarg :release)
   (system-name
    :initarg :system-name)))


;; * Sources

(defclass clpm-source ()
  ((url
    :initarg :url
    :accessor source-url
    :documentation "A puri URL that specifies where the source is located.")
   (name
    :initarg :name
    :accessor source-name
    :documentation "The name of the source."))
  (:documentation "Base class for any CLPM source. A source is identified
uniquely by a URL to its canonical location and contains projects and
systems."))

(defmethod initialize-instance :after ((self clpm-source) &rest initargs &key url)
  "Ensure that the URL field on a CLPM-SOURCE instance is a puri URL."
  (declare (ignore initargs))
  (setf (source-url self) (parse-uri url)))

(defgeneric source-cache-directory (source)
  (:documentation "The source's cache directory."))

(defgeneric source-lib-directory (source)
  (:documentation "The source's lib directory."))

(defun source-context-pathname (source context-name)
  (merge-pathnames (make-pathname :type "sexp"
                                  :directory `(:relative "contexts")
                                  :name context-name)
                   (source-lib-directory source)))

(defun source-source-registry-cache-pathname (source)
  (merge-pathnames ".cl-source-registry.cache"
                   (source-lib-directory source)))

(defgeneric sync-source (source)
  (:documentation "Synchronize the local source metadata with the upstream
metadata. Returns T if the local data has changed, NIL otherwise."))

(defgeneric source-type-keyword (source)
  (:documentation "Returns the keyword designating the type of this source."))

(defgeneric source-to-form (source)
  (:documentation "Given a source, return a list that is appropriate for
inclusion in the sources.conf file. Consists of the URL followed by a plist of
initargs and a :type as a keyword."))

(defgeneric source-project (source project-name &optional error)
  (:documentation "Return an instance of CLPM-PROJECT. If the project is not in
the source and ERROR is T (default), an error condition of type
SOURCE-MISSING-PROJECT is signaled. Otherwise, NIL is returned."))

(defgeneric source-project-release (source project-name version-string &optional error)
  (:documentation
   "Return a release object for the specified project and version. If error is
t (default), a ~source-no-such-object~ error is signaled. If possible, the
source implementation should provide a ~sync-and-retry~ restart.")
  (:method (source project-name version-string &optional (error t))
    (let ((project (source-project source project-name error)))
      (when project
        (project-release project version-string error)))))

(defgeneric source-system (source system-name &optional error)
  (:documentation "Return an instance of CLPM-SYSTEM. If the system is not
located in the source and ERROR is T (default), signals an error of type
SOURCE-MISSING-SYSTEM. Otherwise, returns NIL."))

(defgeneric source-projects (source)
  (:documentation "Return a list of all projects (as CLPM-PROJECT instances)
that are provided by the SOURCE."))

(defgeneric source-systems (source)
  (:documentation "Return a list of all systems (as CLPM-SYSTEM instances) that
are provided by the SOURCE."))


;; * Systems

(defclass clpm-system ()
  ((source
    :initarg :source
    :accessor system-source
    :documentation "The source that provides this system.")
   (name
    :initarg :name
    :accessor system-name
    :documentation "The name of the system."))
  (:documentation "Base class for a system that is located in a source. A system
represents an ASDF system."))

(defgeneric system-releases (system)
  (:documentation "Return a list of releases (as CLPM-RELEASE instances) that
provide this system."))

(defgeneric system-system-releases (system)
  (:documentation "Return a list of system-releases (as CLPM-SYSTEM-RELEASE
instances) that provide this system."))


;; * Projects

(defclass clpm-project ()
  ((source
    :initarg :source
    :accessor project-source
    :documentation "The source that provides this project.")
   (name
    :initarg :name
    :accessor project-name
    :documentation "The name of this project.")
   (repo
    :initarg :repo
    :accessor project-repo))
  (:documentation "The base class for all CLPM projects. A project can provide
many systems and the list of provided systems can change over time as new
releases are made."))

(defgeneric project-release (project version-string &optional error)
  (:documentation "Return a release matching the given version string. If ERROR
is T (default) and there is no such version of the project, signals an error of
type PROJECT-MISSING-VERSION."))

(defgeneric project-releases (project)
  (:documentation "Return a list of releases (as CLPM-RELEASE instances) of this
project."))


;; * Releases

(defclass clpm-release ()
  ()
  (:documentation "The base class for all CLPM releases. A release is a
versioned snapshot of a project. A release provides systems."))

(defmethod print-object ((r clpm-release) stream)
  (pprint-logical-block (stream nil)
    (print-unreadable-object (r stream :type t :identity t)
      (pprint-newline :linear stream)
      ;; Project
      (prin1 :project stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (write (project-name (release-project r))
             :stream stream)
      ;; Version
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (prin1 :version stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (write (release-version r)
             :stream stream)
      ;; ID
      (pprint-newline :linear stream))))

(defgeneric release-satisfies-version-spec-p (release version-spec)
  (:documentation "Returns T if the release satisfies the VERSION-SPEC."))

(defgeneric release-requirements (release)
  (:documentation "A list of requirements needed by all systems in this release.")
  (:method (release)
    (mapcan #'system-release-requirements (release-system-releases release))))

(defgeneric release-system-releases (release)
  (:documentation "A list of system releases provided by this project release.")
  (:method (release)
    (let* ((systems (release-systems release))
           (system-names (mapcar #'system-name systems)))
      (mapcar (curry #'release-system-release release) system-names))))

(defgeneric release-systems (release)
  (:documentation "A list of systems (as CLPM-SYSTEM instances) provided by this
release."))

(defgeneric release-installed-p (release)
  (:documentation "Returns T if the release has been installed already.")
  (:method (r)
    (uiop:probe-file* (release-lib-pathname r))))

(defgeneric release-lib-pathname (release)
  (:documentation "Return the base pathname for where this release is installed.")
  (:method (release)
    (let ((version (release-version release)))
      (uiop:resolve-absolute-location
       (list (source-lib-directory (release-source release))
             "projects"
             (project-name (release-project release))
             version)
       :ensure-directory t))))

(defgeneric release-system-file (release system-file-namestring)
  (:documentation "Return the system-file object that is part of this release
located at system-file-namestring."))

(defgeneric release-system-files (release)
  (:documentation "Return all system files part of this release."))

(defgeneric release-system-release (release system-name &optional error)
  (:documentation "Given a release and the name of a system provided by the
release, return the corresponding CLPM-SYSTEM-RELEASE object. If these is no
such system release and ERROR is T (default), signals an error of type
RELEASE-MISSING-SYSTEM-RELEASE. Otherwise, returns NIL."))

(defgeneric release-> (release-1 release-2)
  (:documentation "Returns T if release-1 is a greater version than
release-2."))


;; * System files

(defclass clpm-system-file ()
  ((source
    :initarg :source
    :accessor system-file-source
    :documentation "The source that provides this system file.")
   (release
    :initarg :release
    :accessor system-file-release
    :documentation "The release that provides this system file."))
  (:documentation
   "Base class representing a single system file. It is scoped to a release."))

(defgeneric system-file-asd-enough-namestring (clpm-system-file))

(defgeneric system-file-absolute-asd-pathname (clpm-system-file))

(defgeneric system-file-system-releases (clpm-system-file))


;; * System releases

(defclass clpm-system-release ()
  ()
  (:documentation "Base class to represent a specific release of a system."))

(defmethod print-object ((sr clpm-system-release) stream)
  (print-unreadable-object (sr stream :type t :identity t)
    (format stream "system: ~A, project: ~A, version: ~A"
            (system-name (system-release-system sr))
            (project-name (release-project (system-release-release sr)))
            (release-version (system-release-release sr)))))

(defgeneric system-release-satisfies-version-spec-p (system-release version-spec)
  (:documentation "Returns T if the system release specifies the version
spec."))

(defgeneric system-release-asd-pathname (system-release)
  (:documentation "The pathname to the asd file, relative to the root folder for
this release."))

(defgeneric system-release-absolute-asd-pathname (system-release)
  (:documentation "Returns the absolute pathname to the installed ASD file for
this system release.")
  (:method (system-release)
    (merge-pathnames (system-release-asd-pathname system-release)
                     (release-lib-pathname (system-release-release system-release)))))

(defgeneric system-release-requirements (system-release))

(defgeneric system-release-system-file (system-release)
  (:documentation "Returns the system-file object representing the file that
contains the definition of this system release."))

(defgeneric system-release-> (system-release-1 system-release-2)
  (:documentation "Returns T if system-release-1 is a greater version than
system-release-2."))


;; * Utils

(defun uri-to-relative-pathname (uri)
  (uiop:make-pathname*
   :directory (list* :relative
                     (string-downcase (symbol-name (uri-scheme uri)))
                     (uri-host uri)
                     (split-sequence #\/ (uri-path uri)
                                     :remove-empty-subseqs t))))
