;;;; CLPI derived sources
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/sources/clpi
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/cache
          #:clpm/data
          #:clpm/http-client
          #:clpm/log
          #:clpm/repos
          #:clpm/requirement
          #:clpm/sources/db-backed
          #:clpm/sources/defs
          #:clpm/sources/simple-versioned-project
          #:clpm/sources/tarball-release
          #:clpm/sources/vcs
          #:clpm/utils
          #:split-sequence
          #:sxql)
  (:import-from #:puri
                #:parse-uri
                #:uri-path
                #:uri-scheme
                #:uri-host)
  (:export #:clpi-source))

(uiop:define-package #:clpm/sources/clpi-index-file
  (:use #:cl))

(in-package #:clpm/sources/clpi)

(setup-logger)


;;; * Source

(defclass clpi-source (clpm-source db-backed-source)
  ())

(defclass clpi-source-meta (db-backed-source-meta)
  ()
  (:metaclass dao-table-class))

(defmethod db-source-table-names ((source clpi-source))
  '(clpi-source-meta clpi-project clpi-release
    clpi-system clpi-system-release))

(defmethod db-source-indices ((source clpi-source))
  '((:key_clpi-release_project-name_version
     :clpi_release :project_name :version)
    (:key_clpi-system-release_release-id_system-name
     :clpi_system_release :release_id :system_name)))

(defmethod db-source-meta-class-name ((source clpi-source))
  'clpi-source-meta)

(defun clpi-source-index-url (source)
  (let* ((base-url (source/url source))
         (base-path (or (puri:uri-path base-url)
                        "/"))
         (extended-path (format nil "~A~:[/~;~]package-index/v0.1/index.lisp"
                                base-path (ends-with #\/ base-path))))
    (puri:copy-uri base-url :path extended-path)))

(defmethod source/cache-directory ((source clpi-source))
  "Compute the cache location for this source, based on its canonical url."
  (clpm-cache-pathname
   `("sources"
     "clpi"
     ,(string-downcase (string (uri-scheme (source/url source))))
     ,(uri-host (source/url source))
     ,@(split-sequence #\/ (uri-path (source/url source)) :remove-empty-subseqs t))
   :ensure-directory t))

(defmethod source/lib-directory ((source clpi-source))
  "Compute the data location for this source, based on its canonical url."
  (clpm-data-pathname
   `("sources"
     "clpi"
     ,(string-downcase (string (uri-scheme (source/url source))))
     ,(uri-host (source/url source))
     ,@(split-sequence #\/ (uri-path (source/url source)) :remove-empty-subseqs t))
   :ensure-directory t))

(defmethod source/project ((source clpi-source) project-name)
  (with-source-connection (source)
    (find-dao 'clpi-project :name project-name)))

(defmethod source/system ((source clpi-source) system-name)
  (with-source-connection (source)
    (find-dao 'clpi-system :name system-name)))

(defmethod source-to-form ((source clpi-source))
  (list (source/name source)
        :url (uri-to-string (source/url source))
        :type :clpi))


;;; * Projects

(defclass clpi-project (db-backed-mixin)
  ((name
    :initarg :name
    :accessor clpi-project-name
    :reader project/name
    :col-type :text
    :primary-key t
    :reader object-id
    :documentation
    "The name of the project.")
   (repo-type
    :initarg :repo-type
    :initform nil
    :accessor clpi-project-repo-type
    :col-type (or :text :null)
    :deflate (lambda (x)
               (uiop:with-safe-io-syntax ()
                 (prin1-to-string x)))
    :inflate (lambda (x)
               (uiop:with-safe-io-syntax ()
                 (read-from-string x)))
    :documentation
    "The type of the upstream repo.")
   (repo-args
    :initarg :repo-args
    :initform nil
    :accessor clpi-project-repo-args
    :col-type (or :text :null)
    :deflate (lambda (x)
               (uiop:with-safe-io-syntax ()
                 (prin1-to-string x)))
    :inflate (lambda (x)
               (uiop:with-safe-io-syntax ()
                 (read-from-string x)))))
  (:metaclass dao-table-class)
  (:record-timestamps nil)
  (:documentation
   "Represents a project in a CLPI source."))

(defmethod project/source ((p clpi-project))
  (db-backed-object-source p))

(defmethod project/release ((p clpi-project) (version string))
  (with-source-connection ((db-backed-object-source p))
    (find-dao 'clpi-release
              :project-name (clpi-project-name p)
              :version version)))

(defmethod project/releases ((p clpi-project))
  (with-source-connection ((db-backed-object-source p))
    (retrieve-dao 'clpi-release :project-name (clpi-project-name p))))

(defmethod project/repo ((p clpi-project))
  (let ((repo-type (clpi-project-repo-type p))
        (repo-args (clpi-project-repo-args p)))
    (make-repo-from-description (list* repo-type repo-args))))


;;; * Releases

(defclass clpi-release (db-backed-mixin
                        tarball-release
                        simple-versioned-release)
  ((project-name
    :initarg :project-name
    :accessor clpi-release-project-name
    :col-type :text
    :documentation
    "The name of the project to which this release corresponds.")
   (version
    :reader release/version
    :initarg :version
    :col-type :text
    :documentation
    "The version of this release.")
   (prefix
    :accessor clpi-release-prefix
    :initarg :prefix
    :col-type :text
    :documentation
    "The prefix of all files in the tarball.")
   (url
    :accessor clpi-release-url
    :reader tarball-release/url
    :initarg :url
    :col-type :text
    :deflate (lambda (x)
               (uri-to-string x))
    :inflate (lambda (x)
               (parse-uri x))
    :documentation
    "The URL where the tarball for this release is located."))
  (:metaclass dao-table-class)
  (:record-timestamps nil)
  (:documentation
   "Represents a release of a project."))

(defmethod release/lib-pathname ((r clpi-release))
  (with-source-connection ((release/source r))
    (uiop:resolve-absolute-location
     (list (source/lib-directory (release/source r))
           "projects"
           (project/name (release/project r))
           (clpi-release-prefix r))
     :ensure-directory t)))

(defmethod release/system-releases ((release clpi-release))
  (with-source-connection ((release/source release))
    (retrieve-dao 'clpi-system-release :release-id (object-id release))))

(defmethod release/project ((release clpi-release))
  (with-source-connection ((release/source release))
    (find-dao 'clpi-project :name (clpi-release-project-name release))))


;;; * Systems

(defclass clpi-system (db-backed-mixin)
  ((name
    :initarg :name
    :accessor system/name
    :col-type :text
    :primary-key t
    :reader object-id
    :documentation
    "The name of the system."))
  (:metaclass dao-table-class)
  (:record-timestamps nil)
  (:documentation
   "An ASD system contained in a CLPI source."))


;;; * System releases

(defclass clpi-system-release (db-backed-mixin)
  ((release-id
    :initarg :release-id
    :accessor clpi-system-release-release-id
    :col-type :integer
    :documentation
    "The ID of the release to which the system release belongs.")
   (system-name
    :initarg :system-name
    :accessor clpi-system-release-system-name
    :col-type :text
    :documentation
    "The name of the system.")
   (system-version
    :initarg :system-version
    :accessor clpi-system-release-system-version
    :col-type (or :text :null)
    :documentation
    "The version of this system at this release.")
   (depends-on
    :initarg :depends-on
    :accessor clpi-system-release-depends-on
    :col-type :text
    :deflate (lambda (x)
               (uiop:with-safe-io-syntax ()
                 (prin1-to-string x)))
    :inflate (lambda (x)
               (uiop:with-safe-io-syntax ()
                 (read-from-string x)))
    :documentation
    "The list of dependencies this system has.")
   (system-file
    :initarg :system-file
    :accessor clpi-system-release-system-file
    :col-type :text
    :documentation
    "The asd file in which this system is located."))
  (:metaclass dao-table-class)
  (:record-timestamps nil)
  (:documentation
   "A release of a system."))

(defmethod system-release/source ((system-release clpi-system-release))
  (db-backed-object-source system-release))

(defmethod system-release/system ((system-release clpi-system-release))
  (with-source-connection ((system-release/source system-release))
    (find-dao 'clpi-system :name (clpi-system-release-system-name system-release))))

(defmethod system-release/release ((system-release clpi-system-release))
  (with-source-connection ((system-release/source system-release))
    (find-dao 'clpi-release :id (clpi-system-release-release-id system-release))))

(defmethod system-release/requirements ((system-release clpi-system-release))
  (let ((deps (remove-if (rcurry #'member (list "asdf" "uiop") :test #'string-equal)
                         (clpi-system-release-depends-on system-release))))
    (mapcar (lambda (dep-name)
              (make-instance 'system-requirement
                             :name dep-name))
            deps)))


;;; * Syncing

(defvar *active-source* nil)

(defmethod sync-source ((source clpi-source))
  (let ((index-file (merge-pathnames "index.lisp"
                                     (source/cache-directory source))))
    (when (ensure-file-fetched index-file (clpi-source-index-url source))
      (let ((*package* (find-package :clpm/sources/clpi-index-file))
            (*active-source* source))
        (load index-file))))
  (values))

(defun ensure-project (name repo)
  (let ((existing-proj (find-dao 'clpi-project :name name)))
    (destructuring-bind (repo-type &rest repo-args) (or repo (list nil))
      (when existing-proj
        ;; Ensure the project's data is up to date.
        (setf (clpi-project-repo-type existing-proj) repo-type
              (clpi-project-repo-args existing-proj) repo-args)
        (save-dao existing-proj))
      (or existing-proj
          (create-dao 'clpi-project
                      :name name
                      :repo-type repo-type
                      :repo-args repo-args)))))

(defun ensure-release (project-name version prefix url)
  (let ((existing-release (find-dao 'clpi-release
                                    :project-name project-name
                                    :version version)))
    (when existing-release
      ;; Ensure the release is up to date.
      (setf (clpi-release-prefix existing-release) prefix
            (clpi-release-url existing-release) (parse-uri url))
      (save-dao existing-release))
    (or existing-release
        (create-dao 'clpi-release
                    :project-name project-name
                    :version version
                    :prefix prefix
                    :url (parse-uri url)))))

(defun ensure-system-release (release-id system-name system-file system-version depends-on)
  (let ((existing (find-dao 'clpi-system-release
                            :release-id release-id
                            :system-name system-name)))
    (when existing
      ;; Ensure the system release is up to date.
      (setf (clpi-system-release-system-file existing) system-file
            (clpi-system-release-system-version existing) system-version
            (clpi-system-release-depends-on existing) depends-on)
      (save-dao existing))
    (or existing
        (create-dao 'clpi-system-release
                    :release-id release-id
                    :system-name system-name
                    :system-file system-file
                    :system-version system-version
                    :depends-on depends-on))))

(defun ensure-project-defined (name &key repo releases)
  (with-source-connection (*active-source*)
    (let* ((name (string-downcase (string name)))
           (proj (ensure-project name repo)))
      (dolist (r-desc releases)
        (destructuring-bind (version &key url tar-prefix ((:systems systems-desc)))
            r-desc
          (let ((release (ensure-release name version tar-prefix url)))
            (dolist (s-desc systems-desc)
              (destructuring-bind (system-name &key asd-pathname depends-on ((:version system-version)))
                  s-desc
                (ensure-system-release (object-id release) system-name
                                       asd-pathname
                                       system-version
                                       depends-on)
                (or (find-dao 'clpi-system
                              :name system-name)
                    (create-dao 'clpi-system
                                :name system-name)))))))
      proj)))

(in-package #:clpm/sources/clpi-index-file)

(defun version (version-string)
  (assert (equal version-string "0.1")))

(defmacro define-project (name &key repo releases)
  `(progn
     (clpm/sources/clpi::ensure-project-defined ,name
                                                :repo ',repo
                                                :releases ',releases)))
