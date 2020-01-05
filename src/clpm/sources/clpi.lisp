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
          #:clpm/sources/defs
          #:clpm/sources/simple-versioned-project
          #:clpm/sources/tarball-release
          #:clpm/sources/vcs
          #:clpm/utils
          #:split-sequence)
  (:import-from #:puri
                #:parse-uri
                #:uri-path
                #:uri-scheme
                #:uri-host)
  (:import-from #:cl-conspack)
  (:export #:clpi-source))

(uiop:define-package #:clpm/sources/clpi-index-file
  (:use #:cl))

(in-package #:clpm/sources/clpi)

(setup-logger)

(defvar *active-source* nil)


;;; * Source

(defclass clpi-source (clpm-source)
  ((project-ht
    :initform (make-hash-table :test 'equal)
    :accessor clpi-source-project-ht)
   (system-ht
    :initform (make-hash-table :test 'equal)
    :accessor clpi-source-system-ht)))

(defmethod initialize-instance :after ((source clpi-source) &key &allow-other-keys)
  ;; Load the synced files if they exist.
  (let ((pn (merge-pathnames "clpi-source.conspack"
                             (source-lib-directory source))))
    (when (probe-file pn)
      (with-open-file (s pn
                         :element-type '(unsigned-byte 8))
        (assert (= 1 (cpk:decode-stream s)))
        (let ((*active-source* source))
          (cpk:tracking-refs ()
            (setf (clpi-source-project-ht source) (cpk:decode-stream s))
            (setf (clpi-source-system-ht source) (cpk:decode-stream s))))))))

(defun save-clpi-source! (source)
  (let ((pn (merge-pathnames "clpi-source.conspack"
                             (source-lib-directory source))))
    (ensure-directories-exist pn)
    (with-open-file (s pn
                       :if-exists :supersede
                       :direction :output
                       :element-type '(unsigned-byte 8))
      (cpk:encode 1 :stream s)
      (cpk:tracking-refs ()
        (cpk:encode (clpi-source-project-ht source) :stream s)
        (cpk:encode (clpi-source-system-ht source) :stream s)))))

(defun clpi-source-index-url (source)
  (let* ((base-url (source-url source))
         (base-path (or (puri:uri-path base-url)
                        "/"))
         (extended-path (format nil "~A~:[/~;~]package-index/v0.1/index.lisp"
                                base-path (ends-with #\/ base-path))))
    (puri:copy-uri base-url :path extended-path)))

(defmethod source-cache-directory ((source clpi-source))
  "Compute the cache location for this source, based on its canonical url."
  (clpm-cache-pathname
   `("sources"
     "clpi"
     ,(string-downcase (string (uri-scheme (source-url source))))
     ,(uri-host (source-url source))
     ,@(split-sequence #\/ (uri-path (source-url source)) :remove-empty-subseqs t))
   :ensure-directory t))

(defmethod source-lib-directory ((source clpi-source))
  "Compute the data location for this source, based on its canonical url."
  (clpm-data-pathname
   `("sources"
     "clpi"
     ,(string-downcase (string (uri-scheme (source-url source))))
     ,(uri-host (source-url source))
     ,@(split-sequence #\/ (uri-path (source-url source)) :remove-empty-subseqs t))
   :ensure-directory t))

(defmethod source-project ((source clpi-source) project-name)
  (gethash project-name (clpi-source-project-ht source)))

(defmethod source-system ((source clpi-source) system-name)
  (gethash system-name (clpi-source-system-ht source)))

(defmethod source-to-form ((source clpi-source))
  (list (source-name source)
        :url (uri-to-string (source-url source))
        :type :clpi))


;;; * Projects

(defclass clpi-project ()
  ((source
    :initarg :source
    :reader clpi-project-source
    :reader project-source
    :documentation
    "The source of the project.")
   (name
    :initarg :name
    :accessor clpi-project-name
    :reader project-name
    :documentation
    "The name of the project.")
   (repo-type
    :initarg :repo-type
    :initform nil
    :accessor clpi-project-repo-type
    :documentation
    "The type of the upstream repo.")
   (repo-args
    :initarg :repo-args
    :initform nil
    :accessor clpi-project-repo-args)
   (release-ht
    :initform (make-hash-table :test 'equal)
    :reader clpi-project-release-ht
    :documentation
    "Maps version strings to ~clpi-release~ objects."))
  (:documentation
   "Represents a project in a CLPI source."))

(cpk:defencoding clpi-project
  name repo-type repo-args release-ht)

(defmethod cpk:decode-object :around ((class (eql 'clpi-project)) alist &key &allow-other-keys)
  (aprog1 (call-next-method)
    (setf (slot-value it 'source) *active-source*)))

(defmethod project-release ((p clpi-project) (version string))
  (gethash version (clpi-project-release-ht p)))

(defmethod project-releases ((p clpi-project))
  (hash-table-values (clpi-project-release-ht p)))

(defmethod project-repo ((p clpi-project))
  (let ((repo-type (clpi-project-repo-type p))
        (repo-args (clpi-project-repo-args p)))
    (make-repo-from-description (list* repo-type repo-args))))


;;; * Releases

(defclass clpi-release (tarball-release
                        simple-versioned-release)
  ((source
    :initarg :source
    :accessor clpi-release-source
    :reader release-source
    :documentation
    "The source of the release.")
   (project
    :initarg :project
    :accessor clpi-release-project
    :reader release-project
    :documentation
    "The project to which this release corresponds.")
   (version
    :reader release-version
    :initarg :version
    :documentation
    "The version of this release.")
   (prefix
    :accessor clpi-release-prefix
    :initarg :prefix
    :documentation
    "The prefix of all files in the tarball.")
   (url
    :accessor clpi-release-url
    :reader tarball-release/url
    :initarg :url
    :documentation
    "The URL where the tarball for this release is located.")
   (system-release-ht
    :initform (make-hash-table :test 'equal)
    :reader clpi-release-system-release-ht
    :documentation
    "Maps system names to ~clpi-system-release~ objects."))
  (:documentation
   "Represents a release of a project."))

(cpk:defencoding clpi-release
  project version prefix url system-release-ht)

(defmethod cpk:decode-object :around ((class (eql 'clpi-release)) alist &key &allow-other-keys)
  (aprog1 (call-next-method)
    (setf (slot-value it 'source) *active-source*)))

(defmethod release-lib-pathname ((r clpi-release))
  (uiop:resolve-absolute-location
   (list (source-lib-directory (release-source r))
         "projects"
         (project-name (release-project r))
         (clpi-release-prefix r))
   :ensure-directory t))

(defmethod release-system-releases ((release clpi-release))
  (hash-table-values (clpi-release-system-release-ht release)))


;;; * Systems

(defclass clpi-system ()
  ((source
    :initarg :source
    :accessor clpi-system-source
    :reader system-source
    :documentation
    "The source of the system.")
   (name
    :initarg :name
    :accessor system-name
    :documentation
    "The name of the system."))
  (:documentation
   "An ASD system contained in a CLPI source."))

(cpk:defencoding clpi-system
  name)

(defmethod cpk:decode-object :around ((class (eql 'clpi-system)) alist &key &allow-other-keys)
  (aprog1 (call-next-method)
    (setf (slot-value it 'source) *active-source*)))


;;; * System releases

(defclass clpi-system-release ()
  ((source
    :initarg :source
    :accessor clpi-system-release-source
    :reader system-release-source
    :documentation
    "The source of the system release.")
   (release
    :initarg :release
    :accessor clpi-system-release-release
    :reader system-release-release
    :documentation
    "The release to which the system release belongs.")
   (system
    :initarg :system
    :accessor clpi-system-release-system
    :reader system-release-system
    :documentation
    "The system.")
   (system-version
    :initarg :system-version
    :accessor clpi-system-release-system-version
    :documentation
    "The version of this system at this release.")
   (depends-on
    :initarg :depends-on
    :accessor clpi-system-release-depends-on
    :documentation
    "The list of dependencies this system has.")
   (system-file
    :initarg :system-file
    :accessor clpi-system-release-system-file
    :documentation
    "The asd file in which this system is located."))
  (:documentation
   "A release of a system."))

(cpk:defencoding clpi-system-release
  release system system-version depends-on system-file)

(defmethod cpk:decode-object :around ((class (eql 'clpi-system-release)) alist &key &allow-other-keys)
  (aprog1 (call-next-method)
    (setf (slot-value it 'source) *active-source*)))

(defmethod system-release-requirements ((system-release clpi-system-release))
  (let ((deps (remove-if (rcurry #'member (list "asdf" "uiop") :test #'string-equal)
                         (clpi-system-release-depends-on system-release))))
    (mapcar (lambda (dep-name)
              (make-instance 'system-requirement
                             :name dep-name))
            deps)))


;;; * Syncing

(defmethod sync-source ((source clpi-source))
  (let ((index-file (merge-pathnames "index.lisp"
                                     (source-cache-directory source))))
    (ensure-file-fetched index-file (clpi-source-index-url source))
    (when t ;;(ensure-file-fetched index-file (clpi-source-index-url source))
      (let ((*package* (find-package :clpm/sources/clpi-index-file))
            (*active-source* source))
        (load index-file)
        (save-clpi-source! source))))
  (values))

(defun ensure-project (source name repo)
  (aprog1
      (ensure-gethash name (clpi-source-project-ht source)
                      (make-instance 'clpi-project
                                     :source source
                                     :name name))
    (destructuring-bind (repo-type &rest repo-args) (or repo (list nil))
      (setf (clpi-project-repo-type it) repo-type
            (clpi-project-repo-args it) repo-args))))

(defun ensure-release (source project version prefix url)
  (aprog1
      (ensure-gethash version (clpi-project-release-ht project)
                      (make-instance 'clpi-release
                                     :source source
                                     :version version
                                     :project project))
    (setf (clpi-release-prefix it) prefix
          (clpi-release-url it) url)))

(defun ensure-system (source system-name)
  (ensure-gethash system-name (clpi-source-system-ht source)
                  (make-instance 'clpi-system
                                 :source source
                                 :name system-name)))

(defun ensure-system-release (source release system-name system-file system-version depends-on)
  (aprog1
      (ensure-gethash system-name (clpi-release-system-release-ht release)
                      (make-instance 'clpi-system-release
                                     :source source
                                     :release release))
    (setf (clpi-system-release-system-version it) system-version
          (clpi-system-release-depends-on it) depends-on
          (clpi-system-release-system it) (ensure-system source system-name)
          (clpi-system-release-system-file it) system-file)))

;; (defun ensure-system-release (release-id system-name system-file system-version depends-on)
;;   (let ((existing (find-dao 'clpi-system-release
;;                             :release-id release-id
;;                             :system-name system-name)))
;;     (when existing
;;       ;; Ensure the system release is up to date.
;;       (setf (clpi-system-release-system-file existing) system-file
;;             (clpi-system-release-system-version existing) system-version
;;             (clpi-system-release-depends-on existing) depends-on)
;;       (save-dao existing))
;;     (or existing
;;         (create-dao 'clpi-system-release
;;                     :release-id release-id
;;                     :system-name system-name
;;                     :system-file system-file
;;                     :system-version system-version
;;                     :depends-on depends-on))))

(defun ensure-project-defined (name &key repo releases)
  (let* ((name (string-downcase (string name)))
         (proj (ensure-project *active-source* name repo)))
    (dolist (r-desc releases)
      (destructuring-bind (version &key url tar-prefix ((:systems systems-desc)))
          r-desc
        (let ((release (ensure-release *active-source* proj version tar-prefix url)))
          (dolist (s-desc systems-desc)
            (destructuring-bind (system-name &key asd-pathname depends-on ((:version system-version)))
                s-desc
              (ensure-system-release *active-source*
                                     release
                                     system-name
                                     asd-pathname
                                     system-version
                                     depends-on))))))
    proj))

;; (defun ensure-project-defined (name &key repo releases)
;;   (with-source-connection (*active-source*)
;;     (let* ((name (string-downcase (string name)))
;;            (proj (ensure-project name repo)))
;;       (dolist (r-desc releases)
;;         (destructuring-bind (version &key url tar-prefix ((:systems systems-desc)))
;;             r-desc
;;           (let ((release (ensure-release name version tar-prefix url)))
;;             (dolist (s-desc systems-desc)
;;               (destructuring-bind (system-name &key asd-pathname depends-on ((:version system-version)))
;;                   s-desc
;;                 (ensure-system-release (object-id release) system-name
;;                                        asd-pathname
;;                                        system-version
;;                                        depends-on)
;;                 (or (find-dao 'clpi-system
;;                               :name system-name)
;;                     (create-dao 'clpi-system
;;                                 :name system-name)))))))
;;       proj)))

(in-package #:clpm/sources/clpi-index-file)

(defun version (version-string)
  (assert (equal version-string "0.1")))

(defmacro define-project (name &key repo releases)
  `(progn
     (clpm/sources/clpi::ensure-project-defined ,name
                                                :repo ',repo
                                                :releases ',releases)))
