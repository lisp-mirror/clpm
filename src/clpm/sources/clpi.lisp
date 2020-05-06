;;;; CLPI sources
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

;; * define-package
(uiop:define-package #:clpm/sources/clpi
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/cache
          #:clpm/config
          #:clpm/data
          #:clpm/http-client
          #:clpm/install/defs
          #:clpm/repos
          #:clpm/requirement
          #:clpm/sources/defs
          #:clpm/sources/tarball-release
          #:clpm/sources/vcs
          #:clpm/utils
          #:clpm/version-strings
          #:do-urlencode)
  (:import-from #:clpi)
  (:export #:clpi-backing-object
           #:clpi-dual-source
           #:clpi-release
           #:clpi-source
           #:clpi-source-index
           #:clpi-source-release-class))

(in-package #:clpm/sources/clpi)


;; * Source

(defclass clpi-source (clpm-source)
  ((name
    :initarg :name
    :reader source-name)
   (url
    :initarg :url
    :accessor source-url)
   (index
    :accessor clpi-source-index)
   (local-index
    :accessor clpi-source-local-index)
   (system-ht
    :initform (make-hash-table :test 'equal)
    :reader clpi-source-system-ht)
   (project-ht
    :initform (make-hash-table :test 'equal)
    :reader clpi-source-project-ht)))

(defclass clpi-dual-source (clpi-source)
  ())

(defmethod make-source ((type (eql 'clpi-dual-source)) &rest initargs
                        &key url name &allow-other-keys)
  (let ((url-string (if (stringp url) url (uri-to-string url))))
    (ensure-gethash (list type name url-string) *source-cache*
                    (apply #'make-instance
                           type
                           initargs))))

(defmethod initialize-instance :after ((source clpi-source) &rest initargs
                                       &key url)
  (declare (ignore initargs))
  (unless url
    (error "URL is required"))
  (let ((url url))
    (if (puri:uri-p url)
        (setf url (puri:copy-uri url))
        (setf url (puri:parse-uri url)))
    (setf (source-url source) url))
  (setf (clpi-source-local-index source)
        (make-instance 'clpi:file-index
                       :root (merge-pathnames "clpi/"
                                              (source-lib-directory source)))))

(defmethod initialize-instance :after ((source clpi-dual-source)
                                       &rest initargs
                                       &key url installed-only-p)
  (declare (ignore initargs))
  (let ((file-index (make-instance 'clpi:file-index
                                   :root (merge-pathnames
                                          "clpi/"
                                          (if installed-only-p
                                              (source-lib-directory source)
                                              (source-cache-directory source))))))
    (setf (clpi-source-index source)
          (if (or (config-value :local) installed-only-p)
              file-index
              (make-instance 'clpi:dual-index
                             :primary (make-instance 'clpi:http-index
                                                     :http-client (get-http-client)
                                                     :url url)
                             :secondary file-index)))))

(defmethod source-cache-directory ((source clpi-source))
  "Compute the cache location for this source, based on its canonical url."
  (let ((url (source-url source)))
    (clpm-cache-pathname
     `("sources"
       "clpi"
       ,(apply #'concatenate 'string
               (string-downcase (puri:uri-host url))
               "_"
               (format nil "~d" (url-port url))
               (awhen (puri:uri-path url)
                 (list "_"
                       (urlencode (subseq it 1))))))
     :ensure-directory t)))

(defmethod source-ensure-system ((source clpi-source) system-name)
  (or (source-system source system-name nil)
      (setf (gethash system-name (clpi-source-system-ht source))
            (make-instance 'vcs-system
                           :name system-name
                           :source source))))

(defmethod source-can-lazy-sync-p ((source clpi-dual-source))
  t)

(defmethod source-lib-directory ((source clpi-source))
  "Compute the cache location for this source, based on its canonical url."
  (let ((url (source-url source)))
    (clpm-data-pathname
     `("sources"
       "clpi"
       ,(apply #'concatenate 'string
               (string-downcase (puri:uri-host url))
               "_"
               (format nil "~d" (url-port url))
               (awhen (puri:uri-path url)
                 (list "_"
                       (urlencode (subseq it 1))))))
     :ensure-directory t)))

(defun ensure-local-project (source project)
  (let ((local-index (clpi-source-local-index source))
        (backing-project (clpi-backing-object project)))
    (or (clpi:index-project local-index (clpi:project-name backing-project) nil)
        (let ((new-project
                (make-instance (clpi:index-project-class local-index)
                               :index local-index
                               :name (clpi:project-name backing-project)
                               :version-scheme (clpi:project-version-scheme backing-project))))
          (clpi:index-add-project local-index new-project)
          new-project))))

(defun ensure-local-system (source system-name)
  (let ((local-index (clpi-source-local-index source)))
    (or (clpi:index-system local-index system-name nil)
        (let ((new-system
                (make-instance (clpi:index-system-class local-index)
                               :index local-index
                               :name system-name)))
          (clpi:index-add-system local-index new-system)
          new-system))))

(defmethod source-project ((source clpi-source) name &optional (error t))
  (ensure-gethash name (clpi-source-project-ht source)
                  (let ((index-project (clpi:index-project (clpi-source-index source)
                                                           name nil)))
                    (if index-project
                        (make-instance 'clpi-project
                                       :source source
                                       :backing-object index-project)
                        (when error
                          (error 'source-missing-project
                                 :source source
                                 :project-name name))))))

(defmethod source-system ((source clpi-source) system-name &optional (error t))
  (ensure-gethash system-name (clpi-source-system-ht source)
                  (let ((index-system (clpi:index-system (clpi-source-index source)
                                                         system-name nil)))
                    (if index-system
                        (make-instance 'clpi-system
                                       :source source
                                       :backing-object index-system)
                        (when error
                          (error 'source-missing-system
                                 :source source
                                 :system-name system-name))))))

(defmethod source-type-keyword ((source clpi-source))
  :clpi)

(defmethod source-to-form ((source clpi-source))
  (list (source-name source)
        :url (uri-to-string (source-url source))
        :type (source-type-keyword source)))

(defmethod sync-source ((source clpi-dual-source))
  (clpi:dual-index-sync (clpi-source-index source)))

(defgeneric clpi-source-release-class (source))

(defmethod clpi-source-release-class ((source clpi-source))
  'clpi-release)


;; * Project

(defclass clpi-backed-object ()
  ((backing-object
    :initarg :backing-object
    :reader clpi-backing-object)))

(defclass clpi-project (clpi-backed-object)
  ((source
    :initarg :source
    :reader project-source)
   (release-ht
    :initform (make-hash-table :test 'equal)
    :reader clpi-project-release-ht)
   (repo
    :reader project-repo)
   (vcs-release-ht
    :initform (make-hash-table :test 'equal)
    :reader clpi-project-vcs-release-ht)))

(defmethod project-name ((project clpi-project))
  (clpi:project-name (clpi-backing-object project)))

(defmethod project-release ((project clpi-project) (version-string list) &optional error)
  (declare (ignore error))
  (apply #'project-vcs-release project version-string))

(defmethod project-release ((project clpi-project) version-string &optional (error t))
  (ensure-gethash version-string (clpi-project-release-ht project)
                  (let ((index-release (clpi:project-release (clpi-backing-object project)
                                                             version-string nil)))
                    (if index-release
                        (make-instance (clpi-source-release-class (project-source project))
                                       :source (project-source project)
                                       :project project
                                       :backing-object index-release)
                        (when error
                          (error 'project-missing-version
                                 :source (project-source project)
                                 :version version-string
                                 :project project))))))

(defmethod project-releases ((project clpi-project))
  (mapcar (curry #'project-release project)
          (clpi:project-versions (clpi-backing-object project))))

(defmethod slot-unbound (class (project clpi-project) (slot-name (eql 'repo)))
  (let (repo
        (clpi-repo (clpi:project-repo (clpi-backing-object project))))
    (when clpi-repo
      (setf repo (make-repo-from-description (clpi:repo-to-description clpi-repo))))
    (setf (slot-value project slot-name)
          repo)))

(defmethod project-vcs-release ((project clpi-project) &key commit branch tag)
  (let* ((ref (cond
                (commit `(:commit ,commit))
                (branch `(:branch ,branch))
                (tag `(:tag ,tag))))
         (release (ensure-gethash ref (clpi-project-vcs-release-ht project)
                                  (make-vcs-release (project-source project) project ref))))
    (unless commit
      (setf release (ensure-gethash `(:commit ,(vcs-release-commit release))
                                    (clpi-project-vcs-release-ht project)
                                    release)))
    release))



;; * Release

(defclass clpi-release (clpm-release
                        clpi-backed-object
                        tarball-release)
  ((source
    :initarg :source
    :reader release-source)
   (project
    :initarg :project
    :reader release-project)
   (system-release-ht
    :initform (make-hash-table :test 'equal)
    :reader release-system-release-ht)
   (system-file-ht
    :initform (make-hash-table :test 'equal)
    :reader release-system-file-ht)))

(defmethod release-> ((release-1 clpi-release)
                      (release-2 clpi-release))
  (string> (release-version release-1)
           (release-version release-2)))

(defmethod release-satisfies-version-spec-p ((release clpi-release) version-spec)
  (let ((scheme (clpi:project-version-scheme (clpi:project (clpi-backing-object release))))
        (version (release-version release)))
    (case scheme
      (:date
       (version-spec-satisfied-p/simple-string version-spec version))
      (:semver
       (version-spec-satisfied-p/semantic version-spec version))
      (t
       t))))

(defmethod release-system-file ((release clpi-release) system-file-namestring)
  (ensure-gethash system-file-namestring (release-system-file-ht release)
                  (make-instance 'clpi-system-file
                                 :source (release-source release)
                                 :release release
                                 :backing-object (clpi:release-system-file
                                                  (clpi-backing-object release)
                                                  system-file-namestring))))

(defmethod release-system-files ((release clpi-release))
  (mapcar (curry #'release-system-file release)
          (clpi:release-system-file-namestrings (clpi-backing-object release))))

(defmethod release-system-release ((release clpi-release) system-name &optional error)
  (ensure-gethash system-name (release-system-release-ht release)
                  (let ((index-system-release (clpi:release-system-release (clpi-backing-object release)
                                                                           system-name
                                                                           nil)))
                    (if index-system-release
                        (make-instance 'clpi-system-release
                                       :source (release-source release)
                                       :backing-object index-system-release
                                       :release release)
                        (when error
                          (error 'release-missing-system-release
                                 :source (release-source release)
                                 :release release
                                 :system-name system-name))))))

(defmethod release-systems ((release clpi-release))
  (mapcar (curry #'source-system
                 (release-source release))
          (clpi:release-system-names (clpi-backing-object release))))

(defmethod release-version ((release clpi-release))
  (clpi:release-version (clpi-backing-object release)))

(defmethod tarball-release-url ((release clpi-release))
  (clpi:release-url (clpi-backing-object release)))

(defmethod tarball-release-desired-md5 ((release clpi-release))
  (clpi:release-md5 (clpi-backing-object release)))

(defmethod tarball-release-desired-size ((release clpi-release))
  (clpi:release-size (clpi-backing-object release)))

(defun copy-clpi-release (clpi-release clpi-project)
  (apply #'make-instance
         (clpi:project-release-class (clpi:project clpi-release))
         :version (clpi:release-version clpi-release)
         :project clpi-project
         (clpi:release-plist clpi-release)))

(defmethod install-release ((release clpi-release))
  (when (call-next-method)
    (let* ((source (release-source release))
           (project (release-project release))
           (local-index (clpi-source-local-index source))
           (local-project (ensure-local-project source project))
           (local-release (copy-clpi-release (clpi-backing-object release) local-project)))
      (clpi:project-add-release local-project local-release)
      (dolist (sr (clpi:release-system-releases local-release))
        (clpi:system-add-system-release (ensure-local-system source (clpi:system-release-system-name sr))
                                        sr))
      (clpi:index-save local-index))
    t))


;; * System

(defclass clpi-system (clpi-backed-object)
  ((source
    :initarg :source
    :reader system-source)
   (vcs-releases
    :initform (make-hash-table :test 'equal)
    :reader clpi-system-vcs-releases)))

(defmethod system-name ((system clpi-system))
  (clpi:system-name (clpi-backing-object system)))

(defmethod system-system-releases ((system clpi-system))
  (let* ((clpi-releases (clpi:system-releases (clpi-backing-object system) nil))
         (releases (mapcar (lambda (r)
                             (source-project-release (system-source system)
                                                     (clpi:project-name (clpi:project r))
                                                     (clpi:release-version r)))
                           clpi-releases)))
    (append (mapcar (lambda (r) (release-system-release r (system-name system))) releases)
            (hash-table-values (clpi-system-vcs-releases system)))))

(defmethod system-register-release! ((system clpi-system) (release vcs-release))
  (setf (gethash (release-version release) (clpi-system-vcs-releases system))
        release))


;; * System file

(defclass clpi-system-file (clpi-backed-object)
  ((source
    :initarg :source
    :reader system-file-source)
   (release
    :initarg :release
    :reader system-file-release)))

(defmethod system-file-absolute-asd-pathname ((system-file clpi-system-file))
  (merge-pathnames (system-file-asd-enough-namestring system-file)
                   (release-lib-pathname (system-file-release system-file))))

(defmethod system-file-asd-enough-namestring ((system-file clpi-system-file))
  (clpi:system-file-enough-namestring (clpi-backing-object system-file)))


;; * System release

(defclass clpi-system-release (clpi-backed-object
                                  clpm-system-release)
  ((source
    :initarg :source
    :reader system-release-source)
   (release
    :initarg :release
    :reader system-release-release)))

(defmethod system-release-> ((sr-1 clpi-system-release) (sr-2 clpi-system-release))
  (release-> (system-release-release sr-1) (system-release-release sr-2)))

(defmethod system-release-asd-pathname ((system-release clpi-system-release))
  (clpi:system-file-enough-namestring
   (clpi:system-release-system-file
    (clpi-backing-object system-release))))

(defmethod system-release-requirements ((system-release clpi-system-release))
  (mapcar #'convert-asd-system-spec-to-req
          (clpi:system-release-dependencies (clpi-backing-object system-release))))

(defmethod system-release-satisfies-version-spec-p ((system-release clpi-system-release)
                                                    version-spec)
  (let ((system-version (clpi:system-release-version (clpi-backing-object system-release))))
    (or (null system-version)
        (version-spec-satisfied-p/dotted version-spec system-version))))

(defmethod system-release-system ((system-release clpi-system-release))
  (source-system (system-release-source system-release)
                 (clpi:system-release-system-name (clpi-backing-object system-release))))
