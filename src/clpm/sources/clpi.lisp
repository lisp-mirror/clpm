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
          #:clpm/requirement
          #:clpm/sources/defs
          #:clpm/sources/tarball-release
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
   (index
    :accessor clpi-source-index)
   (system-ht
    :initform (make-hash-table :test 'equal)
    :reader clpi-source-system-ht)
   (project-ht
    :initform (make-hash-table :test 'equal)
    :reader clpi-source-project-ht)))

(defclass clpi-dual-source (clpi-source)
  ((url
    :initarg :url
    :accessor source-url)))

(defmethod make-source ((type (eql 'clpi-dual-source)) &rest initargs &key url name)
  (let ((url-string (if (stringp url) url (uri-to-string url))))
    (ensure-gethash (list type name url-string) *source-cache*
                    (apply #'make-instance
                           type
                           initargs))))

(defmethod initialize-instance :after ((source clpi-dual-source)
                                       &rest initargs
                                       &key url)
  (declare (ignore initargs))
  (unless url
    (error "URL is required"))
  (let ((url url))
    (if (puri:uri-p url)
        (setf url (puri:copy-uri url))
        (setf url (puri:parse-uri url)))
    (setf (source-url source) url))
  (let ((file-index (make-instance 'clpi:file-index
                                   :root (merge-pathnames
                                          "clpi/"
                                          (source-cache-directory source)))))
    (setf (clpi-source-index source)
          (if (config-value :local)
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
    :reader clpi-project-release-ht)))

(defmethod project-name ((project clpi-project))
  (clpi:project-name (clpi-backing-object project)))

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


;; * Release

(defclass clpi-release (clpi-backed-object
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


;; * System

(defclass clpi-system (clpi-backed-object)
  ((source
    :initarg :source
    :reader system-source)))

(defmethod system-name ((system clpi-system))
  (clpi:system-name (clpi-backing-object system)))

(defmethod system-system-releases ((system clpi-system))
  (let* ((clpi-releases (clpi:system-releases (clpi-backing-object system) nil))
         (releases (mapcar (lambda (r)
                             (source-project-release (system-source system)
                                                     (clpi:project-name (clpi:project r))
                                                     (clpi:release-version r)))
                           clpi-releases)))
    (mapcar (lambda (r) (release-system-release r (system-name system))) releases)))


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
