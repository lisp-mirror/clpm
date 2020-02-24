;;;; CLPI derived sources
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

;; * define-package
(uiop:define-package #:clpm/sources/clpi
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/cache
          #:clpm/data
          #:clpm/http-client
          #:clpm/requirement
          #:clpm/sources/defs
          #:clpm/sources/flat-file
          #:clpm/sources/tarball-release
          #:clpm/utils
          #:clpm/version-strings
          #:do-urlencode
          #:split-sequence)
  (:import-from #:puri)
  (:export #:clpi-source))

(in-package #:clpm/sources/clpi)


;; * Definitions
;; ** Source

(defclass clpi-source (ff-source)
  ((name
    :initarg :name
    :reader source-name)
   (url
    :initarg :url
    :accessor source-url)))

(defmethod make-source ((type (eql 'clpi-source)) &rest initargs &key url name)
  (let ((url-string (if (stringp url) url (uri-to-string url))))
    (ensure-gethash (list type name url-string) *source-cache*
                    (apply #'make-instance
                           type
                           initargs))))

(defmethod initialize-instance :after ((source clpi-source)
                                       &rest initargs
                                       &key url)
  (declare (ignore initargs))
  (unless url
    (error "URL is required"))
  (let ((url url))
    (if (puri:uri-p url)
        (setf url (puri:copy-uri url))
        (setf url (puri:parse-uri url)))
    (setf (source-url source) url)))

;; ** Release

(defclass clpi-release (ff-release
                        tarball-release)
  ((url
    :initarg :url
    :reader tarball-release/url)
   (tar-prefix
    :initarg :tar-prefix
    :reader tarball-release/prefix)
   (size
    :initarg :size
    :reader tarball-release/desired-size)
   (file-md5
    :initarg :file-md5
    :reader tarball-release/desired-md5)))

;; ** System Release

(defclass clpi-system-release (ff-system-release)
  ())


;; * Basic source methods

(defmethod release-> ((release-1 clpi-release)
                      (release-2 clpi-release))
  (string> (release-version release-1)
           (release-version release-2)))

(defmethod release-satisfies-version-spec-p ((release clpi-release)
                                             version-spec)
  (version-spec-satisfied-p/semantic version-spec (release-version release)))

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

(defmethod source-type-keyword ((source clpi-source))
  :clpi)

(defmethod source-to-form ((source clpi-source))
  (list (source-name source)
        :url (uri-to-string (source-url source))
        :type :clpi))

(defmethod system-release-> ((sr-1 clpi-system-release) (sr-2 clpi-system-release))
  (release-> (system-release-release sr-1) (system-release-release sr-2)))

(defmethod system-release-requirements ((system-release clpi-system-release))
  (let ((deps (remove-if (rcurry #'member (list "asdf" "uiop") :test #'string-equal)
                         (ff-system-release-dependencies system-release))))
    (mapcar (lambda (dep-name)
              (make-instance 'system-requirement
                             :name dep-name))
            deps)))

(defmethod system-release-satisfies-version-spec-p ((system-release clpi-system-release)
                                                    version-spec)
  (version-spec-satisfied-p/semantic version-spec (system-release-system-version system-release)))


;; * Flat file methods

(defmethod ff-source-release-class ((source clpi-source))
  'clpi-release)

(defmethod ff-source-system-release-class ((source clpi-source))
  'clpi-system-release)

(defmethod source-project :around ((source clpi-source) project-name &optional (error t))
  (declare (ignore error))
  (restart-case
      (call-next-method)
    (sync-and-retry (c)
      :report "Sync and try again"
      (declare (ignore c))
      (sync-source source)
      (call-next-method))))



;; * Syncing

(defmethod sync-source ((source clpi-source))
  (let* ((root-url (source-url source))
         (base-url (puri:merge-uris "package-index/v0.3/" root-url))
         (project-index-pn (ff-source-repo-project-index-pathname source))
         (system-index-pn (ff-source-repo-system-index-pathname source))
         (index-modified-p nil))

    (when (ensure-file-fetched project-index-pn
                               (puri:merge-uris "project-index" base-url))
      (setf index-modified-p t))
    (when (ensure-file-fetched system-index-pn
                               (puri:merge-uris "system-index" base-url))
      (setf index-modified-p t))

    (when index-modified-p
      (uiop:with-safe-io-syntax ()
        (with-open-file (s project-index-pn)
          (with-forms-from-stream (s form)
            (destructuring-bind (project-name &rest args) form
              (declare (ignore args))
              (let ((releases-url (puri:merge-uris (uiop:strcat "projects/" project-name "/releases")
                                                   base-url))
                    (releases-pn (merge-pathnames "releases"
                                                  (ff-source-repo-project-pathname source project-name)))
                    (metadata-url (puri:merge-uris (uiop:strcat "projects/" project-name "/metadata")
                                                   base-url))
                    (metadata-pn (merge-pathnames "metadata"
                                                  (ff-source-repo-project-pathname source project-name))))
                (ensure-file-fetched releases-pn releases-url)
                (ensure-file-fetched metadata-pn metadata-url)))))

        (with-open-file (s system-index-pn)
          (with-forms-from-stream (s form)
            (destructuring-bind (system-name &rest args) form
              (declare (ignore args))
              (let ((url (puri:merge-uris (uiop:strcat "systems/" system-name "/releases")
                                          base-url))
                    (pn (merge-pathnames "releases"
                                         (ff-source-repo-system-pathname source system-name))))
                (ensure-file-fetched pn url))))))
      t)))
