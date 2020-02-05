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
          #:do-urlencode
          #:split-sequence)
  (:import-from #:puri)
  (:export #:clpi-source))

(in-package #:clpm/sources/clpi)


;; * Definitions
;; ** Source

(defclass clpi-source (flat-file-source)
  ((name
    :initarg :name
    :reader source-name)
   (url
    :initarg :url
    :accessor source-url)))

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

(defclass clpi-release (flat-file-release
                        tarball-release-with-md5
                        tarball-release-with-size)
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

(defclass clpi-system-release (flat-file-system-release)
  ())


;; * Quicklisp specific methods

;; (defun ql-flat-release-quicklisp-versions (release)
;;   (let ((project (release-project release)))
;;     (gethash (release-version release)
;;              (ql-flat-project-snapshot-to-ql-version-map project))))

;; (defun ql-flat-source-metadata (source)
;;   (let ((metadata-pathname (flat-file-source-repo-metadata-pathname source)))
;;     (when (probe-file metadata-pathname)
;;       (uiop:with-safe-io-syntax ()
;;         (uiop:read-file-forms metadata-pathname)))))

;; (defun ql-flat-source-update-metadata (source &rest args &key &allow-other-keys)
;;   (let ((metadata-pathname (flat-file-source-repo-metadata-pathname source))
;;         (existing-metadata (or (ql-flat-source-metadata source)
;;                                (list (cons :api-version "0.1")))))
;;     (loop
;;       :for key :in args :by #'cddr
;;       :for value :in (rest args) :by #'cddr
;;       :do
;;          (setf (assoc-value existing-metadata key) value))
;;     (uiop:with-safe-io-syntax ()
;;       (with-open-file (s metadata-pathname
;;                          :direction :output
;;                          :if-exists :supersede
;;                          :if-does-not-exist :create)
;;         (let ((*print-right-margin* nil)
;;               (*print-case* :downcase))
;;           (dolist (pair existing-metadata)
;;             (prin1 pair s)
;;             (terpri s)))))

;;     existing-metadata))

;; (defun ql-flat-source-latest-version-synced (source)
;;   (assoc-value (ql-flat-source-metadata source) :latest-version-synced))

;; (defun intersection-non-empty-p (set1 set2 &key test)
;;   (some (rcurry #'member set2 :test test) set1))

;; (defun ql-system-release-overlapping-snapshots (system-release system)
;;   "Given a system-release, return a list of versions of system that are present
;; in the same Quicklisp distribution versions as system-release."
;;   ;; First find all Quicklisp distribution versions containing the
;;   ;; system-release. Then, iterate over all releases of the system to find the
;;   ;; ones that are also present in at least one of the same distribution
;;   ;; versions.
;;   (let* ((release (system-release-release system-release))
;;          (quicklisp-versions (ql-flat-release-quicklisp-versions release))
;;          (potential-releases (system-releases system)))
;;     (mapcar #'release-version (remove-if-not (rcurry #'intersection-non-empty-p
;;                                                      quicklisp-versions
;;                                                      :test #'equal)
;;                                              potential-releases
;;                                              :key #'ql-flat-release-quicklisp-versions))))


;; * Basic source methods

(defmethod release-> ((release-1 clpi-release)
                      (release-2 clpi-release))
  (string> (release-version release-1)
           (release-version release-2)))

(defmethod source-cache-directory ((source clpi-source))
  "Compute the cache location for this source, based on its canonical url."
  (let ((url (source-url source)))
    (clpm-cache-pathname
     `("sources"
       "clpi"
       ,(concatenate 'string
                     (string-downcase (puri:uri-host url))
                     "_"
                     (format nil "~d" (url-port url))
                     "_"
                     (urlencode (subseq (puri:uri-path url) 1))))
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
                         (flat-file-system-release-dependencies system-release))))
    (mapcar (lambda (dep-name)
              (make-instance 'system-requirement
                             :name dep-name))
            deps)))

(defun %system-release-satisfies-version-spec-p-1 (system-release version-spec)
  (declare (ignore system-release version-spec))
  ;; There is currently no good way to reasonably get the system version from
  ;; the metadata alone, so say everything is satisfied.
  t)

(defmethod system-release-satisfies-version-spec-p ((system-release clpi-system-release)
                                                    version-spec)
  "There is currently no good way to reasonably get the system version from the
  metadata alone, so say everything is satisfied."
  (every (curry #'%system-release-satisfies-version-spec-p-1 system-release) version-spec))


;; * Flat file methods

(defmethod flat-file-source-repo-pathname ((source clpi-source))
  (merge-pathnames "repo/"
                   (source-lib-directory source)))

(defmethod flat-file-source-release-class ((source clpi-source))
  'clpi-release)

(defmethod flat-file-source-system-release-class ((source clpi-source))
  'clpi-system-release)

;; (defmethod source-project :around ((source ql-flat-source) project-name &optional (error t))
;;   (declare (ignore error))
;;   (restart-case
;;       (call-next-method)
;;     (sync-and-retry (c)
;;       :report "Sync and try again"
;;       (declare (ignore c))
;;       (sync-source source)
;;       (call-next-method))))



;; * Syncing

(defmethod sync-source ((source clpi-source))
  (let* ((root-url (source-url source))
         (base-url (puri:merge-uris "package-index/v0.3/" root-url))
         (project-index-pn (flat-file-source-repo-project-index-pathname source))
         (system-index-pn (flat-file-source-repo-system-index-pathname source))
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
              (let ((url (puri:merge-uris (uiop:strcat "projects/" project-name "/releases")
                                          base-url))
                    (pn (merge-pathnames "releases"
                                         (flat-file-source-repo-project-pathname source project-name))))
                (ensure-file-fetched pn url)))))

        (with-open-file (s system-index-pn)
          (with-forms-from-stream (s form)
            (destructuring-bind (system-name &rest args) form
              (declare (ignore args))
              (let ((url (puri:merge-uris (uiop:strcat "systems/" system-name "/releases")
                                          base-url))
                    (pn (merge-pathnames "releases"
                                         (flat-file-source-repo-system-pathname source system-name))))
                (ensure-file-fetched pn url))))))
      t)))
