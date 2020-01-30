;;;; Quicklisp based source
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

;; * define-package
(uiop:define-package #:clpm/sources/ql-flat
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/cache
          #:clpm/data
          #:clpm/requirement
          #:clpm/sources/defs
          #:clpm/sources/flat-file
          #:clpm/ql
          #:clpm/utils
          #:do-urlencode
          #:split-sequence)
  (:import-from #:puri)
  (:export #:ql-flat-source))

(in-package #:clpm/sources/ql-flat)


;; * Definitions
;; ** Source

(defclass ql-flat-source (flat-file-source)
  ((name
    :initarg :name
    :reader source-name)
   (url
    :initarg :url
    :accessor ql-flat-source-url)
   (force-https
    :initarg :force-https
    :initform nil
    :accessor ql-flat-source-force-https)))

(defmethod initialize-instance :after ((source ql-flat-source)
                                       &rest initargs
                                       &key url
                                         force-https)
  (declare (ignore initargs))
  (unless url
    (error "URL is required"))
  (let ((url url))
    (if (puri:uri-p url)
        (setf url (puri:copy-uri url))
        (setf url (puri:parse-uri url)))
    ;; If the URL is already https, set force https to be *at least*
    ;; :metadata-only.
    (when (and (not force-https)
               (eql (puri:uri-scheme url) :https))
      (setf force-https :metadata-only)
      (setf (ql-flat-source-force-https source) force-https))
    ;; If the scheme is not already https, set it to https if necessary.
    (ecase force-https
      (nil)
      ((t :metadata-only)
       (setf (puri:uri-scheme url) :https)))
    (setf (ql-flat-source-url source) url)))

;; ** Release

(defclass tarball-mixin ()
  ((url
    :initarg :url)
   (tar-prefix
    :initarg :tar-prefix)
   (size
    :initarg :size)
   (file-md5
    :initarg :file-md5)))

(defclass ql-flat-release (flat-file-release tarball-mixin)
  ())

;; ** System Release

(defclass ql-flat-system-release (flat-file-system-release)
  ())


;; * Quicklisp specific methods

(defun ql-flat-source-metadata (source)
  (let ((metadata-pathname (merge-pathnames "metadata.sexp"
                                            (source-lib-directory source))))
    (when (probe-file metadata-pathname)
      (uiop:with-safe-io-syntax ()
        (uiop:read-file-forms metadata-pathname)))))

(defun ql-flat-source-update-metadata (source &rest args &key &allow-other-keys)
  (let ((metadata-pathname (merge-pathnames "metadata.sexp"
                                            (source-lib-directory source)))
        (existing-metadata (or (ql-flat-source-metadata source)
                               (list (cons :api-version "0.1")))))
    (loop
      :for key :in args :by #'cddr
      :for value :in (rest args) :by #'cddr
      :do
         (setf (assoc-value existing-metadata key) value))
    (uiop:with-safe-io-syntax ()
      (with-open-file (s metadata-pathname
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
        (let ((*print-right-margin* nil)
              (*print-case* :downcase))
          (dolist (pair existing-metadata)
            (prin1 pair s)
            (terpri s)))))

    existing-metadata))

(defun ql-flat-source-latest-version-synced (source)
  (getf (ql-flat-source-metadata source) :latest-version-synced))


;; * Basic source methods

(defmethod release-> ((release-1 ql-flat-release)
                      (release-2 ql-flat-release))
  (string> (second (release-version release-1))
           (second (release-version release-2))))

(defmethod source-cache-directory ((source ql-flat-source))
  "Compute the cache location for this source, based on its canonical url."
  (let ((url (ql-flat-source-url source)))
    (clpm-cache-pathname
     `("sources"
       "quicklisp"
       ,(concatenate 'string
                     (string-downcase (puri:uri-host url))
                     "_"
                     (format nil "~d" (url-port url))
                     "_"
                     (urlencode (subseq (puri:uri-path url) 1))))
     :ensure-directory t)))

(defmethod source-lib-directory ((source ql-flat-source))
  "Compute the cache location for this source, based on its canonical url."
  (let ((url (ql-flat-source-url source)))
    (clpm-data-pathname
     `("sources"
       "quicklisp"
       ,(concatenate 'string
                     (string-downcase (puri:uri-host url))
                     "_"
                     (format nil "~d" (url-port url))
                     "_"
                     (urlencode (subseq (puri:uri-path url) 1))))
     :ensure-directory t)))

(defmethod source-type-keyword ((source ql-flat-source))
  :quicklisp)

(defmethod source-to-form ((source ql-flat-source))
  (list (source-name source)
        :url (uri-to-string (ql-flat-source-url source))
        :type :quicklisp
        :force-https (ql-flat-source-force-https source)))

(defmethod system-release-> ((sr-1 ql-flat-system-release) (sr-2 ql-flat-system-release))
  (release-> (system-release-release sr-1) (system-release-release sr-2)))

(defmethod system-release-requirements ((system-release ql-flat-system-release))
  (let ((deps (remove-if (rcurry #'member (list "asdf" "uiop") :test #'string-equal)
                         (flat-file-system-release-dependencies system-release))))
    (mapcar (lambda (dep-name)
              (make-instance 'system-requirement
                             :name dep-name))
            deps)))

(defmethod system-release-satisfies-version-spec-p ((system-release ql-flat-system-release)
                                                    version-spec)
  "There is currently no good way to reasonably get the system version from the
  metadata alone, so say everything is satisfied."
  t)


;; * Flat file methods

(defmethod flat-file-source-root-pathname ((source ql-flat-source))
  (merge-pathnames "repo/"
                   (source-lib-directory source)))

(defmethod flat-file-source-release-class ((source ql-flat-source))
  'ql-flat-release)

(defmethod flat-file-source-system-release-class ((source ql-flat-source))
  'ql-flat-system-release)

(defmethod source-project :around ((source ql-flat-source) project-name &optional (error t))
  (declare (ignore error))
  (restart-case
      (call-next-method)
    (sync-and-retry (c)
      :report "Sync and try again"
      (declare (ignore c))
      (sync-source source)
      (call-next-method))))



;; * Syncing

(defstruct ql-sync-state
  (project-index-map nil
   :read-only t)
  (project-detail-map (make-hash-table :test 'equal)
   :read-only t)
  (system-detail-map (make-hash-table :test 'equal)
   :read-only t)
  (previous-dist-version nil))

(defun ql-sync-version (sync-state repo version)
  (let* ((dv (ql-repo-dist-version repo version))
         (release-map (ql-dist-version-release-map dv))
         (system-map (ql-dist-version-system-by-name-map dv))
         (modified-projects nil))

    (maphash (lambda (project-name release)
               (let ((url-string (uri-to-string (ql-release-url release)))
                     (previous-release (when (ql-sync-state-previous-dist-version sync-state)
                                         (ql-dist-version-release
                                          (ql-sync-state-previous-dist-version sync-state)
                                          project-name nil))))

                 (when (or (null previous-release)
                           (not (equal (ql-release-file-md5 release)
                                       (ql-release-file-md5 previous-release))))
                   (push project-name modified-projects)
                   (let ((project-version (list :snapshot version)))
                     (push (list project-version
                                 :url url-string
                                 :tar-prefix (ql-release-prefix release)
                                 :size (ql-release-size release)
                                 :file-md5 (ql-release-file-md5 release)
                                 :system-files (ql-release-system-files release)
                                 :systems (mapcar #'ql-system-system-name
                                                  (gethash project-name
                                                           (ql-dist-version-system-by-project-map dv))))
                           (gethash project-name (ql-sync-state-project-detail-map sync-state)))
                     (push project-version
                           (getf (gethash project-name (ql-sync-state-project-index-map sync-state))
                                 :releases))))))
             release-map)

        (maphash (lambda (system-name system)
                     (let ((project-name (ql-system-project system)))
                       (when (member project-name modified-projects :test #'equal)
                         (let* ((short-system-file (ql-system-system-file system))
                                (release (gethash project-name release-map))
                                (release-system-files (ql-release-system-files release))
                                (long-system-file (find-if (lambda (full-name)
                                                             (equal (pathname-name full-name)
                                                                    short-system-file))
                                                           release-system-files)))
                           (unless long-system-file
                             (error "Unable to determine system file path for system"))

                           (push (list* (list (ql-system-project system)
                                              (list :snapshot version))
                                        ;; TODO: releases.txt and systems.txt give
                                        ;; different file names...
                                        :system-file long-system-file
                                        (awhen (ql-system-dependencies system)
                                          (list :dependencies it)))
                                 (gethash system-name (ql-sync-state-system-detail-map sync-state)))))))
                 system-map)

    (setf (ql-sync-state-previous-dist-version sync-state) dv)))

(defun write-project-index (map pathname)
  (uiop:with-safe-io-syntax ()
    (let ((keys (sort (hash-table-keys map) #'string<))
          (*print-right-margin* nil)
          (*print-case* :downcase))
      (with-open-file (s pathname
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
        (dolist (key keys)
          (prin1 (list key (gethash key map)) s)
          (terpri s))))))

(defun write-project-details (map pathname)
  (ensure-directories-exist pathname)
  (uiop:with-safe-io-syntax ()
    (let ((*print-right-margin* nil)
          (*print-case* :downcase))
      (maphash (lambda (project-name details)
                 (let ((releases-pathname
                         (merge-pathnames (concatenate 'string project-name "/releases") pathname)))
                   (ensure-directories-exist releases-pathname)
                   (with-open-file (s releases-pathname
                                      :direction :output
                                      :if-exists :append
                                      :if-does-not-exist :create)
                     (dolist (detail (reverse details))
                       (prin1 detail s)
                       (terpri s)))))
               map))))

(defun write-system-details (map pathname)
  (ensure-directories-exist pathname)
  (uiop:with-safe-io-syntax ()
    (let ((*print-right-margin* nil)
          (*print-case* :downcase))
      (maphash (lambda (system-name details)
                 (with-open-file (s (merge-pathnames (urlencode system-name) pathname)
                                    :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
                   (dolist (detail (reverse details))
                     (prin1 detail s)
                     (terpri s))))
               map))))

(defun ql-flat-load-existing-project-index (source)
  (let ((pathname (merge-pathnames "repo/project-index.txt" (source-lib-directory source)))
        (out (make-hash-table :test 'equal)))
    (when (probe-file pathname)
      (uiop:with-safe-io-syntax ()
        (with-open-file (s pathname)
          (loop
            (let ((form (read s nil :eof)))
              (when (eql form :eof)
                (return))
              (setf (gethash (first form) out) (second form)))))))
    out))

(defmethod sync-source ((source ql-flat-source))
  ;; Create an object to interact with the remote repo.
  (let* ((sync-cache-pathname (merge-pathnames "remote-repo/"
                                               (source-cache-directory source)))
         (sync-lib-pathname (merge-pathnames "repo/"
                                             (source-lib-directory source)))
         (repo (make-instance 'ql-repo
                              :url (ql-flat-source-url source)
                              :force-https (ql-flat-source-force-https source)
                              :cache-pathname sync-cache-pathname)))
    (ensure-directories-exist sync-cache-pathname)
    (ensure-directories-exist sync-lib-pathname)
    (let ((latest-version-synced (ql-flat-source-latest-version-synced source))
          (sync-state (make-ql-sync-state
                       :project-index-map (ql-flat-load-existing-project-index source)))
          (projects-pathname (merge-pathnames "project-index.txt" sync-lib-pathname)))
      (when latest-version-synced
        (setf (ql-sync-state-previous-dist-version sync-state)
              (ql-repo-dist-version repo latest-version-synced)))
      ;; Loop over every version of this repo
      (let ((versions (ql-repo-versions repo)))
        (dolist (version versions)
          (unless (and latest-version-synced
                       (string<= version latest-version-synced))
            (ql-sync-version sync-state repo version))))

      (write-project-index (ql-sync-state-project-index-map sync-state)
                           projects-pathname)
      (write-project-details (ql-sync-state-project-detail-map sync-state)
                             (merge-pathnames "projects/" sync-lib-pathname))
      (write-system-details (ql-sync-state-system-detail-map sync-state)
                            (merge-pathnames "systems/" sync-lib-pathname))
      (let ((prev-latest-version-synced latest-version-synced)
            (latest-version-synced (ql-dist-version-version
                                    (ql-sync-state-previous-dist-version sync-state))))
        (ql-flat-source-update-metadata source :latest-version-synced latest-version-synced)
        (not (equal latest-version-synced prev-latest-version-synced))))))
