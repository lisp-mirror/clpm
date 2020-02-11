;;;; Quicklisp based source
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

;; * define-package
(uiop:define-package #:clpm/sources/quicklisp
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/cache
          #:clpm/data
          #:clpm/requirement
          #:clpm/sources/defs
          #:clpm/sources/flat-file
          #:clpm/sources/tarball-release
          #:clpm/ql
          #:clpm/utils
          #:clpm/version-strings
          #:do-urlencode
          #:split-sequence)
  (:import-from #:puri)
  (:export #:ql-flat-source))

(in-package #:clpm/sources/quicklisp)


;; * Definitions
;; ** Source

(defclass ql-flat-source (ff-source)
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

;; ** Project

(defclass ql-flat-project (ff-project)
  ((snapshot-to-ql-version-map
    :accessor ql-flat-project-snapshot-to-ql-version-map)))

(defmethod slot-unbound (class (project ql-flat-project) (slot-name (eql 'snapshot-to-ql-version-map)))
  (let ((map (make-hash-table :test 'equal)))
    (uiop:with-safe-io-syntax ()
      (with-open-file (s (merge-pathnames
                          (make-pathname :directory (list :relative "projects" (project-name project))
                                         :name "dist-to-snapshot")
                          (ff-source-repo-pathname (project-source project))))
        (loop
          (let ((form (read s nil :eof)))
            (when (eql form :eof)
              (return))
            (push (car form) (gethash (cdr form) map))))))
    (setf (slot-value project slot-name) map)
    map))

;; ** Release

(defclass ql-flat-release (ff-release
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

(defclass ql-flat-system-release (ff-system-release)
  ())


;; * Quicklisp specific methods

(defun ql-flat-release-quicklisp-versions (release)
  (let ((project (release-project release)))
    (gethash (release-version release)
             (ql-flat-project-snapshot-to-ql-version-map project))))

(defun intersection-non-empty-p (set1 set2 &key test)
  (some (rcurry #'member set2 :test test) set1))

(defun ql-system-release-overlapping-snapshots (system-release system)
  "Given a system-release, return a list of versions of system that are present
in the same Quicklisp distribution versions as system-release."
  ;; First find all Quicklisp distribution versions containing the
  ;; system-release. Then, iterate over all releases of the system to find the
  ;; ones that are also present in at least one of the same distribution
  ;; versions.
  (let* ((release (system-release-release system-release))
         (quicklisp-versions (ql-flat-release-quicklisp-versions release))
         (potential-releases (system-releases system)))
    (mapcar #'release-version (remove-if-not (rcurry #'intersection-non-empty-p
                                                     quicklisp-versions
                                                     :test #'equal)
                                             potential-releases
                                             :key #'ql-flat-release-quicklisp-versions))))


;; * Basic source methods

(defmethod release-> ((release-1 ql-flat-release)
                      (release-2 ql-flat-release))
  (string> (release-version release-1)
           (release-version release-2)))

(defmethod release-satisfies-version-spec-p ((release ql-flat-release)
                                             version-spec)
  (version-spec-satisfied-p/simple-string version-spec (release-version release)))

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
                         (ff-system-release-dependencies system-release))))
    (mapcar (lambda (dep-name)
              (make-instance 'system-requirement
                             :name dep-name))
            deps)))

(defun %system-release-satisfies-version-spec-p-1 (system-release version-spec)
  (declare (ignore system-release version-spec))
  ;; There is currently no good way to reasonably get the system version from
  ;; the metadata alone, so say everything is satisfied.
  t)

(defmethod system-release-satisfies-version-spec-p ((system-release ql-flat-system-release)
                                                    version-spec)
  "There is currently no good way to reasonably get the system version from the
  metadata alone, so say everything is satisfied."
  (every (curry #'%system-release-satisfies-version-spec-p-1 system-release) version-spec))


;; * Flat file methods

(defmethod ff-source-project-class ((source ql-flat-source))
  'ql-flat-project)

(defmethod ff-source-release-class ((source ql-flat-source))
  'ql-flat-release)

(defmethod ff-source-system-release-class ((source ql-flat-source))
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
  (system-index-map nil
   :read-only t)
  (project-detail-map (make-hash-table :test 'equal)
   :read-only t)
  (system-detail-map (make-hash-table :test 'equal)
   :read-only t)
  ;; Maps project names to the last snapshot version.
  (last-snapshot-seen-map (make-hash-table :test 'equal)
   :read-only t)
  ;; Maps project names to an alist that maps quicklisp dist versions to the
  ;; corresponding snapshot of the project.
  (dist-to-snapshot-map (make-hash-table :test 'equal))
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
                   (let ((project-version version))
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
                                 :releases))
                     (setf (gethash project-name (ql-sync-state-last-snapshot-seen-map sync-state))
                           project-version)))
                 ;; Map this quicklisp dist version to the latest known snapshot
                 (setf (assoc-value (gethash project-name (ql-sync-state-dist-to-snapshot-map sync-state))
                                    version :test 'equal)
                       (gethash project-name (ql-sync-state-last-snapshot-seen-map sync-state)))))
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
                           ;; This madness is because releases.txt and
                           ;; systems.txt give different strings for the system files...
                           (unless long-system-file
                             (error "Unable to determine system file path for system"))

                           (pushnew project-name
                                    (getf (gethash system-name
                                                   (ql-sync-state-system-index-map sync-state))
                                          :projects)
                                    :test #'equal)
                           (push (list (ql-system-project system)
                                       version)
                                 (getf (gethash system-name
                                                (ql-sync-state-system-index-map sync-state))
                                       :releases))
                           (push (list* (list (ql-system-project system)
                                              version)
                                        :system-file long-system-file
                                        (awhen (ql-system-dependencies system)
                                          (list :dependencies it)))
                                 (gethash system-name (ql-sync-state-system-detail-map sync-state)))))))
                 system-map)

    (setf (ql-sync-state-previous-dist-version sync-state) dv)))

(defun write-index (map pathname)
  (ensure-directories-exist pathname)
  (uiop:with-safe-io-syntax ()
    (let ((keys (sort (hash-table-keys map) #'string<))
          (*print-right-margin* nil)
          (*print-case* :downcase))
      (with-open-file (s pathname
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
        (dolist (key keys)
          (prin1 (list* key (gethash key map)) s)
          (terpri s))))))

(defun write-project-details (map dist-to-snapshot-map source)
  (uiop:with-safe-io-syntax ()
    (let ((*print-right-margin* nil)
          (*print-case* :downcase))
      (maphash (lambda (project-name details)
                 (let ((releases-pathname
                         (merge-pathnames "releases"
                                          (ff-source-repo-project-pathname source project-name))))
                   (ensure-directories-exist releases-pathname)
                   (with-open-file (s releases-pathname
                                      :direction :output
                                      :if-exists :append
                                      :if-does-not-exist :create)
                     (dolist (detail (reverse details))
                       (prin1 detail s)
                       (terpri s)))))
               map)
      (maphash (lambda (project-name dist-to-snapshot-alist)
                 (let ((dist-to-snapshot-pathname
                         (merge-pathnames "dist-to-snapshot"
                                          (ff-source-repo-project-pathname source project-name))))
                   (ensure-directories-exist dist-to-snapshot-pathname)
                   (with-open-file (s dist-to-snapshot-pathname
                                      :direction :output
                                      :if-exists :append
                                      :if-does-not-exist :create)
                     (dolist (pair (reverse dist-to-snapshot-alist))
                       (prin1 pair s)
                       (terpri s)))))
               dist-to-snapshot-map))))

(defun write-system-details (map source)
  (uiop:with-safe-io-syntax ()
    (let ((*print-right-margin* nil)
          (*print-case* :downcase))
      (maphash (lambda (system-name details)
                 (let ((pn (merge-pathnames "releases"
                                            (ff-source-repo-system-pathname source system-name))))
                   (ensure-directories-exist pn)
                   (with-open-file (s pn
                                    :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
                     (dolist (detail (reverse details))
                       (prin1 detail s)
                       (terpri s)))))
               map))))

(defun ql-flat-load-existing-project-index (source)
  (let ((pathname (ff-source-repo-project-index-pathname source))
        (out (make-hash-table :test 'equal)))
    (when (probe-file pathname)
      (uiop:with-safe-io-syntax ()
        (with-open-file (s pathname)
          (loop
            (let ((form (read s nil :eof)))
              (when (eql form :eof)
                (return))
              (setf (gethash (first form) out) (rest form)))))))
    out))

(defun ql-flat-load-existing-system-index (source)
  (let ((pathname (ff-source-repo-system-index-pathname source))
        (out (make-hash-table :test 'equal)))
    (when (probe-file pathname)
      (uiop:with-safe-io-syntax ()
        (with-open-file (s pathname)
          (loop
            (let ((form (read s nil :eof)))
              (when (eql form :eof)
                (return))
              (setf (gethash (first form) out) (rest form)))))))
    out))

(defmethod sync-source ((source ql-flat-source))
  ;; Create an object to interact with the remote repo.
  (let* ((sync-cache-pathname (merge-pathnames "remote-repo/"
                                               (source-cache-directory source)))
         (repo (make-instance 'ql-repo
                              :url (ql-flat-source-url source)
                              :force-https (ql-flat-source-force-https source)
                              :cache-pathname sync-cache-pathname))
         (project-index-map (ql-flat-load-existing-project-index source))
         (system-index-map (ql-flat-load-existing-system-index source)))
    (ensure-directories-exist sync-cache-pathname)
    (let ((latest-version-synced (ff-source-metadata source :latest-version-synced))
          (sync-state (make-ql-sync-state
                       :project-index-map project-index-map
                       :system-index-map system-index-map))
          (project-index-pathname (ff-source-repo-project-index-pathname source))
          (system-index-pathname (ff-source-repo-system-index-pathname source)))
      (when latest-version-synced
        (setf (ql-sync-state-previous-dist-version sync-state)
              (ql-repo-dist-version repo latest-version-synced))
        ;; Save the latest snapshot version for each project.
        (maphash (lambda (project-name index)
                   (setf (gethash project-name (ql-sync-state-last-snapshot-seen-map sync-state))
                         (first (getf index :releases))))
                 project-index-map))
      ;; Loop over every version of this repo
      (let ((versions (ql-repo-versions repo)))
        (dolist (version versions)
          (unless (and latest-version-synced
                       (string<= version latest-version-synced))
            (ql-sync-version sync-state repo version))))

      (write-index (ql-sync-state-project-index-map sync-state)
                   project-index-pathname)
      (write-index (ql-sync-state-system-index-map sync-state)
                   system-index-pathname)
      (write-project-details (ql-sync-state-project-detail-map sync-state)
                             (ql-sync-state-dist-to-snapshot-map sync-state)
                             source)
      (write-system-details (ql-sync-state-system-detail-map sync-state)
                            source)
      (let ((prev-latest-version-synced latest-version-synced)
            (latest-version-synced (ql-dist-version-version
                                    (ql-sync-state-previous-dist-version sync-state))))
        (setf (ff-source-metadata source :latest-version-synced) latest-version-synced)
        (not (equal latest-version-synced prev-latest-version-synced))))))
