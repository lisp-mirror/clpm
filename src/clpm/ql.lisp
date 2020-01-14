(uiop:define-package #:clpm/ql
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/http-client
          #:clpm/utils
          #:do-urlencode
          #:puri
          #:split-sequence)
  (:import-from #:puri)
  (:import-from #:trivial-garbage)
  (:export #:ql-dist-version-release
           #:ql-dist-version-release-map
           #:ql-dist-version-system-by-name-map
           #:ql-dist-version-system-by-project-map
           #:ql-dist-version-version
           #:ql-release-file-md5
           #:ql-release-prefix
           #:ql-release-size
           #:ql-release-system-files
           #:ql-release-url
           #:ql-repo
           #:ql-repo-dist-version
           #:ql-repo-versions
           #:ql-system-dependencies
           #:ql-system-project
           #:ql-system-system-file
           #:ql-system-system-name))

(in-package #:clpm/ql)

(defclass ql-repo ()
  ((url
    :accessor ql-repo-url)
   (force-https
    :initarg :force-https
    :initform nil
    :accessor ql-repo-force-https)
   (cache-pathname
    :initarg :cache-pathname
    :initform (error "A cache pathname must be provided")
    :accessor ql-repo-cache-pathname)
   (versions-url
    :accessor ql-repo-versions-url)
   (version-urls-map
    :reader ql-repo-version-urls-map)

   (dist-version-cache
    :initform (tg:make-weak-hash-table :test 'equal :weakness :value)
    :reader ql-repo-dist-version-cache)))

(defmethod initialize-instance :after ((repo ql-repo) &rest initargs
                                       &key
                                         url
                                         force-https
                                         cache-pathname
                                       &allow-other-keys)
  (declare (ignore initargs))
  (let ((url url))
    (if (puri:uri-p url)
        (setf url (puri:copy-uri url))
        (setf url (puri:parse-uri url)))
    ;; If the URL is already https, set force https to be *at least*
    ;; :metadata-only.
    (when (and (not force-https)
               (eql (puri:uri-scheme url) :https))
      (setf force-https :metadata-only)
      (setf (ql-repo-force-https repo) force-https))
    ;; If the scheme is not already https, set it to https if necessary.
    (ecase force-https
      (nil)
      ((t :metadata-only)
       (setf (puri:uri-scheme url) :https)))
    ;; Compute the versions url
    (let* ((path (puri:uri-path url))
           (file-name (pathname-name path)))
      (setf (ql-repo-versions-url repo)
            (puri:merge-uris (namestring
                              (merge-pathnames (concatenate 'string file-name "-versions")
                                               path))
                             url)))
    (unless (uiop:directory-pathname-p cache-pathname)
      (error "cache pathname must name a directory"))
    (ensure-directories-exist cache-pathname)
    (setf (ql-repo-url repo) url)))

(defmethod slot-unbound (class (repo ql-repo) (slot-name (eql 'version-urls-map)))
  ;; Fetch the versions file and parse it.
  (let ((versions-pathname (merge-pathnames "versions.txt"
                                            (ql-repo-cache-pathname repo))))
    (ensure-file-fetched versions-pathname (ql-repo-versions-url repo))
    (let ((ht (make-hash-table :test 'equal)))
      (with-open-file (s versions-pathname)
        (loop
          (let ((line (read-line s nil :eof)))
            (when (or (eql line :eof) (equal line ""))
              (return))
            (destructuring-bind (version url)
                (split-sequence #\Space line)
              (setf url (puri:parse-uri url))
              (when (ql-repo-force-https repo)
                (setf (puri:uri-scheme url) :https))
              (setf (gethash version ht) url)))))
      (setf (slot-value repo 'version-urls-map) ht))))

(defun ql-repo-versions (repo)
  (sort (hash-table-keys (ql-repo-version-urls-map repo))
        #'string<))

(defun ql-repo-dist-version (repo version)
  (ensure-gethash version (ql-repo-dist-version-cache repo)
                  (let ((url (gethash version (ql-repo-version-urls-map repo))))
                    (unless url
                      (error "No such version"))
                    (make-instance 'ql-dist-version
                                   :repo repo
                                   :version version
                                   :url url))))

(defmacro do-dist-versions ((binding repo) &body body)
  (with-gensyms (v)
    (once-only (repo)
      `(dolist (,v (ql-repo-versions ,repo))
         (let ((,binding (ql-repo-dist-version ,repo ,v)))
           ,@body)))))



(defclass ql-dist-version ()
  ((repo
    :initarg :repo
    :reader ql-dist-version-repo)
   (version
    :initarg :version
    :reader ql-dist-version-version)
   (url
    :initarg :url
    :reader ql-dist-version-url)

   (system-index-url
    :reader ql-dist-version-system-index-url)
   (release-index-url
    :reader ql-dist-version-release-index-url)

   (release-map
    :reader ql-dist-version-release-map)
   (system-by-name-map
    :reader ql-dist-version-system-by-name-map)
   (system-by-project-map
    :reader ql-dist-version-system-by-project-map)))

(defun ql-dist-version-pathname (dist-version file)
  (merge-pathnames file
                   (uiop:ensure-directory-pathname
                    (merge-pathnames (ql-dist-version-version dist-version)
                                     (ql-repo-cache-pathname
                                      (ql-dist-version-repo dist-version))))))

(defun parse-distinfo-line (line)
  "Parse a single line of a distinfo.txt file."
  (let* ((colon-pos (position #\: line))
         (property (make-keyword (uiop:standard-case-symbol-name
                                  (subseq line 0 colon-pos))))
         (value (subseq line (+ 2 colon-pos))))
    (list property value)))

(defun parse-distinfo (distinfo-pathname)
  "Given a distinfo.txt pathname, return a plist of its contents."
  (let* ((contents (read-file-into-string distinfo-pathname))
         (lines (split-sequence #\Newline contents :remove-empty-subseqs t)))
    (mapcan #'parse-distinfo-line lines)))


(defun populate-ql-dist-version! (dist-version)
  (let ((distinfo-pathname (ql-dist-version-pathname dist-version "distinfo.txt")))
    (ensure-file-fetched distinfo-pathname (ql-dist-version-url dist-version))
    (let ((plist (parse-distinfo distinfo-pathname)))
      (let ((system-index-url (puri:parse-uri (getf plist :system-index-url)))
            (release-index-url (puri:parse-uri (getf plist :release-index-url))))
        (when (ql-repo-force-https (ql-dist-version-repo dist-version))
          (setf (puri:uri-scheme system-index-url) :https
                (puri:uri-scheme release-index-url) :https))
        (setf (slot-value dist-version 'system-index-url) system-index-url
              (slot-value dist-version 'release-index-url) release-index-url))))
  dist-version)

(defmethod slot-unbound (class (dist-version ql-dist-version) (slot-name (eql 'system-index-url)))
  (populate-ql-dist-version! dist-version)
  (ql-dist-version-system-index-url dist-version))

(defmethod slot-unbound (class (dist-version ql-dist-version) (slot-name (eql 'release-index-url)))
  (populate-ql-dist-version! dist-version)
  (ql-dist-version-release-index-url dist-version))


(defun populate-ql-dist-version-releases! (dist-version)
  (let ((ht (make-hash-table :test 'equal))
        (releases-pathname (ql-dist-version-pathname dist-version "releases.txt")))
    (ensure-file-fetched releases-pathname (ql-dist-version-release-index-url dist-version))
    (with-open-file (s releases-pathname)
      (loop
        (let ((line (read-line s nil :eof)))
          (when (eql line :eof)
            (return))
          ;; Skip empty lines and comments
          (unless (or (equal line "")
                      (equal (aref line 0) #\#))
            (destructuring-bind (project url size file-md5 content-sha1 prefix &rest system-files)
                (split-sequence #\Space line)
              (setf url (puri:parse-uri url))
              (when (eql t (ql-repo-force-https (ql-dist-version-repo dist-version)))
                (setf (puri:uri-scheme url) :https))
              (setf (gethash project ht)
                    (make-instance 'ql-release
                                   :dist-version dist-version
                                   :project project
                                   :url url
                                   :size size
                                   :file-md5 file-md5
                                   :content-sha1 content-sha1
                                   :prefix prefix
                                   :system-files system-files)))))))
    (setf (slot-value dist-version 'release-map) ht)))

(defmethod slot-unbound (class (dist-version ql-dist-version) (slot-name (eql 'release-map)))
  (populate-ql-dist-version-releases! dist-version)
  (slot-value dist-version 'release-map))

(defmethod populate-ql-dist-version-systems! (dist-version)
  (let ((ht-by-system-name (make-hash-table :test 'equal))
        (ht-by-project (make-hash-table :test 'equal))
        (systems-pathname (ql-dist-version-pathname dist-version "systems.txt")))
    (ensure-file-fetched systems-pathname (ql-dist-version-system-index-url dist-version))
    (with-open-file (s systems-pathname)
      (loop
        (let ((line (read-line s nil :eof)))
          (when (eql line :eof)
            (return))
          ;; Skip empty lines and comments
          (unless (or (equal line "")
                      (equal (aref line 0) #\#))
            (destructuring-bind (project system-file system-name &rest dependencies)
                (split-sequence #\Space line)
              (let ((system (make-instance 'ql-system
                                           :dist-version dist-version
                                           :project project
                                           :system-file system-file
                                           :system-name system-name
                                           :dependencies dependencies)))
                (setf (gethash system-name ht-by-system-name) system)
                (push system (gethash project ht-by-project))))))))
    (setf (slot-value dist-version 'system-by-name-map) ht-by-system-name
          (slot-value dist-version 'system-by-project-map) ht-by-project)))

(defmethod slot-unbound (class (dist-version ql-dist-version) (slot-name (eql 'system-by-name-map)))
  (populate-ql-dist-version-systems! dist-version)
  (slot-value dist-version 'system-by-name-map))

(defmethod slot-unbound (class (dist-version ql-dist-version) (slot-name (eql 'system-by-project-map)))
  (populate-ql-dist-version-systems! dist-version)
  (slot-value dist-version 'system-by-project-map))

(defun ql-dist-version-release (dist-version project-name
                                &optional (missing-error-p t) missing-value)
  (or (gethash project-name (ql-dist-version-release-map dist-version))
      (and missing-error-p (error "project missing"))
      missing-value))



(defclass ql-release ()
  ((dist-version
    :initarg :dist-version
    :reader ql-release-dist-version)

   (project
    :initarg :project
    :reader ql-release-project)
   (url
    :initarg :url
    :reader ql-release-url)
   (size
    :initarg :size
    :reader ql-release-size)
   (file-md5
    :initarg :file-md5
    :reader ql-release-file-md5)
   (content-sha1
    :initarg :content-sha1
    :reader ql-release-content-sha1)
   (prefix
    :initarg :prefix
    :reader ql-release-prefix)
   (system-files
    :initarg :system-files
    :reader ql-release-system-files)))



(defclass ql-system ()
  ((dist-version
    :initarg :dist-version
    :reader ql-system-dist-version)

   (project
    :initarg :project
    :reader ql-system-project)
   (system-file
    :initarg :system-file
    :reader ql-system-system-file)
   (system-name
    :initarg :system-name
    :reader ql-system-system-name)
   (dependencies
    :initarg :dependencies
    :reader ql-system-dependencies)))
