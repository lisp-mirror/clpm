(uiop:define-package #:clpm/sources/quicklisp
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/cache
          #:clpm/fetch
          #:clpm/requirement
          #:clpm/sources/defs
          #:clpm/sources/simple-versioned-project
          #:clpm/version-strings
          #:puri
          #:split-sequence)
  (:export #:quicklisp-source))

(in-package #:clpm/sources/quicklisp)

;;; Quicklisp's


;;; A quicklisp based source

(defclass quicklisp-source (clpm-known-source)
  ((force-https
    :initarg :force-https
    :initform nil
    :accessor ql-force-https)
   (curation-p
    :initarg :curation-p
    :initform nil
    :accessor ql-curation-p)
   (versions-url
    :accessor source/versions-url)
   (projects
    :initform nil
    :accessor source/raw-projects)
   (systems
    :initform nil
    :accessor source/raw-systems)
   (db
    :accessor source/db)))

(defun quicklisp-source-tester (url contents)
  (declare (ignore url))
  (when (and (starts-with-subseq "name:" contents)
             (search (format nil "~%version:") contents)
             (search (format nil "~%distinfo-subscription-url:") contents)
             (search (format nil "~%release-index-url:") contents)
             (search (format nil "~%system-index-url:") contents))
    (values 'quicklisp-source `(:name ,(getf (parse-distinfo contents) :name)))))

(register-source-test-function 'quicklisp-source-tester)

(defun save-db (source)
  (let ((db-file (merge-pathnames "db.sexp" (source/cache-directory source))))
    (with-open-file (s db-file :direction :output :if-exists :supersede)
      (uiop:with-safe-io-syntax ()
        (print (source/db source) s)))))

(defmethod slot-unbound (class (source quicklisp-source) (slot-name (eql 'db)))
  "Lazily read the DB from file."
  (let ((db-file (merge-pathnames "db.sexp" (source/cache-directory source))))
    (when (probe-file db-file)
      (setf (source/db source)
            (uiop:with-safe-io-syntax ()
              (uiop:read-file-form db-file))))))

(defmethod slot-unbound (class (source quicklisp-source) (slot-name (eql 'versions-url)))
  (let* ((path (puri:uri-path (source/url source)))
         (file-name (pathname-name path)))
    (setf (source/versions-url source)
          (merge-uris (namestring
                       (merge-pathnames (concatenate 'string file-name "-versions")
                                        path))
                      (source/url source)))))

(defmethod source-type-keyword ((source quicklisp-source))
  :quicklisp)

(defmethod source-to-form ((source quicklisp-source))
  (list (source/name source)
        :url (uri-to-string (source/url source))
        :type :quicklisp
        :force-https (ql-force-https source)))

(defun source/dist-versions (source)
  (let ((db (source/db source)))
    (getf db :versions)))

(defun ensure-projects (source)
  (mapc (curry #'source/project source) (source/project-names source)))

(defun source/project-names (source)
  (mapcar #'car (getf (source/db source) :releases)))

(defmethod source/projects ((source quicklisp-source))
  (ensure-projects source)
  (mapcar #'cdr (source/raw-projects source)))

(defmethod source/project ((source quicklisp-source) project-name)
  (let ((project (assoc-value (source/raw-projects source) project-name :test #'equal)))
    (when (and (not project)
               (member project-name (source/project-names source)
                       :test #'equal))
      (setf project
            (make-instance 'quicklisp-project :source source :name project-name))
      (setf (assoc-value (source/raw-projects source) project-name :test #'equal)
            project))
    project))

(defun ensure-systems (source)
  (mapc (curry #'source/system source) (source/system-names source)))

(defun source/system-names (source)
  (mapcar #'car (getf (source/db source) :systems)))

(defmethod source/systems ((source quicklisp-source))
  (ensure-systems source)
  (mapcar #'cdr (source/raw-systems source)))

(defmethod source/system ((source quicklisp-source) system-name)
  (let ((system (assoc-value (source/raw-systems source) system-name :test #'equal)))
    ;; (when (not system)
    ;;   (break "~S" system))
    (when (and (not system)
               (member system-name (source/system-names source)
                       :test #'equal))
      (setf system
            (make-instance 'quicklisp-system :source source :name system-name))
      (setf (assoc-value (source/raw-systems source) system-name :test #'equal)
            system))
    system))


;;; A system provided by quicklisp

(defclass quicklisp-system (clpm-system)
  ())

(defun system-data (system)
  (assoc-value (getf (source/db (system/source system)) :systems)
               (system/name system)
               :test #'equal))

(defmethod system/releases ((system quicklisp-system))
  (mapcar (lambda (pair)
            (destructuring-bind (project-name . project-version)
                pair
              (project/release (source/project (system/source system) project-name)
                               project-version)))
          (system-data system)))

(defmethod system/system-releases ((system quicklisp-system))
  (loop
    :for r :in (system/releases system)
    :collecting (release/system-release r (system/name system))))


;;; A project provided by quicklisp

(defclass quicklisp-project (clpm-project)
  ((repo
    :initform nil)
   (raw-releases
    :initform nil
    :accessor project/raw-releases)))

(defun project-data (project)
  (assoc-value (getf (source/db (project/source project)) :releases)
               (project/name project)
               :test #'equal))

(defun ensure-releases (project)
  (mapc (curry #'project/release project) (project/version-strings project)))

(defmethod project/release ((project quicklisp-project) version-string)
  (let ((release (assoc-value (project/raw-releases project) version-string :test #'equal)))
    (unless release
      (setf release
            (make-instance 'quicklisp-release :source (project/source project)
                                              :project project
                                              :version version-string))
      (setf (assoc-value (project/raw-releases project) version-string :test #'equal)
            release))
    release))

(defmethod project/releases ((project quicklisp-project))
  (ensure-releases project)
  (mapcar #'cdr (project/raw-releases project)))

(defun project/version-strings (project)
  (sort (loop
          :for version-alist :in (getf (project-data project) :versions)
          :appending (car version-alist))
        #'string>))


;;; Releases

(defclass quicklisp-release (tarball-release simple-versioned-release)
  ((system-files-ht
    :accessor quicklisp-release/system-files-ht
    :documentation "Hash table that maps system file names to system-file objects.")))

(defmethod slot-unbound (class (release quicklisp-release) (slot-name (eql 'urls)))
  (let ((release-data (release-data release)))
    (setf (tarball-release/urls release)
          (list (aprog1
                    (parse-uri (getf release-data :url))
                  (when (ql-force-https (release/source release))
                    (setf (uri-scheme it) :https)))))))

(defmethod slot-unbound (class (release quicklisp-release) (slot-name (eql 'system-files-ht)))
  (let* ((ht (make-hash-table :test 'equalp))
         (release-data (release-data release))
         (system-file-strings (getf release-data :system-files)))
    (dolist (system-file-string system-file-strings)
      (setf (gethash system-file-string ht) (make-instance 'quicklisp-system-file
                                                           :release release
                                                           :source (release/source release)
                                                           :enough-pathname system-file-string)))
    (setf (quicklisp-release/system-files-ht release) ht))
  (quicklisp-release/system-files-ht release))

(defun release-data (release)
  (cdr (find-if (lambda (x)
                  (member (release/version release) (car x)
                          :test #'string-equal))
                (getf (project-data (release/project release)) :versions))))

(defun release-prefix (release)
  (let ((release-data (release-data release)))
    (getf release-data :prefix)))

(defmethod release/systems ((release quicklisp-release))
  (let* ((source (release/source release))
         (release-data (release-data release))
         (system-names (mapcar #'car (getf release-data :systems))))
    (mapcar (curry #'source/system source) system-names)))

(defmethod release/system-file ((release quicklisp-release) system-file-namestring)
  (gethash system-file-namestring (quicklisp-release/system-files-ht release)))

(defmethod release/system-files ((release quicklisp-release))
  (hash-table-values (quicklisp-release/system-files-ht release)))

(defmethod release/system-release ((r quicklisp-release) system-name)
  (make-instance 'quicklisp-system-release
                 :source (release/source r)
                 :release r
                 :system (source/system (release/source r) system-name)))

(defmethod release/lib-pathname ((r quicklisp-release))
  (uiop:resolve-absolute-location
     (list (source/lib-directory (release/source r))
           "projects"
           (project/name (release/project r))
           (release-prefix r))
     :ensure-directory t))


;;; System files
(defclass quicklisp-system-file (clpm-system-file)
  ((enough-pathname
    :initarg :enough-pathname
    :accessor system-file/asd-enough-namestring)))

(defmethod system-file/absolute-asd-pathname ((system-file quicklisp-system-file))
  (let* ((release (system-file/release system-file))
         (system-release (release/system-release release
                                                 (pathname-name
                                                  (system-file/asd-enough-namestring system-file)))))
    (system-release/absolute-asd-pathname system-release)))


;;; Release systems

(defclass quicklisp-system-release (clpm-system-release)
  ())

(defmethod system-release-satisfies-version-spec-p ((system-release quicklisp-system-release)
                                                    version-spec)
  "There is currently no way to get reasonably get the system version from
quicklisp metadata. So say everything is satisfied."
  t)

(defmethod system-release/asd-pathname ((system-release quicklisp-system-release))
  (let* ((release (system-release/release system-release))
         (release-data (release-data release))
         (system-files (getf release-data :system-files))
         (system (assoc-value (getf release-data :systems)
                              (system/name (system-release/system system-release))
                              :test #'string-equal)))
    (find (getf system :system-file) system-files
          :key #'pathname-name
          :test #'string-equal)))

(defmethod system-release/requirements ((system-release quicklisp-system-release))
  (let* ((source (system-release/source system-release))
         (release (system-release/release system-release))
         (release-data (release-data release))
         (system-release-plist (assoc-value (getf release-data :systems)
                                            (system/name (system-release/system system-release))
                                            :test #'string-equal))
         (deps (remove-if (rcurry #'member (list "asdf" "uiop") :test #'string-equal)
                          (getf system-release-plist :deps))))
    (mapcar (lambda (dep-name)
              (if (ql-curation-p source)
                  (let* ((dep-system (source/system source dep-name))
                         (dep-releases (system/releases dep-system))
                         (dep-release (find (release/version release)
                                            dep-releases
                                            :key #'release/version
                                            :test #'string-equal)))

                    (make-instance 'project-requirement
                                   :name (project/name (release/project dep-release))
                                   :version-spec `(= . ,(release/version release))))
                  (make-instance 'system-requirement
                                 :name dep-name)))
            deps)))

(defmethod system-release/system-file ((system-release quicklisp-system-release))
  (gethash (system-release/asd-pathname system-release)
           (quicklisp-release/system-files-ht (system-release/release system-release))))

(defmethod system-release-> ((sr-1 quicklisp-system-release) (sr-2 quicklisp-system-release))
  (release-> (system-release/release sr-1) (system-release/release sr-2)))


;;; Syncing the source

(defun parse-distinfo-line (line)
  "Parse a single line of a distinfo.txt file."
  (let* ((colon-pos (position #\: line))
         (property (make-keyword (uiop:standard-case-symbol-name
                                  (subseq line 0 colon-pos))))
         (value (subseq line (+ 2 colon-pos))))
    (list property value)))

(defun parse-distinfo (distinfo)
  "Givean a distinfo.txt file as a string, return a plist of its contents."
  (let* ((lines (split-sequence #\Newline distinfo :remove-empty-subseqs t)))
    (mapcan #'parse-distinfo-line lines)))

(defun parse-releases (releases)
  (let* ((lines (rest (split-sequence #\Newline releases :remove-empty-subseqs t))))
    (mapcar (lambda (l)
              (split-sequence #\Space l :remove-empty-subseqs t))
            lines)))

(defun parse-systems (systems)
  (let* ((lines (rest (split-sequence #\Newline systems :remove-empty-subseqs t))))
    (mapcar (lambda (l)
              (split-sequence #\Space l :remove-empty-subseqs t))
            lines)))

(defun latest-all-versions (source)
  "Fetch the latest versions for a source. Returns an alist mapping version
strings to distinfo.txt urls."
  (let* ((versions-string (fetch-url (source/versions-url source)))
         (versions-lines (split-sequence #\Newline versions-string :remove-empty-subseqs t)))
    (mapcar (lambda (l)
              (apply #'cons (split-sequence #\Space l)))
            versions-lines)))

(defun fetch-distribution-version (dest-dir distinfo-url)
  "Given the URL to a distinfo.txt file, save it and all related files for this
version of the distribution to the directory specified by DEST-DIR."
  ;; Fetch the distinfo.txt file.
  (let ((distinfo-pathname (merge-pathnames "distinfo.txt"
                                            dest-dir)))
    (ensure-file-fetched distinfo-pathname
                         distinfo-url)
    (let ((distinfo-plist (parse-distinfo (read-file-into-string distinfo-pathname))))
      (destructuring-bind (&key system-index-url
                             release-index-url
                             &allow-other-keys)
          distinfo-plist
        (assert system-index-url)
        (assert release-index-url)
        ;; Fetch the system and release index files.
        (let ((system-index-pathname (merge-pathnames "systems.txt"
                                                      dest-dir))
              (release-index-pathname (merge-pathnames "releases.txt"
                                                       dest-dir)))
          (ensure-file-fetched system-index-pathname system-index-url)
          (ensure-file-fetched release-index-pathname release-index-url)
          (values))))))

(defun make-db-for-dist-version (cache-dir version distinfo-url)
  (let* ((version-cache-dir (uiop:resolve-location `(,cache-dir
                                                     "versions"
                                                     ,version)
                                                   :ensure-directory t))
         (system-index (merge-pathnames "systems.txt"
                                         version-cache-dir))
         (release-index (merge-pathnames "releases.txt" version-cache-dir)))
    (fetch-distribution-version version-cache-dir
                                distinfo-url)
    (let ((systems (parse-systems (read-file-into-string system-index)))
          (releases (parse-releases (read-file-into-string release-index)))
          (out nil))
      (dolist (r releases)
        (destructuring-bind (project-name url size file-md5 content-sha1 prefix &rest system-files)
            r
          (push (cons project-name
                      (list :url url
                            :size (parse-integer size)
                            :file-md5 file-md5
                            :content-sha1 content-sha1
                            :prefix prefix
                            :system-files system-files))
                out)))
      (dolist (s systems)
        (destructuring-bind (project-name system-file system-name &rest deps)
            s
          (let ((project-pair (assoc project-name out :test #'string-equal)))
            (push (cons system-name
                        (list :system-file system-file
                              :deps deps))
                  (getf (cdr project-pair) :systems)))))
      (dolist (p out)
        (setf (cdr p)
              (list :versions (list (cons (list version)
                                          (cdr p))))))
      (nreverse out))))

(defun assemble-release-db (cache-dir orig-db missing-versions)
  (let ((new-db (copy-tree orig-db)))
    (dolist (missing-version missing-versions)
      (destructuring-bind (version-string . distinfo-url)
          missing-version
        (let ((version-db (make-db-for-dist-version cache-dir version-string distinfo-url)))
          (setf new-db (merge-dist-dbs new-db version-db)))))
    (sort new-db #'string< :key #'car)))

(defun assemble-systems-db (releases-db)
  (let ((out nil))
    (dolist (r releases-db)
      (destructuring-bind (release-name &key versions)
          r
        (dolist (v versions)
          (destructuring-bind (version-strings &key systems &allow-other-keys)
              v
            (dolist (s systems)
              (destructuring-bind (system-name &key &allow-other-keys)
                  s
                (dolist (vs version-strings)
                  (push (cons release-name vs) ;;(list :release release-name :release-version vs)
                        (assoc-value out system-name :test 'string-equal)))))))))
    (sort out #'string< :key #'car)))

(defun equal-version-descriptions-p (d1 d2)
  (let ((url-1 (getf d1 :url))
        (url-2 (getf d2 :url)))
    (string-equal url-1 url-2)))

(defun merge-dist-dbs (orig-db new-db)
  (let ((out-db (copy-tree orig-db)))
    (dolist (r new-db)
      (destructuring-bind (release-name &key versions)
          r
        (let ((orig-release (assoc release-name out-db :test #'string-equal)))
          (if orig-release
              (dolist (v versions)
                (destructuring-bind (version-strings &rest version-description)
                    v
                  (let ((matching-version (rassoc version-description (getf (cdr orig-release) :versions)
                                                  :test #'equal-version-descriptions-p)))
                    (if matching-version
                        (setf (car matching-version)
                              (union (car matching-version) version-strings :test #'string-equal))
                        (push v (getf (cdr orig-release) :versions))))))
              (push r out-db)))))
    out-db))


(defmethod sync-source ((source quicklisp-source))
  (let* ((known-versions (source/dist-versions source))
         (latest-versions (latest-all-versions source))
         (missing-versions (set-difference latest-versions known-versions :key #'car :test #'equal)))
    (when missing-versions
      ;; There are versions that we don't have! Need to sync.
      (setf (getf (source/db source) :versions)
            latest-versions)
      (setf (getf (source/db source) :releases)
            (assemble-release-db (source/cache-directory source)
                                 (getf (source/db source) :releases)
                                 missing-versions))
      ;; Build the updated system table
      (setf (getf (source/db source) :systems)
            (assemble-systems-db (getf (source/db source) :releases)))
      (save-db source)
      t)))




(defmethod init-source-cache ((source quicklisp-source))
  (ensure-directories-exist (source/cache-directory source)))
