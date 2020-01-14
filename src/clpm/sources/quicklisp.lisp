;;;; Quicklisp derived sources
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/sources/quicklisp
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/cache
          #:clpm/data
          #:clpm/http-client
          #:clpm/log
          #:clpm/requirement
          #:clpm/sources/defs
          #:clpm/sources/simple-versioned-project
          #:clpm/sources/tarball-release
          #:clpm/utils
          #:iterate
          #:puri
          #:split-sequence)
  (:import-from #:cl-conspack)
  (:export #:quicklisp-source))

(in-package #:clpm/sources/quicklisp)

(setup-logger)

;;; This package allows CLPM to natively interact with Quicklisp
;;; distributions. It is tested primarily on the main Quicklisp distribution
;;; (quicklisp), but should nearly work on other distributions as well, such as
;;; ultralisp or cl21.
;;;
;;; Interacting with Quicklisp distributions is a little painful because of the
;;; different design focuses of each project. CLPM needs to be able to determine
;;; information on all the previous releases of a system (preferably quickly!)
;;; whereas Quicklisp distributions are designed to make data easily accessible
;;; for only a single version of a distribution. As such, CLPM needs to crawl
;;; all distribution versions and index their data locally.
;;;
;;; In the future, I would like the CLPM project to grow and host metadata for
;;; the most popular Quicklisp derived distributions in a format that is easy
;;; for CLPM to fetch and incorporate. But there will always be a place for this
;;; package, as I would like to continue supporting private Quicklisp
;;; distributions that can't be indexed by the CLPM project or just random
;;; public ones that may pop up.


;;; * Quicklisp conditions

(define-condition quicklisp-version-missing (source-no-such-object)
  ((missing-version
    :initarg :missing-version)))


;;; * Quicklisp backed source

(defvar *current-source* nil)

(defclass quicklisp-source (clpm-source)
  ((force-https
    :initarg :force-https
    :initform nil
    :accessor ql-force-https
    :documentation
    "~T~ iff this source should download everything over HTTPS.")
   (versions-url
    :accessor ql-versions-url
    :documentation
    "Holds the URL to the .txt file that stores all versions of this source.")
   (dist-release-ht
    :initform (make-hash-table :test 'equal)
    :accessor ql-source-dist-release-ht
    :documentation
    "Maps version strings to ~ql-dist-release~ objects.")
   (project-ht
    :initform (make-hash-table :test 'equal)
    :accessor ql-source-project-ht
    :documentation
    "Maps project names (strings) to ~ql-project~ objects.")
   (system-ht
    :initform (make-hash-table :test 'equal)
    :accessor ql-source-system-ht
    :documentation
    "Maps system names (strings) to ~ql-system~ objects."))
  (:documentation
   "A CLPM source backed by a quicklisp-style repository."))

(defmethod initialize-instance :after ((source quicklisp-source) &rest initargs
                                       &key &allow-other-keys)
  "If the source is being forced to HTTPS, update the base URL accordingly. If
the base URL is HTTPS, ensure the force-https flag is set (presumably that is
what the user intends). Last, computes the versions URL."
  (declare (ignore initargs))
  (when (ql-force-https source)
    (ensure-uri-scheme-https! (source-url source)))
  ;; If the uri is given as https, assume the user wants to fetch everything
  ;; from this source over https.
  (when (and (not (ql-force-https source))
             (eql (uri-scheme (source-url source)) :https))
    (setf (ql-force-https source) t))
  ;; Compute the versions url. Example:
  ;; http://beta.quicklisp.org/dist/quicklisp.txt ->
  ;; http://beta.quicklisp.org/dist/quicklisp-versions.txt
  (let* ((path (puri:uri-path (source-url source)))
         (file-name (pathname-name path)))
    (setf (ql-versions-url source)
          (merge-uris (namestring
                       (merge-pathnames (concatenate 'string file-name "-versions")
                                        path))
                      (source-url source))))
  ;; Load the synced files if they exist.
  (let ((pn (merge-pathnames "ql-source.conspack"
                             (source-lib-directory source))))
    (when (probe-file pn)
      (with-open-file (s pn
                         :element-type '(unsigned-byte 8))
        (assert (= 1 (cpk:decode-stream s)))
        (let ((*current-source* source))
          (cpk:tracking-refs ()
            (setf (ql-source-dist-release-ht source) (cpk:decode-stream s))
            (setf (ql-source-project-ht source) (cpk:decode-stream s))
            (setf (ql-source-system-ht source) (cpk:decode-stream s))))))))

(defun save-ql-source! (source)
  (let ((pn (merge-pathnames "ql-source.conspack"
                             (source-lib-directory source))))
    (ensure-directories-exist pn)
    (with-open-file (s pn
                       :if-exists :supersede
                       :direction :output
                       :element-type '(unsigned-byte 8))
      (cpk:encode 1 :stream s)
      (cpk:tracking-refs ()
        (cpk:encode (ql-source-dist-release-ht source) :stream s)
        (cpk:encode (ql-source-project-ht source) :stream s)
        (cpk:encode (ql-source-system-ht source) :stream s)))))

(defmethod source-cache-directory ((source quicklisp-source))
  "Compute the cache location for this source, based on its canonical url."
  (clpm-cache-pathname
   `("sources"
     "quicklisp"
     ,(string-downcase (string (uri-scheme (source-url source))))
     ,(uri-host (source-url source))
     ,@(split-sequence #\/ (uri-path (source-url source)) :remove-empty-subseqs t))
   :ensure-directory t))

(defmethod source-lib-directory ((source quicklisp-source))
  "Compute the data location for this source, based on its canonical url."
  (clpm-data-pathname
   `("sources"
     "quicklisp"
     ,(string-downcase (string (uri-scheme (source-url source))))
     ,(uri-host (source-url source))
     ,@(split-sequence #\/ (uri-path (source-url source)) :remove-empty-subseqs t))
   :ensure-directory t))

(defun %source-project-release (source project-name version-string
                                &optional (error t))
  ;; Make sure the dist release is present.
  (let ((dist-release (ql-source-dist-release source version-string)))
    (unless dist-release
      (if error
          (error 'quicklisp-version-missing
                 :missing-version version-string)
          (return-from %source-project-release nil)))
    (let* ((project (source-project source project-name))
           (release (project-release project version-string)))
      release)))

(defmethod source-project-release ((source quicklisp-source) project-name version-string
                                   &optional (error t))
    (restart-case
        (%source-project-release source project-name version-string error)
      (sync-and-retry (c)
        :report "Sync source and try again."
        (declare (ignore c))
        (sync-version-list! source)
        (sync-dist-release! (ql-source-dist-release source version-string))
        (%source-project-release source project-name version-string error))))

(defmethod source-project ((source quicklisp-source) project-name &optional (error t))
  (or (gethash project-name (ql-source-project-ht source))
      (when error
        (error 'source-missing-project
               :source source
               :project-name project-name))))

(defmethod source-system ((source quicklisp-source) system-name &optional (error t))
  (or (gethash system-name (ql-source-system-ht source))
      (when error
        (error 'source-missing-system
               :source source
               :system-name system-name))))

(defmethod source-type-keyword ((source quicklisp-source))
  :quicklisp)

(defmethod source-to-form ((source quicklisp-source))
  (list (source-name source)
        :url (uri-to-string (source-url source))
        :type :quicklisp
        :force-https (ql-force-https source)))


;;; * Version of a quicklisp distribution

(defclass ql-dist-release ()
  ((source
    :initarg :source
    :accessor ql-dist-release-source)
   (version
    :initarg :version
    :accessor ql-dist-release-version
    :documentation
    "A string naming this version. Typically time based.")
   (synced-p
    :initform nil
    :accessor ql-dist-release-synced-p
    :documentation
    "If true this version has been synced.")
   (canonical-distinfo-url
    :initarg :canonical-distinfo-url
    :accessor ql-dist-release-canonical-distinfo-url
    :documentation
    "The canonical URL for this version's =distinfo.txt= file.")
   (system-index-url
    :initarg :system-index-url
    :initform nil
    :accessor ql-dist-release-system-index-url
    :documentation
    "The URL for this version's =systems.txt= file.")
   (release-index-url
    :initarg :release-index-url
    :initform nil
    :accessor ql-dist-release-release-index-url
    :documentation
    "The URL for this version's =releases.txt= file.")))

(cpk:defencoding ql-dist-release
  version synced-p canonical-distinfo-url system-index-url release-index-url)

(defmethod cpk:decode-object :around ((class (eql 'ql-dist-release)) alist &key &allow-other-keys)
  (aprog1 (call-next-method)
    (setf (slot-value it 'source) *current-source*)))

(defun ql-source-unsynced-dist-releases (source)
  "Returns a list of the unsynced versions of the ~source~ distribution."
  (iter
    (for (version release) :in-hashtable (ql-source-dist-release-ht source))
    (unless (ql-dist-release-synced-p release)
      (collect release))))

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

(defun ql-source-dist-release (source version-id)
  (gethash version-id (ql-source-dist-release-ht source)))

(defun (setf ql-source-dist-release) (value source version-id)
  (setf (gethash version-id (ql-source-dist-release-ht source))
        value))


;;; * Projects

(defclass ql-project ()
  ((source
    :initarg :source
    :accessor ql-project-source
    :reader project-source)
   (name
    :initarg :name
    :accessor ql-project-name
    :reader project-name
    :documentation
    "The name of the project.")
   (release-ht
    :initform (make-hash-table :test 'equal)
    :accessor ql-project-release-ht))
  (:documentation
   "Represents a project in a quicklisp distribution."))

(cpk:defencoding ql-project
  name release-ht)

(defmethod cpk:decode-object :around ((class (eql 'ql-project)) alist &key &allow-other-keys)
  (aprog1 (call-next-method)
    (setf (slot-value it 'source) *current-source*)))

(defmethod project-release ((self ql-project) version-string &optional (error t))
  (or (gethash version-string (ql-project-release-ht self))
      (when error
        (error 'project-missing-version
               :source (project-source self)
               :project self
               :version version-string))))

(defmethod project-releases ((p ql-project))
  (hash-table-values (ql-project-release-ht p)))


;;; * Releases

(defclass ql-release (tarball-release-with-md5
                      tarball-release-with-size
                      simple-versioned-release)
  ((source
    :initarg :source
    :accessor ql-release-source
    :reader release-source)
   (project
    :initarg :project
    :accessor ql-release-project)
   (url
    :initarg :url
    :accessor ql-release-url
    :documentation
    "The URL where the tarball for this release is located.")
   (size
    :initarg :size
    :accessor ql-release-size
    :reader tarball-release/desired-size
    :documentation
    "The size of the tarball.")
   (file-md5
    :initarg :file-md5
    :accessor ql-release-file-md5
    :reader tarball-release/desired-md5
    :documentation
    "The md5sum of the tarball.")
   (content-sha1
    :initarg :content-sha1
    :accessor ql-release-content-sha1
    :documentation
    "The sha1sum of the tarball contents.")
   (prefix
    :initarg :prefix
    :accessor ql-release-prefix
    :documentation
    "The prefix on every file in the tarball.")
   (system-release-ht
    :initform (make-hash-table :test 'equal)
    :accessor ql-release-system-release-ht
    :documentation
    "Maps system names to ~ql-system-release~ objects.")
   (system-file-ht
    :initform (make-hash-table :test 'equal)
    :accessor ql-release-system-file-ht
    :documentation
    "Maps system file namestrings to ~ql-system-file~ objects."))
  (:documentation
   "Represents a release of a project in a quicklisp distribution."))

(cpk:defencoding ql-release
  project url size file-md5 content-sha1 prefix system-release-ht system-file-ht version)

(defmethod cpk:decode-object :around ((class (eql 'ql-release)) alist &key &allow-other-keys)
  (aprog1 (call-next-method)
    ;;(break)
    (setf (slot-value it 'source) *current-source*)))

(defmethod release-system-file ((release ql-release) system-file-namestring)
  (gethash system-file-namestring (ql-release-system-file-ht release)))

(defmethod release-system-release ((release ql-release) system-name &optional (error t))
  (or (gethash system-name (ql-release-system-release-ht release))
      (when error
        (error 'release-missing-system-release
               :source (release-source release)
               :release release
               :system-name system-name))))

(defmethod release-system-releases ((release ql-release))
  (hash-table-values (ql-release-system-release-ht release)))

(defmethod release-project ((release ql-release))
  (ql-release-project release))

(defmethod release-lib-pathname ((r ql-release))
  (uiop:resolve-absolute-location
   (list (source-lib-directory (release-source r))
         "projects"
         (project-name (release-project r))
         (ql-release-prefix r))
   :ensure-directory t))

(defmethod tarball-release/url ((release ql-release))
  (aprog1
      (parse-uri (ql-release-url release))
    (when (ql-force-https (release-source release))
      (ensure-uri-scheme-https! it))))


;;; * Systems

(defclass ql-system ()
  ((source
    :initarg :source
    :accessor ql-system-source
    :reader system-source)
   (name
    :initarg :name
    :accessor ql-system-name
    :accessor system-name
    :documentation
    "The name of the system.")
   (releases-ht
    :initform (make-hash-table :test 'equal)
    :accessor ql-system-system-releases-ht
    :documentation
    "Maps version strings to ~ql-system-release~ objects."))
  (:documentation
   "An ASD system contained in a quicklisp distribution."))

(cpk:defencoding ql-system
  name releases-ht)

(defmethod cpk:decode-object :around ((class (eql 'ql-system)) alist &key &allow-other-keys)
  (aprog1 (call-next-method)
    (setf (slot-value it 'source) *current-source*)))

(defmethod system-system-releases ((system ql-system))
  (hash-table-values (ql-system-system-releases-ht system)))


;;; * System releases

(defclass ql-system-release ()
  ((source
    :initarg :source
    :accessor ql-system-release-source
    :reader system-release-source)
   (release
    :initarg :release
    :accessor ql-system-release-release
    :reader system-release-release
    :documentation
    "The release to which this system release belongs.")
   (system
    :initarg :system
    :accessor ql-system-release-system
    :reader system-release-system
    :documentation
    "The system.")
   (system-file
    :initarg :system-file
    :accessor ql-system-release-system-file
    :documentation
    "The asd file in which this system is located.")
   (dependencies
    :initarg :dependencies
    :accessor ql-system-release-dependencies
    :documentation
    "A list of dependencies this system has in this release."))
  (:documentation
   "A release of a system. Ties together an ASD system, a point in time (the
   release), and the state of the asd system at that time (what file it was
   located in and its dependencies)."))

(cpk:defencoding ql-system-release
  release system system-file dependencies)

(defmethod cpk:decode-object :around ((class (eql 'ql-system-release)) alist &key &allow-other-keys)
  (aprog1 (call-next-method)
    (setf (slot-value it 'source) *current-source*)))

(defmethod system-release-system-file ((system-release ql-system-release))
  (release-system-file (system-release-release system-release)
                       (ql-system-release-system-file system-release)))

(defmethod system-release-absolute-asd-pathname ((system-release ql-system-release))
  (system-file-absolute-asd-pathname
   (release-system-file (system-release-release system-release)
                        (ql-system-release-system-file system-release))))

(defmethod system-release-satisfies-version-spec-p ((system-release ql-system-release) version-spec)
  "There is currently no good way to reasonably get the system version from the
  metadata alone, so say everything is satisfied."
  t)

(defmethod system-release-> ((sr-1 ql-system-release) (sr-2 ql-system-release))
  (release-> (system-release-release sr-1) (system-release-release sr-2)))

(defmethod system-release-requirements ((system-release ql-system-release))
  (let ((deps (remove-if (rcurry #'member (list "asdf" "uiop") :test #'string-equal)
                         (ql-system-release-dependencies system-release))))
    (mapcar (lambda (dep-name)
              (make-instance 'system-requirement
                             :name dep-name))
            deps)))


;;; * System files

(defclass ql-system-file ()
  ((source
    :initarg :source
    :reader system-file-source
    :documentation
    "The source for this system file.")
   (namestring
    :initarg :namestring
    :reader system-file-asd-enough-namestring
    :documentation
    "The namestring for the file.")
   (release
    :initarg :release
    :reader system-file-release
    :documentation
    "The release to which the system-file belongs."))
  (:documentation
   "A system file. Belongs to a specific release."))

(cpk:defencoding ql-system-file
  namestring release)

(defmethod cpk:decode-object :around ((class (eql 'ql-system-file)) alist &key &allow-other-keys)
  (aprog1 (call-next-method)
    (setf (slot-value it 'source) *current-source*)))

(defmethod system-file-absolute-asd-pathname ((system-file ql-system-file))
  (let* ((release (system-file-release system-file)))
    (merge-pathnames (system-file-asd-enough-namestring system-file)
                     (release-lib-pathname release))))


;;; * Syncing!

(defun latest-version-map (source)
  "Fetch the latest versions for a source. Returns an alist mapping version
strings to distinfo.txt urls."
  (let ((versions-pathname (merge-pathnames "metadata/distinfo-versions.txt"
                                            (source-cache-directory source))))
    (ensure-file-fetched versions-pathname (ql-versions-url source))
    (let ((versions-lines (uiop:read-file-lines versions-pathname)))
      (mapcar (lambda (l)
                (destructuring-bind (version url)
                    (split-sequence #\Space l)
                  (cons version url)))
              versions-lines))))

(defun sync-version-list! (source)
  "Given a ~source~, sync the known versions of this distribution."
  (let ((all-versions (latest-version-map source)))
    (dolist (pair all-versions)
      (destructuring-bind (version . url) pair
        (unless (ql-source-dist-release source version)
          (setf (ql-source-dist-release source version)
                (make-instance 'ql-dist-release
                               :source source
                               :version version
                               :canonical-distinfo-url url)))))))

(defun sync-dist-release-projects! (self source)
  "Given a ~ql-dist-release~, download its release data."
  (let ((release-index-pathname (merge-pathnames (uiop:strcat "metadata/"
                                                              (ql-dist-release-version self)
                                                              "/releases.txt")
                                                 (source-cache-directory source)))
        (release-index-url (ql-dist-release-release-index-url self)))
    (ensure-file-fetched release-index-pathname release-index-url)

    (let ((release-index-contents (uiop:read-file-string release-index-pathname))
          (dist-version-id (ql-dist-release-version self)))
      (dolist (line (rest (split-sequence #\Newline release-index-contents :remove-empty-subseqs t)))
        (destructuring-bind (project-name url size file-md5 content-sha1 prefix &rest system-files)
            (split-sequence #\Space line)
          (let* ((project (ensure-gethash project-name (ql-source-project-ht source)
                                          (make-instance 'ql-project
                                                         :source source
                                                         :name project-name)))
                 (release (make-instance 'ql-release
                                         :source source
                                         :version dist-version-id
                                         :project project
                                         :url url
                                         :size (parse-integer size)
                                         :file-md5 file-md5
                                         :content-sha1 content-sha1
                                         :prefix prefix)))
            (dolist (sf system-files)
              (setf (gethash sf (ql-release-system-file-ht release))
                    (make-instance 'ql-system-file
                                   :source source
                                   :namestring sf
                                   :release release)))
            (setf (gethash dist-version-id (ql-project-release-ht project))
                  release)))))))

(defun sync-dist-release-systems! (self source)
  "Given a ~ql-dist-version~, download its system data and merge it into the
local database."
  (let ((system-index-pathname (merge-pathnames (uiop:strcat "metadata/"
                                                             (ql-dist-release-version self)
                                                             "/systems.txt")
                                                (source-cache-directory source)))
        (system-index-url (ql-dist-release-system-index-url self)))
    (ensure-file-fetched system-index-pathname system-index-url)

    (let ((system-index-contents (uiop:read-file-string system-index-pathname))
          (dist-version-id (ql-dist-release-version self)))
      (dolist (line (rest (split-sequence #\Newline system-index-contents :remove-empty-subseqs t)))
        (destructuring-bind (project-name system-file system-name &rest dependencies)
            (split-sequence #\Space line)
          (let* ((system (ensure-gethash system-name (ql-source-system-ht source)
                                         (make-instance 'ql-system
                                                        :name system-name
                                                        :source source)))
                 (release (project-release (source-project source project-name) dist-version-id))
                 (system-release (make-instance 'ql-system-release
                                                :source source
                                                :system system
                                                :release release
                                                :system-file (concatenate 'string system-file ".asd")
                                                :dependencies dependencies)))
            (assert release)
            (setf (gethash dist-version-id (ql-system-system-releases-ht system))
                  system-release)
            (setf (gethash system-name (ql-release-system-release-ht release))
                  system-release)))))))

(defun sync-dist-release! (self)
  "Given a ~ql-dist-version~ object, sync its data."
  (when (ql-dist-release-synced-p self)
    (error "Already synced!"))

  (let* ((source (ql-dist-release-source self))
         (distinfo-url (ql-dist-release-canonical-distinfo-url self))
         (distinfo-pathname (merge-pathnames (uiop:strcat "metadata/"
                                                          (ql-dist-release-version self)
                                                          "/distinfo.txt")
                                             (source-cache-directory source))))
    (ensure-file-fetched distinfo-pathname distinfo-url)
    (let* ((distinfo-contents (uiop:read-file-string distinfo-pathname))
           (distinfo-plist (parse-distinfo distinfo-contents)))
      (destructuring-bind (&key
                             release-index-url system-index-url
                             canonical-distinfo-url
                           &allow-other-keys)
          distinfo-plist
        (setf (ql-dist-release-canonical-distinfo-url self) canonical-distinfo-url
              (ql-dist-release-system-index-url self) system-index-url
              (ql-dist-release-release-index-url self) release-index-url)
        (sync-dist-release-projects! self source)
        (sync-dist-release-systems! self source)
        (setf (ql-dist-release-synced-p self) t)))))

(defmethod sync-source ((source quicklisp-source))
  "Sync the version list and then sync all unsynced dist versions."
  (sync-version-list! source)
  (mapc 'sync-dist-release!
        (ql-source-unsynced-dist-releases source))
  (save-ql-source! source)
  (values))
