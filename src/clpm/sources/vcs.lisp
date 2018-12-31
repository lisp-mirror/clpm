;;;; Sources for projects taken from source control. A VCS source requires a
;;;; repo to be checked out before much can be done with it, so it shares a lot
;;;; of code with fs-sources. But it ensures that the repo is checked out before
;;;; it falls back to fs methods

;; * define package

(uiop:define-package #:clpm/sources/vcs
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/archives
          #:clpm/cache
          #:clpm/config
          #:clpm/data
          #:clpm/deps
          #:clpm/http-client
          #:clpm/requirement
          #:clpm/sources/defs
          #:clpm/sources/semantic-versioned-project
          #:clpm/utils
          #:puri)
  (:export #:ensure-git-release-installed!
           #:get-git-source
           #:git-project/git-uri-string
           #:git-release
           #:git-release/branch
           #:git-release/commit
           #:git-release/tag
           #:git-source/host
           #:git-source-register-project!
           #:git-source-type-keyword
           #:git-system
           #:git-system-release
           #:vcs-project/cache-directory
           #:vcs-project/path))

(in-package #:clpm/sources/vcs)


;; * URI parsing

(defclass git-uri (uri)
  ((user-name
    :initform nil
    :accessor git-uri/user-name)
   (real-host
    :initform nil
    :accessor git-uri/real-host))
  (:documentation "A URI class for git uris. holds on to the real host and username."))

(defmethod initialize-instance :after ((uri git-uri) &rest initargs
                                       &key host)
  (declare (ignore initargs))
  (let ((@-pos (position #\@ host)))
    (if @-pos
        (setf (git-uri/real-host uri) (subseq host (1+ @-pos))
              (git-uri/user-name uri) (subseq host 0 @-pos))
        (setf (git-uri/real-host uri) host))))

(defun guess-git-protocol (uri-string)
  (let* ((colon-pos (position #\: uri-string)))
    (if colon-pos
        ;; Either the protocol is directly specified *or* the user is using
        ;; SCP-like syntax.
        (let ((pre-colon (subseq uri-string 0 colon-pos)))
          (switch (pre-colon :test #'string-equal)
            ("http"
             :http)
            ("https"
             :https)
            ("ssh"
             :ssh)
            ("file"
             :file)
            (t
             :scp)))
        ;; Implicit file protocol
        :file-implicit)))

(defun convert-scp-to-ssh (uri-string)
  (concatenate 'string "ssh://"
               (substitute #\/ #\: uri-string :count 1)))

(defun parse-git-uri (uri-string)
  ;; First, guess at the protocol:
  (let ((protocol (guess-git-protocol uri-string)))
    (ecase protocol
      (:file-implicit
       (pathname uri-string))
      (:scp
       (parse-uri (convert-scp-to-ssh uri-string)
                  :class 'git-uri))
      ((:http :https :ssh :file)
       (parse-uri uri-string
                  :class 'git-uri)))))


;; * sources

(defvar *git-source-cache* (make-hash-table :test 'equalp)
  "A global cache mapping host names to git sources.")

(defclass vcs-source (clpm-known-source)
  ()
  (:documentation "root class for version control sources"))

(defclass git-source (vcs-source)
  ((host
    :initarg :host
    :accessor source/host
    :reader git-source/host
    :documentation "A string naming the host of the git repo")
   (projects-by-name
    :initform (make-hash-table :test 'equal)
    :accessor git-source/projects-by-name
    :documentation "hash table mapping project names to project objects.")
   (systems-by-name
    :initform (make-hash-table :test 'equalp)
    :accessor git-source/systems-by-name
    :documentation "A hash table mapping system names to system objects."))
  (:documentation "A git source"))

(defclass github-source (git-source)
  ((host
    :initform "github.com"))
  (:documentation "Github hosted project source."))

(defclass gitlab-source (git-source)
  ((host
    :initform "gitlab.com"))
  (:documentation "Gitlab hosted project source."))

(defun get-git-source-type-by-keyword (type)
  "Returns two values: the symbol naming the class of the source and the default
hostname or NIL."
  (ecase type
    (:gitlab
     (values 'gitlab-source "gitlab.com"))))

(defgeneric git-source-type-keyword (source))

(defmethod git-source-type-keyword ((source gitlab-source))
  :gitlab)

(defun get-git-source-type-by-hostname (hostname)
  "Return the correct class type for a git source based on its hostname."
  (switch (hostname :test #'equalp)
    ("gitlab.com"
     'gitlab-source)
    ("github.com"
     'github-source)
    (otherwise
     'git-source)))

(defun get-git-source-by-hostname (hostname)
  "Return the git source object for hostname, instantiating it if necessary."
  ;; TODO: This is no longer valid!
  (ensure-gethash hostname *git-source-cache*
                  (make-instance (get-git-source-type-by-hostname hostname)
                                 :host hostname)))

(defun get-git-source-from-uri (uri)
  "Return the git source object for uri, instantiating it if necessary."
  (let* ((uri (parse-git-uri uri))
         (real-host (if (pathnamep uri)
                        :localhost
                        (git-uri/real-host uri))))
    (get-git-source-by-hostname real-host)))

(defun get-git-source (type &optional host)
  (multiple-value-bind (type default-host)
      (get-git-source-type-by-keyword type)
    (ensure-gethash (list type (or host default-host)) *git-source-cache*
                    (make-instance type
                                   :host (or host default-host)))))

(defun git-source-register-project! (git-source repo project-name)
  "Registers a project with the git source and returns it."
  (let ((projects-by-name (git-source/projects-by-name git-source)))
    (ensure-gethash project-name projects-by-name
                    (make-instance 'git-project
                                   :path repo
                                   :source git-source
                                   :name project-name))))

(defmethod source/cache-directory ((source git-source))
  "The cache directory for this source is based on the hostname."
  (clpm-cache
   `("vcs-sources"
     ,(source/host source))
   :ensure-directory t))

(defmethod source/lib-directory ((source git-source))
  "The lib directory is based on the hostname."
  (clpm-data
   `("vcs-sources"
     ,(source/host source))
   :ensure-directory t))

(defmethod source/project ((source git-source) project-name)
  (gethash project-name (git-source/projects-by-name source)))

(defmethod source/system ((source git-source) system-name)
  (or (gethash system-name (git-source/systems-by-name source))
      (some (lambda (project)
              (some (lambda (release)
                      (when-let ((system-release (release/system-release release system-name)))
                        (system-release/system system-release)))
                    (project/releases project)))
            (hash-table-values (git-source/projects-by-name source)))))


;; * projects

(defclass git-project (clpm-project)
  ((repo-uri
    :initarg :repo-uri
    :accessor git-project/repo-uri)
   (path
    :initarg :path
    :accessor vcs-project/path)
   (releases-by-spec
    :initform (make-hash-table :test 'equal)
    :accessor vcs-project/releases-by-spec)))

(defun find-remote-config (hostname)
  (config-value :git :remotes (string-downcase hostname)))

(defun git-project/git-credentials (project)
  (let* ((creds nil)
         (source (project/source project))
         (remote-config (find-remote-config (source/host source))))
    (when remote-config
      (let ((username (gethash :username remote-config))
            (password (gethash :password remote-config)))
        (when username
          (push (cons :username username) creds))
        (when password
          (push (cons :password password) creds))))
    creds))

(defun git-project/git-uri-string (project)
  (let ((source (project/source project)))
    (etypecase source
      (gitlab-source
       (let* ((remote-config (find-remote-config (source/host source)))
              (method (if remote-config
                          (gethash :method remote-config "https")
                          "https"))
              (port (when remote-config
                      (gethash :port remote-config nil)))
              (path (vcs-project/path project))
              (path-with.git (if (not (ends-with-subseq ".git" path))
                                 (concatenate 'string path ".git")
                                 path)))

         (eswitch (method :test #'equal)
           ("ssh"
            (concatenate 'string
                         "git@"
                         (source/host source)
                         ":"
                         path-with.git))
           ("https"
            (concatenate 'string
                         "https://"
                         (source/host source)
                         "/"
                         path-with.git))
           ("http"
            (concatenate 'string
                         "http://"
                         (source/host source)
                         (when port
                           (format nil ":~A" port))
                         "/"
                         path-with.git))))))))

(defun vcs-project/cache-directory (project)
  (let* ((path (vcs-project/path project))
         (path-with.git (if (not (ends-with-subseq ".git" path))
                            (concatenate 'string path ".git")
                            path)))
    (uiop:resolve-absolute-location
     `(,(source/cache-directory (project/source project))
       ,path-with.git)
     :ensure-directory t)))

(defmethod project/release ((project git-project) (version string))
  "A release named by a string is assumed to refer to a commit SHA1."
  (project/release project `(:commit ,version)))

(defmethod project/release ((project git-project) (version list))
  "The version can be one of (:tag TAG-NAME), (:branch BRANCH-NAME), or (:commit
  COMMIT-SHA1)"
  (destructuring-bind (version-type version-string)
      version
    (check-type version-type (member :tag :branch :commit))
    (check-type version-string string)
    (ensure-gethash version (vcs-project/releases-by-spec project)
                    (apply #'make-instance
                           'git-release
                           :project project
                           :source (project/source project)
                           version))))

(defmethod project/releases ((project git-project))
  (hash-table-values (vcs-project/releases-by-spec project)))


;; * releases

(defclass git-release (clpm-release)
  ((tag
    :initarg :tag
    :initform nil
    :accessor git-release/tag)
   (branch
    :initarg :branch
    :initform nil
    :accessor git-release/branch)
   (commit
    :initarg :commit
    :initform nil
    :accessor git-release/commit)
   (system-files-by-namestring
    :accessor git-release/system-files-by-namestring)
   (system-files-by-primary-name
    :accessor git-release/system-files-by-primary-name)
   (system-releases-by-name
    :initform (make-hash-table :test 'equal)
    :accessor git-release/system-releases-by-name)))

(defun populate-release-system-files! (release)
  (ensure-release-installed! release)
  (let ((ht-by-namestring (make-hash-table :test 'equal))
        (ht-by-primary-name (make-hash-table :test 'equalp)))
    (asdf::collect-sub*directories-asd-files
     (release/lib-pathname release)
     :collect (lambda (x)
                (let* ((namestring (enough-namestring x (release/lib-pathname release)))
                       (system-file (make-instance 'git-system-file
                                                  :relative-pathname namestring
                                                  :release release
                                                  :source (release/source release))))
                  (setf (gethash namestring ht-by-namestring)
                        system-file)
                  (setf (gethash (pathname-name x) ht-by-primary-name)
                        system-file))))
    (setf (git-release/system-files-by-namestring release) ht-by-namestring)
    (setf (git-release/system-files-by-primary-name release) ht-by-primary-name)))

(defmethod slot-unbound (class (release git-release) (slot-name (eql 'system-files-by-namestring)))
  (populate-release-system-files! release)
  (git-release/system-files-by-namestring release))

(defmethod slot-unbound (class (release git-release) (slot-name (eql 'system-files-by-primary-name)))
  (populate-release-system-files! release)
  (git-release/system-files-by-primary-name release))

(defmethod release/version ((release git-release))
  (acond
    ((git-release/commit release)
     `(:commit ,it))
    ((git-release/tag release)
     `(:tag ,it))
    ((git-release/branch release)
     `(:branch ,it))))

(defmethod release/system-file ((release git-release) system-file-namestring)
  "Requires the release to be installed. Return a system file object if not
already created."
  (gethash system-file-namestring (git-release/system-files-by-namestring release)))

(defmethod release/system-files ((release git-release))
  (hash-table-values (git-release/system-files-by-namestring release)))

(defmethod release/system-release ((release git-release) system-name)
  (unless (gethash system-name (git-release/system-releases-by-name release))
    (let* ((system-file (gethash (asdf:primary-system-name system-name)
                                 (git-release/system-files-by-primary-name release))))
      (when system-file
        (let* ((source (release/source release))
               (system (ensure-gethash system-name (git-source/systems-by-name source)
                                       (make-instance 'git-system
                                                      :name system-name
                                                      :source source)))
               (system-release (make-instance 'git-system-release
                                              :release release
                                              :source (release/source release)
                                              :system system
                                              :system-file system-file)))
          (push release (git-system/releases system))
          (setf (gethash system-name (git-system-file/system-releases-by-name system-file))
                system-release)
          (setf (gethash system-name (git-release/system-releases-by-name release))
                system-release)))))
  (gethash system-name (git-release/system-releases-by-name release)))

(defmethod release/system-releases (release)
  "Get all the system files and append together their systems."
  (let* ((system-files (release/system-files release)))
    (apply #'append (mapcar #'system-file/system-releases system-files))))

(defmethod release/lib-pathname ((release git-release))
  (assert (git-release/commit release))
  (let ((project (release/project release))
        (commit-string (git-release/commit release)))
    (uiop:resolve-absolute-location
     (list (source/lib-directory (release/source release))
           (vcs-project/path project)
           (subseq commit-string 0 2)
           (subseq commit-string 2 4)
           commit-string)
     :ensure-directory t)))


;; * system files

(defclass git-system-file (clpm-system-file)
  ((relative-pathname
    :initarg :relative-pathname
    :accessor git-system-file/relative-pathname
    :accessor system-file/asd-enough-namestring)
   (system-releases-by-name
    :initform (make-hash-table :test 'equalp)
    :accessor git-system-file/system-releases-by-name)
   (groveled-p
    :initform nil
    :accessor git-system-file/groveled-p)))

(defmethod system-file/absolute-asd-pathname ((system-file git-system-file))
  "Merge the enough pathname with the release's lib pathname."
  (merge-pathnames (git-system-file/relative-pathname system-file)
                   (release/lib-pathname (system-file/release system-file))))

(defmethod system-file/system-releases ((system-file git-system-file))
  "Grovel over the system file if necessary to determine every system it contains."
  (unless (git-system-file/groveled-p system-file)
    ;; Sigh. We need to grovel the file to make sure we know ~everything it
    ;; defines.
    (let ((system-names (grovel-systems-in-file (system-file/absolute-asd-pathname system-file))))
      (dolist (system-name system-names)
        ;; Get the system release from the release (which will register it in
        ;; the system-releases-by-name slot of this object).
        (let* ((release (system-file/release system-file))
               (system-release (release/system-release release system-name)))
          system-release))
      (setf (git-system-file/groveled-p system-file) t)))
  (hash-table-values (git-system-file/system-releases-by-name system-file)))


;; * systems

(defclass git-system (clpm-system)
  ((releases
    :initform nil
    :accessor git-system/releases)))

(defmethod system/system-releases ((system git-system))
  (mapcar #'(lambda (x)
              (release/system-release x (system/name system)))
          (git-system/releases system)))

(defmethod system/releases ((system git-system))
  (copy-list (git-system/releases system)))


;; * system-releases

(defclass git-system-release (semantic-versioned-system-release clpm-system-release)
  ((reqs
    :accessor system-release/requirements)
   (system-file
    :initarg :system-file
    :accessor system-release/system-file)))

(defun parse-system-release-info-from-groveler! (system-release info)
  "Take the info provided by the groveler and modify system-release in place to
include it."
  (destructuring-bind (system-name
                       &key version depends-on defsystem-depends-on loaded-systems
                       &allow-other-keys)
      info
    (declare (ignore system-name))
    (setf (system-release/system-version system-release) version)
    (setf (system-release/requirements system-release)
          (mapcar #'convert-asd-system-spec-to-req
                  (append depends-on defsystem-depends-on loaded-systems)))))

(defun grovel-system-release! (system-release)
  (let ((info (grovel-system-info (system-release/absolute-asd-pathname system-release)
                                  (system/name (system-release/system system-release)))))
    (parse-system-release-info-from-groveler! system-release info)))

(defmethod system-release/absolute-asd-pathname ((system-release git-system-release))
  (system-file/absolute-asd-pathname (system-release/system-file system-release)))

(defmethod slot-unbound (class (system-release git-system-release)
                         (slot-name (eql 'clpm/sources/defs:system-version)))
  (grovel-system-release! system-release)
  (system-release/system-version system-release))

(defmethod slot-unbound (class (system-release git-system-release)
                         (slot-name (eql 'reqs)))
  (grovel-system-release! system-release)
  (system-release/requirements system-release))


;; * cloning

(defun authenticated-git-command-list (credential-alist)
  (let ((prefix (list "git"))
        (env nil)
        (username (assoc-value credential-alist :username))
        (password (assoc-value credential-alist :password)))

    (when username
      (push "-c" prefix)
      (push (concatenate 'string "credential.username=" username) prefix))

    (when password
      (push (cons "CLPM_GIT_PASS_HELPER_PASS" password) env)
      (push "-c" prefix)
      (push "credential.helper=!f() { echo \"password=${CLPM_GIT_PASS_HELPER_PASS}\"; }; f"
            prefix))

    (values (nreverse prefix)
            env)))

(defun git-rev-parse (rev)
  (uiop:run-program `("git" "rev-parse" ,rev)
                    :output '(:string :stripped t)))

(defun fetch-release! (release)
  (let* ((project (release/project release))
         (project-cache (vcs-project/cache-directory project))
         (uri-string (git-project/git-uri-string project)))
    (multiple-value-bind (prefix env)
        (authenticated-git-command-list (git-project/git-credentials project))
      (uiop:with-current-directory (project-cache)
        (apply
         #'uiop:run-program
         `(,@prefix "fetch" "--tags" "--prune" ,uri-string "+refs/*:refs/*")
         :input :interactive
         :output :interactive
         :error-output :interactive
         (run-program-augment-env-args env))))))

(defun clone-release! (release)
  (let* ((project (release/project release))
         (project-cache (vcs-project/cache-directory project))
         (uri-string (git-project/git-uri-string project)))
    (log:info "Cloning ~A to ~A" uri-string project-cache)
    (multiple-value-bind (prefix env)
        (authenticated-git-command-list (git-project/git-credentials project))
      (apply
       #'uiop:run-program
       `(,@prefix "clone"
                  "--bare"
                  "--mirror"
                  ,uri-string
                  ,(namestring project-cache))
       :input :interactive
       :output :interactive
       :error-output :interactive
       (run-program-augment-env-args env)))))

(defun ensure-release-present-in-cache! (release)
  (let* ((project (release/project release))
         (project-cache (vcs-project/cache-directory project)))
    ;; Make sure we have enough of the repo cloned locally to have the reference
    ;; we need.
    (cond
      ((not (uiop:probe-file* project-cache))
       ;; The repo does not exist locally. We need to clone it.
       (clone-release! release))
      ((not (git-release/commit release))
       ;; We do not have a specific commit requested. Need to fetch from origin
       ;; to make sure we have the latest branches and tags.
       (fetch-release! release))
      ((not (ignore-errors
             (uiop:with-current-directory (project-cache)
               (git-rev-parse (git-release/commit release)))))
       ;; We do not have the specific commit. fetch!
       (fetch-release! release)))))

(defun ensure-release-installed! (release)
  (let* ((project (release/project release))
         (project-cache (vcs-project/cache-directory project)))

    (ensure-release-present-in-cache! release)

    ;; Get the commit sha1 for the requested ref.
    (let ((commit-id
            (uiop:with-current-directory (project-cache)
              (git-rev-parse (or (git-release/branch release)
                                 (git-release/tag release)
                                 (git-release/commit release))))))
      (setf (git-release/commit release) commit-id)
      (setf (gethash `(:commit ,commit-id)
                     (vcs-project/releases-by-spec (release/project release)))
            release))

    (let ((install-root (release/lib-pathname release))
          (archive-proc))
      (unless (uiop:probe-file* install-root)
        (log:info "installing ~A, commit ~A to ~A"
                  (project/name project)
                  (git-release/commit release)
                  install-root)
        (uiop:with-current-directory (project-cache)
          (setf archive-proc (uiop:launch-program `("git" "archive"
                                                          ,(git-release/commit release))
                                                  :output :stream)))
        (unarchive 'tar-archive (uiop:process-info-output archive-proc)
                   install-root)
        (uiop:wait-process archive-proc)))))

(defun ensure-git-release-installed! (release)
  (ensure-release-installed! release))
