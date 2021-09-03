;;;; Sources for projects taken from source control. A VCS source requires a
;;;; repo to be checked out before much can be done with it.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

;; * define package

(uiop:define-package #:clpm/sources/vcs
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/archives
          #:clpm/groveler
          #:clpm/install/defs
          #:clpm/log
          #:clpm/repos
          #:clpm/requirement
          #:clpm/session
          #:clpm/sources/defs
          #:clpm/utils)
  (:export #:*vcs-project-override-fun*
           #:ensure-vcs-release-installed!
           #:make-vcs-release
           #:vcs-local-release
           #:vcs-project/cache-directory
           #:vcs-project/path
           #:vcs-release
           #:vcs-release-commit
           #:vcs-remote-release
           #:vcs-source
           #:vcs-source-clear-visible-releases
           #:vcs-source-project
           #:vcs-system))

(in-package #:clpm/sources/vcs)

(setup-logger)


;; * sources

(defclass vcs-source (clpm-source)
  ((name
    :initarg :name
    :reader source-name)
   (repo
    :initarg :repo
    :reader vcs-source-repo)
   (project
    :accessor vcs-source-project
    :documentation "The single project for the source.")
   (systems-by-name
    :initform (make-hash-table :test 'equalp)
    :accessor vcs-source-systems-by-name
    :documentation "A hash table mapping system names to system objects."))
  (:documentation
   "A source for any bare VCS projects. Projects must be registered with the
source using VCS-SOURCE-REGISTER_PROJECT!."))

(defmethod make-source ((type (eql 'vcs-source)) &key repo project-name)
  (let* ((repo-object (make-repo-from-description repo))
         (name (repo-to-form repo-object)))
    (format t "~S~%" project-name)
    (with-clpm-session (:key `(make-source ,type ,name))
      (make-instance type :name name
                          :project-name project-name
                          :repo repo-object))))

(defmethod initialize-instance :after ((source vcs-source) &key project-name)
  (setf (vcs-source-project source) (make-instance 'vcs-project
                                                   :source source
                                                   :name project-name)))

(defun vcs-source-clear-visible-releases (vcs-source)
  (setf (vcs-project-visible-releases (vcs-source-project vcs-source)) nil))

(defmethod source-ensure-system ((source vcs-source) system-name)
  (ensure-gethash
   system-name (vcs-source-systems-by-name source)
   (make-instance 'vcs-system
                  :source source
                  :name system-name)))

(defmethod source-can-lazy-sync-p ((source vcs-source))
  t)

(defmethod source-project ((source vcs-source) project-name &optional (error t))
  (if (equal project-name (project-name (vcs-source-project source)))
      (vcs-source-project source)
      (when error
        (error 'source-missing-project
               :source source
               :project-name project-name))))

(defmethod source-system ((source vcs-source) system-name &optional (error t))
  (or (gethash system-name (vcs-source-systems-by-name source))
      (some (lambda (release)
              (when-let ((system-release (release-system-release release system-name nil)))
                (system-release-system system-release)))
            (project-releases (vcs-source-project source)))
      (when error
        (error 'source-missing-system
               :source source
               :system-name system-name))))

(defmethod source-to-form ((source vcs-source))
  (let ((project (vcs-source-project source)))
    `(:implicit-vcs
      :type :vcs
      :projects ((,(project-name project) . ,(repo-to-form (project-repo project)))))))

(defmethod sync-source ((source vcs-source))
  nil)


;; * projects

;; ** Overrides

(defvar *vcs-project-override-fun* (constantly nil)
  "A function that, given a project name returns either NIL or a directory
pathname. If a directory pathname is returned, then that is assumed to be a
local override when requesting a VCS release.")

(defun make-vcs-release (source project ref
                         &key (local-release-class 'vcs-local-release)
                           (remote-release-class 'vcs-remote-release))
  (let ((override-pathname (funcall *vcs-project-override-fun* (project-name project))))
    (if override-pathname
        (make-instance local-release-class
                       :source source
                       :project project
                       :ref ref
                       :repo (make-instance 'local-git-override-repo
                                            :pathname override-pathname))
        (make-instance remote-release-class
                       :source source
                       :project project
                       :ref ref))))

;; ** Projects

(defclass vcs-project (clpm-project)
  ((source
    :initarg :source
    :reader project-source)
   (name
    :initarg :name
    :reader project-name)
   (visible-releases
    :initform nil
    :accessor vcs-project-visible-releases)
   (releases-by-spec
    :initform (make-hash-table :test 'equal)
    :accessor vcs-project-releases-by-spec)))

(defmethod project-repo ((project vcs-project))
  (vcs-source-repo (project-source project)))

(defmethod project-releases ((project vcs-project))
  (vcs-project-visible-releases project))

(defmethod project-release ((project vcs-project) (version string) &optional (error t))
  "A release named by a string is assumed to refer to a commit."
  (project-release project `(:commit ,version) error))

(defmethod project-release ((project vcs-project) (version list) &optional error)
  (declare (ignore error))
  (apply #'project-vcs-release project version))

(defmethod project-vcs-release ((project vcs-project) &key commit branch tag ref)
  (let* ((ref (cond
                (commit `(:commit ,commit))
                (branch `(:branch ,branch))
                (tag `(:tag ,tag))
                (ref `(:ref ,ref))))
         (release (ensure-gethash ref (vcs-project-releases-by-spec project)
                                  (make-vcs-release (project-source project) project ref))))
    (pushnew release (vcs-project-visible-releases project))
    (unless commit
      (setf release (ensure-gethash `(:commit ,(vcs-release-commit release)) (vcs-project-releases-by-spec project)
                                    release)))
    release))


;; * releases

(defclass vcs-release ()
  ((source
    :initarg :source
    :reader release-source)
   (project
    :initarg :project
    :reader release-project)
   (commit
    :initarg :commit
    :accessor vcs-release-commit)
   (installed-p
    :initform nil
    :accessor vcs-release-installed-p)

   (system-files-by-namestring
    :accessor vcs-release-system-files-by-namestring)
   (system-files-by-primary-name
    :accessor vcs-release-system-files-by-primary-name)
   (system-releases-by-name
    :initform (make-hash-table :test 'equal)
    :accessor vcs-release-system-releases-by-name)))

(defclass vcs-remote-release (vcs-release)
  ()
  (:documentation "Represents a release fetched from a remote repository."))

(defclass vcs-local-release (vcs-release)
  ((installed-p
    :initform t)
   (repo
    :initarg :repo
    :reader vcs-release-repo))
  (:documentation "Represents a release that uses a user managed local repository."))

(defmethod initialize-instance :after ((release vcs-remote-release) &rest initargs &key ref)
  (declare (ignore initargs))
  (vcs-release-resolve! release ref))

(defmethod initialize-instance :after ((release vcs-local-release) &rest initargs &key ref)
  (declare (ignore initargs))
  (destructuring-bind (ref-type ref-name) ref
    (ecase ref-type
      (:branch
       ;; Error unless the target branch name matches the actual branch name.
       (let ((current-branch-name (repo-current-branch (vcs-release-repo release))))
         (unless (equal ref-name current-branch-name)
           (log:error "In order to use local overrides, branch names must match. Expected: ~a, actual: ~a"
                      ref-name current-branch-name)
           (error "In order to use local overrides, branch names must match. Expected: ~a, actual: ~a"
                  ref-name current-branch-name))))
      (:commit
       ;; Warn if the desired commit is not present in the repo.
       (unless (ref-present-p (vcs-release-repo release) ref)
         (log:warn "Commit ~A is not present in the local override. Your local repo may not be up to date!"
                   ref-name)
         (warn "Commit ~A is not present in the local override. Your local repo may not be up to date!"
               ref-name)))))
  (setf (vcs-release-commit release) (repo-current-commit (vcs-release-repo release))))

(defmethod vcs-release-repo ((release vcs-remote-release))
  (project-repo (release-project release)))

(defun populate-release-system-files! (release)
  (ensure-release-installed! release)
  (let ((ht-by-namestring (make-hash-table :test 'equal))
        (ht-by-primary-name (make-hash-table :test 'equalp)))
    (asdf::collect-sub*directories-asd-files
     (release-lib-pathname release)
     :collect (lambda (x)
                (let* ((namestring (enough-namestring x (release-lib-pathname release)))
                       (system-file (make-instance 'vcs-system-file
                                                   :relative-pathname namestring
                                                   :release release
                                                   :source (release-source release)))
                       (primary-system-name (pathname-name x)))
                  (setf (gethash namestring ht-by-namestring)
                        system-file)
                  (setf (gethash primary-system-name ht-by-primary-name)
                        system-file))))
    (setf (vcs-release-system-files-by-namestring release) ht-by-namestring)
    (setf (vcs-release-system-files-by-primary-name release) ht-by-primary-name)))

(defmethod slot-unbound (class (release vcs-release) (slot-name (eql 'system-files-by-namestring)))
  (populate-release-system-files! release)
  (vcs-release-system-files-by-namestring release))

(defmethod slot-unbound (class (release vcs-release) (slot-name (eql 'system-files-by-primary-name)))
  (populate-release-system-files! release)
  (vcs-release-system-files-by-primary-name release))

(defmethod release-system-files ((release vcs-release))
  (hash-table-values (vcs-release-system-files-by-namestring release)))

(defmethod release-system-file ((release vcs-release) system-file-namestring)
  "Requires the release to be installed. Return a system file object if not
already created."
  (gethash system-file-namestring (vcs-release-system-files-by-namestring release)))

(defmethod release-lib-pathname ((release vcs-remote-release))
  (let ((repo (vcs-release-repo release))
        (commit-string (vcs-release-commit release)))
    (uiop:resolve-absolute-location
     `(,(repo-lib-base-pathname repo)
       ,(subseq commit-string 0 2)
       ,(subseq commit-string 2 4)
       ,commit-string)
     :ensure-directory t)))

(defmethod release-lib-pathname ((release vcs-local-release))
  (git-repo-local-dir (vcs-release-repo release)))

(defmethod release-system-releases ((release vcs-release))
  "Get all the system files and append together their systems."
  (let* ((system-files (release-system-files release)))
    (apply #'append (mapcar #'system-file-system-releases system-files))))

(defmethod release-system-release ((release vcs-release) system-name &optional (error t))
  (unless (gethash system-name (vcs-release-system-releases-by-name release))
    (let* ((system-file (gethash (asdf:primary-system-name system-name)
                                 (vcs-release-system-files-by-primary-name release))))
      (when system-file
        (let* ((source (release-source release))
               (system (source-ensure-system source system-name))
               (system-release (make-instance 'vcs-system-release
                                              :release release
                                              :source (release-source release)
                                              :system system
                                              :system-file system-file)))
          (system-register-release! system release)
          (setf (gethash system-name (vcs-system-file-system-releases-by-name system-file))
                system-release)
          (setf (gethash system-name (vcs-release-system-releases-by-name release))
                system-release)))))
  (or
   (gethash system-name (vcs-release-system-releases-by-name release))
   (when error
     (error 'release-missing-system-release
            :source (release-source release)
            :release release
            :system-name system-name))))

(defmethod release-version ((release vcs-release))
  (list :commit (vcs-release-commit release)))


;; * system files

(defclass vcs-system-file (clpm-system-file)
  ((source
    :initarg :source
    :accessor system-file-source)
   (release
    :initarg :release
    :accessor system-file-release)
   (relative-pathname
    :initarg :relative-pathname
    :accessor system-file-asd-enough-namestring)
   (system-releases-by-name
    :initform (make-hash-table :test 'equalp)
    :accessor vcs-system-file-system-releases-by-name)
   (groveled-p
    :initform nil
    :accessor vcs-system-file/groveled-p)))

(defmethod system-file-absolute-asd-pathname ((system-file vcs-system-file))
  "Merge the enough pathname with the release's lib pathname."
  (merge-pathnames (system-file-asd-enough-namestring system-file)
                   (release-lib-pathname (system-file-release system-file))))

(defmethod system-file-system-releases ((system-file vcs-system-file))
  "Grovel over the system file if necessary to determine every system it contains."
  (unless (vcs-system-file/groveled-p system-file)
    ;; Sigh. We need to grovel the file to make sure we know ~everything it
    ;; defines.
    (active-groveler-ensure-asd-loaded (system-file-absolute-asd-pathname system-file))
    (let ((system-names (active-groveler-systems-in-file (system-file-absolute-asd-pathname system-file))))
      (dolist (system-name system-names)
        ;; Get the system release from the release (which will register it in
        ;; the system-releases-by-name slot of this object).
        (let* ((release (system-file-release system-file))
               (system-release (release-system-release release system-name)))
          system-release))
      (setf (vcs-system-file/groveled-p system-file) t)))
  (hash-table-values (vcs-system-file-system-releases-by-name system-file)))


;; * systems

(defclass vcs-system (clpm-system)
  ((source
    :initarg :source
    :reader system-source)
   (name
    :initarg :name
    :reader system-name)
   (releases
    :initform nil
    :accessor vcs-system-releases)))

(defmethod system-system-releases ((system vcs-system))
  (mapcar #'(lambda (x)
              (release-system-release x (system-name system)))
          (vcs-system-releases system)))

(defmethod system-register-release! ((system vcs-system) release)
  (pushnew release (vcs-system-releases system)))

(defmethod system-releases ((system vcs-system))
  (copy-list (vcs-system-releases system)))


;; * system-releases

(defclass vcs-system-release (clpm-system-release)
  ((source
    :initarg :source
    :reader system-release-source)
   (release
    :initarg :release
    :reader system-release-release)
   (system
    :initarg :system
    :reader system-release-system)
   (system-version
    :accessor system-release-system-version)
   (reqs
    :accessor system-release-requirements)
   (system-file
    :initarg :system-file
    :accessor system-release-system-file)))

(defun parse-system-release-info-from-groveler! (system-release info)
  "Take the info provided by the groveler and modify system-release in place to
include it."
  (log:debug "Parsing from groveler: ~S" info)
  (destructuring-bind (&key version depends-on defsystem-depends-on loaded-systems
                       &allow-other-keys)
      info
    (setf (system-release-system-version system-release) version)
    (setf (system-release-requirements system-release)
          (mapcar #'convert-asd-system-spec-to-req
                  (append depends-on defsystem-depends-on loaded-systems)))))

(defun grovel-system-release! (system-release)
  (active-groveler-ensure-asd-loaded (system-release-absolute-asd-pathname system-release))
  (let ((info (active-groveler-system-deps (system-name (system-release-system system-release)))))
    (parse-system-release-info-from-groveler! system-release info)))

(defmethod system-release-absolute-asd-pathname ((system-release vcs-system-release))
  (system-file-absolute-asd-pathname (system-release-system-file system-release)))

(defmethod system-release-satisfies-version-spec-p ((system-release vcs-system-release) version-spec)
  t)

(defmethod slot-unbound (class (system-release vcs-system-release)
                         (slot-name (eql 'clpm/sources/defs:system-version)))
  (grovel-system-release! system-release)
  (system-release-system-version system-release))

(defmethod slot-unbound (class (system-release vcs-system-release)
                         (slot-name (eql 'reqs)))
  (grovel-system-release! system-release)
  (system-release-requirements system-release))



;; * Installing

(defun vcs-release-resolve! (release ref)
  (let ((repo (vcs-release-repo release)))

    ;; We delay making sure commits are present locally until we want to
    ;; install. This ensures that if a commit is deleted from a repository we
    ;; don't get into a situation where we can't even parse the context.
    (if (eql (first ref) :commit)
        (setf (vcs-release-commit release) (second ref))
        (progn
          (ensure-ref-present-locally! repo ref)
          (let ((commit-id (resolve-ref-to-commit repo ref)))
            (setf (vcs-release-commit release) commit-id))))))

(defmethod ensure-release-installed! ((release vcs-release))
  (unless (vcs-release-installed-p release)
    (let ((project (release-project release))
          (repo (vcs-release-repo release)))

      (ensure-ref-present-locally! repo `(:commit ,(vcs-release-commit release)))

      (let ((install-root (release-lib-pathname release)))
        (unless (uiop:probe-file* install-root)
          (log:info "installing ~A, commit ~A to ~A"
                    (project-name project)
                    (vcs-release-commit release)
                    install-root)
          (multiple-value-bind (stream archive-type)
              (repo-archive-stream repo `(:commit ,(vcs-release-commit release)))
            (with-open-stream (stream stream)
              (unarchive archive-type stream install-root))))))

    (setf (vcs-release-installed-p release) t)))

(defmethod install-release ((release vcs-release))
  (ensure-release-installed! release))
