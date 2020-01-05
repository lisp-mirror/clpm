;;;; Sources for projects taken from source control. A VCS source requires a
;;;; repo to be checked out before much can be done with it, so it shares a lot
;;;; of code with fs-sources. But it ensures that the repo is checked out before
;;;; it falls back to fs methods
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
          #:clpm/repos
          #:clpm/requirement
          #:clpm/sources/defs
          #:clpm/sources/semantic-versioned-project)
  (:export #:ensure-vcs-release-installed!
           #:vcs-project/cache-directory
           #:vcs-project/path
           #:vcs-release
           #:vcs-release/commit
           #:vcs-source
           #:vcs-source-register-project!))

(in-package #:clpm/sources/vcs)


;; * sources

(defclass vcs-source (clpm-source)
  ((projects-by-name
    :initform (make-hash-table :test 'equal)
    :accessor vcs-source-projects-by-name
    :documentation "hash table mapping project names to project objects.")
   (systems-by-name
    :initform (make-hash-table :test 'equalp)
    :accessor vcs-source-systems-by-name
    :documentation "A hash table mapping system names to system objects."))
  (:documentation "The source for any orphaned VCS projects."))

(defun vcs-source-register-project! (vcs-source repo project-name)
  "Registers a project with the vcs source and returns it."
  (let ((projects-by-name (vcs-source-projects-by-name vcs-source)))
    (ensure-gethash project-name projects-by-name
                    (make-instance 'vcs-project
                                   :repo repo
                                   :source vcs-source
                                   :name project-name))))

(defmethod source-project ((source vcs-source) project-name)
  (gethash project-name (vcs-source-projects-by-name source)))

(defmethod source-system ((source vcs-source) system-name)
  (or (gethash system-name (vcs-source-systems-by-name source))
      (some (lambda (project)
              (some (lambda (release)
                      (when-let ((system-release (release-system-release release system-name)))
                        (system-release-system system-release)))
                    (project-releases project)))
            (hash-table-values (vcs-source-projects-by-name source)))))


;; * projects

(defclass vcs-project (clpm-project)
  ((releases-by-spec
    :initform (make-hash-table :test 'equal)
    :accessor vcs-project-releases-by-spec)))

(defmethod project-releases ((project vcs-project))
  (hash-table-values (vcs-project-releases-by-spec project)))

(defmethod project-release ((project vcs-project) (version string))
  "A release named by a string is assumed to refer to a commit."
  (project-release project `(:commit ,version)))

(defmethod project-release ((project vcs-project) (version list))
  "The version can be one of (:tag TAG-NAME), (:branch BRANCH-NAME), or (:commit
  COMMIT-STRING)"
  (destructuring-bind (version-type version-string)
      version
    (check-type version-type (member :tag :branch :commit))
    (check-type version-string string)
    (ensure-gethash version (vcs-project-releases-by-spec project)
                    (apply #'make-instance
                           'remote-vcs-release
                           :project project
                           :source (project-source project)
                           version))))

(defmethod project-releases ((project vcs-project))
  (hash-table-values (vcs-project-releases-by-spec project)))


;; * releases

(defclass vcs-release (clpm-release)
  ((system-files-by-namestring
    :accessor vcs-release-system-files-by-namestring)
   (system-files-by-primary-name
    :accessor vcs-release-system-files-by-primary-name)
   (system-releases-by-name
    :initform (make-hash-table :test 'equal)
    :accessor vcs-release-system-releases-by-name)))

(defclass remote-vcs-release (vcs-release)
  ((tag
    :initarg :tag
    :initform nil
    :accessor vcs-release/tag)
   (branch
    :initarg :branch
    :initform nil
    :accessor vcs-release/branch)
   (commit
    :initarg :commit
    :initform nil
    :accessor vcs-release/commit)))

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
                                                   :source (release-source release))))
                  (setf (gethash namestring ht-by-namestring)
                        system-file)
                  (setf (gethash (pathname-name x) ht-by-primary-name)
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

(defmethod release-lib-pathname ((release remote-vcs-release))
  (assert (vcs-release/commit release))
  (let* ((project (release-project release))
         (repo (project-repo project))
         (commit-string (vcs-release/commit release)))
    (uiop:resolve-absolute-location
     `(,(repo-lib-base-pathname repo)
       ,(subseq commit-string 0 2)
       ,(subseq commit-string 2 4)
       ,commit-string)
     :ensure-directory t)))

(defmethod release-system-releases ((release vcs-release))
  "Get all the system files and append together their systems."
  (let* ((system-files (release-system-files release)))
    (apply #'append (mapcar #'system-file-system-releases system-files))))

(defmethod release-system-release ((release vcs-release) system-name)
  (unless (gethash system-name (vcs-release-system-releases-by-name release))
    (let* ((system-file (gethash (asdf:primary-system-name system-name)
                                 (vcs-release-system-files-by-primary-name release))))
      (when system-file
        (let* ((source (release-source release))
               (system (ensure-gethash system-name (vcs-source-systems-by-name source)
                                       (make-instance 'vcs-system
                                                      :name system-name
                                                      :source source)))
               (system-release (make-instance 'vcs-system-release
                                              :release release
                                              :source (release-source release)
                                              :system system
                                              :system-file system-file)))
          (push release (vcs-system-releases system))
          (setf (gethash system-name (vcs-system-file-system-releases-by-name system-file))
                system-release)
          (setf (gethash system-name (vcs-release-system-releases-by-name release))
                system-release)))))
  (gethash system-name (vcs-release-system-releases-by-name release)))

(defmethod release-version ((release remote-vcs-release))
  (acond
    ((vcs-release/commit release)
     `(:commit ,it))
    ((vcs-release/tag release)
     `(:tag ,it))
    ((vcs-release/branch release)
     `(:branch ,it))))


;; * system files

(defclass vcs-system-file (clpm-system-file)
  ((relative-pathname
    :initarg :relative-pathname
    :accessor vcs-system-file/relative-pathname
    :accessor system-file-asd-enough-namestring)
   (system-releases-by-name
    :initform (make-hash-table :test 'equalp)
    :accessor vcs-system-file-system-releases-by-name)
   (groveled-p
    :initform nil
    :accessor vcs-system-file/groveled-p)))

(defmethod system-file-absolute-asd-pathname ((system-file vcs-system-file))
  "Merge the enough pathname with the release's lib pathname."
  (merge-pathnames (vcs-system-file/relative-pathname system-file)
                   (release-lib-pathname (system-file-release system-file))))

(defmethod system-file-system-releases ((system-file vcs-system-file))
  "Grovel over the system file if necessary to determine every system it contains."
  (unless (vcs-system-file/groveled-p system-file)
    ;; Sigh. We need to grovel the file to make sure we know ~everything it
    ;; defines.
    (active-groveler-ensure-asd-loaded! (system-file-absolute-asd-pathname system-file))
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
  ((releases
    :initform nil
    :accessor vcs-system-releases)))

(defmethod system-system-releases ((system vcs-system))
  (mapcar #'(lambda (x)
              (release-system-release x (system-name system)))
          (vcs-system-releases system)))

(defmethod system-releases ((system vcs-system))
  (copy-list (vcs-system-releases system)))


;; * system-releases

(defclass vcs-system-release (semantic-versioned-system-release clpm-system-release)
  ((reqs
    :accessor system-release-requirements)
   (system-file
    :initarg :system-file
    :accessor system-release-system-file)))

(defun parse-system-release-info-from-groveler! (system-release info)
  "Take the info provided by the groveler and modify system-release in place to
include it."
  (destructuring-bind (system-name
                       &key version depends-on defsystem-depends-on loaded-systems
                       &allow-other-keys)
      info
    (declare (ignore system-name))
    (setf (system-release-system-version system-release) version)
    (setf (system-release-requirements system-release)
          (mapcar #'convert-asd-system-spec-to-req
                  (append depends-on defsystem-depends-on loaded-systems)))))

(defun grovel-system-release! (system-release)
  (active-groveler-ensure-asd-loaded! (system-release-absolute-asd-pathname system-release))
  (let ((info (active-groveler-system-deps (system-name (system-release-system system-release)))))
    (parse-system-release-info-from-groveler! system-release info)))

(defmethod system-release-absolute-asd-pathname ((system-release vcs-system-release))
  (system-file-absolute-asd-pathname (system-release-system-file system-release)))

(defmethod slot-unbound (class (system-release vcs-system-release)
                         (slot-name (eql 'clpm/sources/defs:system-version)))
  (grovel-system-release! system-release)
  (system-release-system-version system-release))

(defmethod slot-unbound (class (system-release vcs-system-release)
                         (slot-name (eql 'reqs)))
  (grovel-system-release! system-release)
  (system-release-requirements system-release))



;; * Installing

(defmethod ensure-release-installed! ((release remote-vcs-release))
  (let* ((project (release-project release))
         (repo (project-repo project)))

    (ensure-ref-present-locally! repo
                                 :tag (vcs-release/tag release)
                                 :branch (vcs-release/branch release)
                                 :commit (vcs-release/commit release))

    (let ((commit-id (resolve-ref-to-commit repo
                                            :tag (vcs-release/tag release)
                                            :branch (vcs-release/branch release)
                                            :commit (vcs-release/commit release))))
      (setf (vcs-release/commit release) commit-id
            (vcs-release/branch release) nil
            (vcs-release/tag release) nil)
      (setf (gethash `(:commit ,commit-id)
                     (vcs-project-releases-by-spec (release-project release)))
            release))
    (let ((install-root (release-lib-pathname release)))
      (unless (uiop:probe-file* install-root)
        (log:info "installing ~A, commit ~A to ~A"
                  (project-name project)
                  (vcs-release/commit release)
                  install-root)
        (multiple-value-bind (stream archive-type)
            (repo-archive-stream repo
                                 :tag (vcs-release/tag release)
                                 :branch (vcs-release/branch release)
                                 :commit (vcs-release/commit release))
          (with-open-stream (strem stream)
            (unarchive archive-type stream install-root)))))))

(defun ensure-vcs-release-installed! (release)
  (ensure-release-installed! release))
