;;;; Interface functions for clpmfiles
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/clpmfile
    (:use #:cl
          #:alexandria
          #:anaphora
          #:iterate
          #:clpm/config
          #:clpm/deps
          #:clpm/repos
          #:clpm/requirement
          #:clpm/source
          #:clpm/utils)
  (:export #:clpmfile-all-requirements
           #:clpmfile-lockfile-pathname
           #:clpmfile-pathname
           #:clpmfile-sources
           #:clpmfile-user-requirements
           #:get-clpmfile
           #:read-clpmfile))

(in-package #:clpm/clpmfile)


;; * Class Definitions

(defclass clpmfile ()
  ((pathname
    :initarg :pathname
    :accessor clpmfile-pathname
    :documentation
    "The pathname to this file.")
   (user-global-sources
    :initarg :user-global-sources
    :initform nil
    :accessor clpmfile-user-global-sources
    :documentation
    "The sources defined by the user in the clpmfile. A list of source
    objects.")
   (fs-source
    :accessor clpmfile-fs-source
    :documentation
    "The filesystem source rooted at this clpmfile's directory.")
   (user-asd-files
    :initarg :user-asd-files
    :initform nil
    :accessor clpmfile-user-asd-files
    :documentation
    "The asd files the user has specified in the clpmfile. A list where each
    element is `(path-to-asd-file &key systems)`. If `:systems` is nil, the file
    is grovelled to find all systems defined within it.")
   (user-requirements
    :initarg :user-requirements
    :initform nil
    :accessor clpmfile-user-requirements
    :documentation
    "A list of requirements specified by the user.")
   (vcs-source
    :accessor clpmfile-vcs-source
    :documentation
    "A VCS source to where raw vcs requirements can be homed."))
  (:documentation
   "Representation of a clpmfile."))

(defmethod initialize-instance :after ((clpmfile clpmfile) &key &allow-other-keys)
  "Set the ~clpmfile~'s filesystem source based on the directory where the
clpmfile is located."
  (setf (clpmfile-fs-source clpmfile)
        (make-source 'fs-source
                     :root-pathname (uiop:pathname-directory-pathname
                                     (uiop:make-pathname* :directory '(:relative)))
                     :name :clpmfile-fs))
  (setf (clpmfile-vcs-source clpmfile)
        (make-source 'vcs-source :name :clpmfile-vcs)))

(defun clpmfile-asd-file-requirements (clpmfile)
  "Return a list of requirements gathered from the ~:asd~ statements in
~clpmfile~."
  (let ((fs-source (clpmfile-fs-source clpmfile)))
    (mapcan (lambda (x)
              (destructuring-bind (asd-file &key systems)
                  x
                (if systems
                    (mapcar (lambda (system-name)
                              (make-instance 'fs-system-requirement
                                             :name system-name
                                             :source fs-source
                                             :pathname asd-file
                                             :why t))
                            systems)
                    (list (make-instance 'fs-system-file-requirement
                                         :source fs-source
                                         :name (merge-pathnames asd-file
                                                                (clpmfile-pathname clpmfile))
                                         :why t)))))
            (clpmfile-user-asd-files clpmfile))))

(defun clpmfile-all-requirements (clpmfile)
  "Return a list of all requirements specified by ~clpmfile~."
  (append
   (clpmfile-asd-file-requirements clpmfile)
   (clpmfile-user-requirements clpmfile)))

(defun clpmfile-lockfile-pathname (clpmfile)
  (merge-pathnames (make-pathname :type "lock")
                   (clpmfile-pathname clpmfile)))

(defun clpmfile-sources (clpmfile)
  "Return a list of all the sources associated with ~clpmfile~."
  (list* (clpmfile-fs-source clpmfile)
         (clpmfile-vcs-source clpmfile)
         (clpmfile-user-global-sources clpmfile)))

(defgeneric get-clpmfile (clpmfile-designator))

(defmethod get-clpmfile ((clpmfile-designator clpmfile))
  clpmfile-designator)

(defmethod get-clpmfile ((clpmfile-designator pathname))
  (read-clpmfile clpmfile-designator))

(defmethod get-clpmfile ((clpmfile-designator string))
  (get-clpmfile (pathname clpmfile-designator)))


;; * Deserializing

;; (defun parse-clpmfile-forms (clpmfile forms)
;;   "Given a list of ~forms~, parse them and register them appropriately with
;; ~clpmfile~ based on the ~:api-version~ which must be specified first in
;; ~forms~."
;;   ;; Try reading the first form to get the api version
;;   (unless (listp (first forms))
;;     (error "The first form must be a list"))
;;   (unless (eql :api-version (first (first forms)))
;;     (error "The first form must specify the api version"))
;;   (unless (equal '(:api-version "0.2") (first forms))
;;     (error "This only supports api version 0.2"))
;;   (pop forms)
;;   ;; Now that we know we're using api version 0.2, let's make sure everything
;;   ;; left is a list.
;;   (unless (every #'listp forms)
;;     (error "Every form must be a list."))
;;   (iter
;;     ;; Sources are only allowed immediately following the :api-version
;;     (with source-allowed-p := t)
;;     (for form :in forms)
;;     (for (type . args) := form)
;;     (when (and (eql type :source)
;;                (not source-allowed-p))
;;       (error "Global sources must be specified immediately after the api declaration"))
;;     (when (and
;;            source-allowed-p
;;            (not (eql type :source)))
;;       (nreversef (clpmfile-user-global-sources clpmfile))
;;       (setf source-allowed-p nil))
;;     (ecase type
;;       (:source
;;        (apply #'parse-source-statement clpmfile args))
;;       (:gitlab
;;        (push form (clpmfile/user-raw-requirements clpmfile))
;;        (apply #'parse-gitlab-statement clpmfile args))
;;       (:system
;;        (push form (clpmfile/user-raw-requirements clpmfile))
;;        (apply #'parse-system-statement clpmfile args))
;;       (:project
;;        (push form (clpmfile/user-raw-requirements clpmfile))
;;        (apply #'parse-project-statement clpmfile args))
;;       (:asd
;;        (apply #'parse-asd-statement clpmfile args))))
;;   (nreversef (clpmfile-user-asd-files clpmfile))
;;   (nreversef (clpmfile-user-requirements clpmfile))
;;   clpmfile)

(defun find-source-or-error (sources source-name)
  "Given a list of sources, return the source that is named source-name or raise
an error if it does not exist."
  (or (find source-name sources
            :key #'source-name
            :test #'string-equal)
      (error "Unable to find source ~S" source-name)))

(defgeneric parse-clpmfile-form (clpmfile type args))

(defmethod parse-clpmfile-form (clpmfile (type (eql :asd)) args)
  "Parse a :asd statement from a clpmfile and register it with the file system
source."
  (destructuring-bind (asd-file &key systems) args
    (unless (stringp asd-file)
      (error "The argument to :ASD must be a string"))
    (unless (probe-file (merge-pathnames asd-file))
      (error "The argument to :ASD must exist"))
    ;; Register the system with the fs-source
    (fs-source-register-asd (clpmfile-fs-source clpmfile)
                            asd-file)
    (push `(,asd-file ,@(when systems (list :systems systems)))
          (clpmfile-user-asd-files clpmfile))))

(defmethod parse-clpmfile-form (clpmfile (type (eql :gitlab)) args)

  "Parse a :gitlab statement from a clpmfile into a vcs-project-requirement
instance."
  (destructuring-bind (name &key (host "gitlab.com") path branch commit tag systems)
      args
    (let ((source (clpmfile-vcs-source clpmfile))
          (repo (make-repo-from-description (list :gitlab :host host :path path))))
      ;; Register the git project.
      (vcs-source-register-project! source repo name)
      (assert (xor branch commit tag))
      (push (make-instance 'vcs-project-requirement
                           :systems systems
                           :name name
                           :source source
                           :branch branch
                           :commit commit
                           :tag tag
                           :why t)
            (clpmfile-user-requirements clpmfile)))))

(defmethod parse-clpmfile-form (clpmfile (type (eql :project)) args)
  (destructuring-bind (name &key source vcs version systems) args
    (if vcs
        (destructuring-bind (&key branch commit tag)
            vcs
          (push (make-instance 'vcs-project-requirement
                               :name (string-downcase (string name))
                               :source (when source
                                         (find-source-or-error (clpmfile-user-global-sources clpmfile)
                                                               source))
                               :branch branch
                               :commit commit
                               :tag tag
                               :systems systems
                               :why t)
                (clpmfile-user-requirements clpmfile)))
        (push (make-instance 'project-requirement
                             :name (string-downcase (string name))
                             :source (when source
                                       (find-source-or-error (clpmfile-user-global-sources clpmfile)
                                                             source))
                             :version-spec (when version
                                             (cons (first version)
                                                   (second version)))
                             :why t)
              (clpmfile-user-requirements clpmfile)))))

(defmethod parse-clpmfile-form (clpmfile (type (eql :source)) args)
  "Load a :source statement from a clpmfile and add it to the list of sources."
  (unless (stringp (first args))
    (error "The first argument to :SOURCE must be a string"))
  (push (load-source-from-form args)
        (clpmfile-user-global-sources clpmfile)))

(defmethod parse-clpmfile-form (clpmfile (type (eql :system)) args)
  "Parse a ~:system~ statement from a ~clpmfile~ into a ~system-requirement~
instance."
  (destructuring-bind (name &key source version) args
    (push (make-instance 'system-requirement
                         :name (string-downcase (string name))
                         :source (when source
                                   (find-source-or-error (clpmfile-user-global-sources clpmfile)
                                                         source))
                         :version-spec version
                         :why t)
          (clpmfile-user-requirements clpmfile))))

(defun read-clpmfile-from-stream (stream pathname)
  (let ((clpmfile (make-instance 'clpmfile :pathname pathname))
        (source-allowed-p t))
    (uiop:with-safe-io-syntax ()
      ;; The first form in the stream must be an API declaration.
      (let ((f (read stream nil)))
        (unless (equal f '(:api-version "0.3"))
          (error "Unknown clpmfile API version")))

      (with-forms-from-stream (stream form)
        (destructuring-bind (type . args) form
          (when (and (eql type :source)
                     (not source-allowed-p))
            (error "Global sources must be specified immediately after the API declaration"))
          (when (and source-allowed-p
                     (not (eql type :source)))
            (nreversef (clpmfile-user-global-sources clpmfile))
            (setf source-allowed-p nil))
          (parse-clpmfile-form clpmfile type args)))
      (nreversef (clpmfile-user-asd-files clpmfile))
      (nreversef (clpmfile-user-requirements clpmfile))
      clpmfile)))

(defun read-clpmfile (pathname)
  "Read a ~clpmfile~ instance from ~pathname~."
  (let* ((*default-pathname-defaults* (uiop:pathname-directory-pathname pathname)))
    (with-open-file (stream pathname)
      (read-clpmfile-from-stream stream pathname))))

(defmethod print-object ((obj clpmfile) stream)
  (print-unreadable-object (obj stream :type t)
    (terpri stream)
    (prin1 :user-global-sources stream)
    (write-char #\Space stream)
    (write (clpmfile-user-global-sources obj) :stream stream)
    (write-char #\Space stream)
    (terpri stream)

    (prin1 :user-asd-files stream)
    (write-char #\Space stream)
    (write (clpmfile-user-asd-files obj) :stream stream)
    (write-char #\Space stream)
    (terpri stream)

    (prin1 :user-requirements stream)
    (write-char #\Space stream)
    (write (clpmfile-user-requirements obj) :stream stream)))


;; (defun find-source-or-error (sources source-name)
;;   "Given a list of ~sources~, return the source that has is named ~source-name~
;; or raise an error if it does not exist."
;;   (or (find source-name sources
;;             :key #'source-name
;;             :test #'string-equal)
;;       (error "Unable to find source ~S" source-name)))

;; (defun parse-req-statement (clpmfile name
;;                             &key (type :project) version source
;;                               systems)
;;   "Parse a ~:req~ statement from a ~clpmfile~ into a ~requirement~ instance."
;;   (assert (null systems))
;;   (let ((req nil))
;;     (setf req
;;           (make-instance (ecase type
;;                            (:project 'project-requirement)
;;                            (:system 'system-requirement))
;;                          :name (string-downcase (string name))
;;                          :source (when source
;;                                    (find-source-or-error (clpmfile-user-global-sources clpmfile)
;;                                                          source))
;;                          :version-spec (when version
;;                                          (cons (first version)
;;                                                (second version)))))
;;     (push req (clpmfile-user-requirements clpmfile))))

;; (defun parse-project-statement (clpmfile name
;;                                 &key source vcs version
;;                                   systems)
;;   (if vcs
;;       (destructuring-bind (&key branch commit tag)
;;           vcs
;;         (push (make-instance 'vcs-project-requirement
;;                              :name (string-downcase (string name))
;;                              :source (when source
;;                                        (find-source-or-error (clpmfile-user-global-sources clpmfile)
;;                                                              source))
;;                              :branch branch
;;                              :commit commit
;;                              :tag tag
;;                              :systems systems)
;;               (clpmfile-user-requirements clpmfile)))
;;       (push (make-instance 'project-requirement
;;                            :name (string-downcase (string name))
;;                            :source (when source
;;                                      (find-source-or-error (clpmfile-user-global-sources clpmfile)
;;                                                            source))
;;                            :version-spec (when version
;;                                            (cons (first version)
;;                                                  (second version))))
;;             (clpmfile-user-requirements clpmfile))))

;; (defun parse-system-statement (clpmfile name &key source)
;;   "Parse a ~:system~ statement from a ~clpmfile~ into a ~system-requirement~
;; instance."
;;   (push (make-instance 'system-requirement
;;                        :name (string-downcase (string name))
;;                        :source (when source
;;                                  (find-source-or-error (clpmfile-user-global-sources clpmfile)
;;                                                        source)))
;;         (clpmfile-user-requirements clpmfile)))

;; (defun parse-gitlab-statement (clpmfile name
;;                                &key (host "gitlab.com") path branch commit tag systems)
;;   "Parse a ~:gitlab~ statement from a ~clpmfile~ into a
;; ~vcs-project-requirement~ instance."
;;   (let ((source (clpmfile/vcs-source clpmfile))
;;         (repo (make-repo-from-description (list :gitlab :host host :path path))))
;;     ;; Register the git project.
;;     (vcs-source-register-project! source repo name)
;;     (assert (xor branch commit tag))
;;     (push (make-instance 'vcs-project-requirement
;;                          :systems systems
;;                          :name name
;;                          :source source
;;                          :repo repo
;;                          :branch branch
;;                          :commit commit
;;                          :tag tag)
;;           (clpmfile-user-requirements clpmfile))))


;; (defun parse-source-statement (clpmfile &rest args)
;;   "Load a ~:source~ statement from a ~clpmfile~ and add it to the list of
;; sources."
;;   (unless (stringp (first args))
;;     (error "The first argument to :SOURCE must be a string"))
;;   (push (load-source-from-form args)
;;         (clpmfile-user-global-sources clpmfile)))

;; (defun parse-clpmfile-forms (clpmfile forms)
;;   "Given a list of ~forms~, parse them and register them appropriately with
;; ~clpmfile~ based on the ~:api-version~ which must be specified first in
;; ~forms~."
;;   ;; Try reading the first form to get the api version
;;   (unless (listp (first forms))
;;     (error "The first form must be a list"))
;;   (unless (eql :api-version (first (first forms)))
;;     (error "The first form must specify the api version"))
;;   (unless (equal '(:api-version "0.2") (first forms))
;;     (error "This only supports api version 0.2"))
;;   (pop forms)
;;   ;; Now that we know we're using api version 0.2, let's make sure everything
;;   ;; left is a list.
;;   (unless (every #'listp forms)
;;     (error "Every form must be a list."))
;;   (iter
;;     ;; Sources are only allowed immediately following the :api-version
;;     (with source-allowed-p := t)
;;     (for form :in forms)
;;     (for (type . args) := form)
;;     (when (and (eql type :source)
;;                (not source-allowed-p))
;;       (error "Global sources must be specified immediately after the api declaration"))
;;     (when (and
;;            source-allowed-p
;;            (not (eql type :source)))
;;       (nreversef (clpmfile-user-global-sources clpmfile))
;;       (setf source-allowed-p nil))
;;     (ecase type
;;       (:source
;;        (apply #'parse-source-statement clpmfile args))
;;       (:gitlab
;;        (push form (clpmfile/user-raw-requirements clpmfile))
;;        (apply #'parse-gitlab-statement clpmfile args))
;;       (:system
;;        (push form (clpmfile/user-raw-requirements clpmfile))
;;        (apply #'parse-system-statement clpmfile args))
;;       (:project
;;        (push form (clpmfile/user-raw-requirements clpmfile))
;;        (apply #'parse-project-statement clpmfile args))
;;       (:asd
;;        (apply #'parse-asd-statement clpmfile args))))
;;   (nreversef (clpmfile-user-asd-files clpmfile))
;;   (nreversef (clpmfile-user-requirements clpmfile))
;;   clpmfile)

;; (defgeneric parse-system-file-statement (lockfile type &key &allow-other-keys)
;;   (:documentation
;;    "Parse a system file statement from a lockfile."))

;; (defmethod parse-system-file-statement (lockfile (type (eql :project))
;;                                         &key name source version system-files)
;;   (let* ((source (find source (clpmfile-user-global-sources (lockfile/clpmfile lockfile))
;;                        :key #'source-name
;;                        :test #'equal))
;;          (release (source-project-release source name version)))
;;     (dolist (system-file-name system-files)
;;       (push (release-system-file release system-file-name) (lockfile/system-files lockfile)))
;;     lockfile))

;; (defmethod parse-system-file-statement (lockfile (type (eql :local-asd))
;;                                         &key path)
;;   (fs-source-register-asd (clpmfile-fs-source (lockfile/clpmfile lockfile)) path)
;;   (let* ((source (clpmfile-fs-source (lockfile/clpmfile lockfile)))
;;          (project (source-project source "all"))
;;          (release (project-release project "newest"))
;;          (system-file (release-system-file release path)))
;;     (assert system-file)
;;     (push system-file (lockfile/system-files lockfile))
;;     lockfile))

;; (defmethod parse-system-file-statement (lockfile (statement-type (eql :gitlab))
;;                                         &key host path commit system-files
;;                                           name)
;;   ;; First, look for a local override.
;;   (let ((local (config-value :bundle :local name :path)))
;;     (if local
;;         ;; A local override exists.
;;         (error "local overrides currently broken.")
;;         (let* ((source (clpmfile/vcs-source (lockfile/clpmfile lockfile)))
;;                (repo (make-repo-from-description `(:gitlab :host ,host :path ,path)))
;;                (project (vcs-source-register-project! source repo name))
;;                (release (project-release project `(:commit ,commit)))
;;                (system-files (mapcar (curry #'release-system-file release) system-files)))
;;           (setf (lockfile/system-files lockfile)
;;                 (append system-files (lockfile/system-files lockfile))))))
;;   lockfile)

;; (defun parse-lockfile-forms (lockfile forms)
;;   "Given a list of ~forms~, parse them and register them appropriately with
;; ~lockfile~ based on the ~:api-version~ which must be specified first in
;; ~forms~."
;;   ;; Try reading the first form to get the api version
;;   (unless (listp (first forms))
;;     (error "The first form must be a list"))
;;   (unless (eql :api-version (first (first forms)))
;;     (error "The first form must specify the api version"))
;;   (unless (equal '(:api-version "0.2") (first forms))
;;     (error "This version of CLPM only supports api version 0.2"))
;;   (pop forms)
;;   ;; Now that we know we're using API version 0.2, let's make sure everything
;;   ;; left is a list.
;;   (unless (every #'listp forms)
;;     (error "every form must be a list."))
;;   (let ((clpmfile (make-instance 'clpmfile
;;                                  :pathname (lockfile-clpmfile-pathname (lockfile/pathname lockfile))))
;;         (sources-form (find :user-global-sources forms :key #'car))
;;         ;;(user-asd-files-form (find :user-asd-files forms :key #'car))
;;         ;;(user-reqs-form (find :user-reqs forms :key #'car))
;;         (all-system-files-form (find :all-system-files forms :key #'car)))
;;     (setf (lockfile/clpmfile lockfile) clpmfile)
;;     (setf (clpmfile-user-global-sources clpmfile)
;;           (mapcar #'load-source-from-form (cdr sources-form)))
;;     (mapc (lambda (x)
;;             (apply #'parse-system-file-statement
;;                    lockfile (first x)
;;                    (rest x)))
;;           (cdr all-system-files-form))))

;; (defun lockfile-system-file-sexps (lockfile)
;;   "Return a list of system file statements from ~lockfile~ suitable for writing
;; to a file."
;;   (let* ((all-system-files (lockfile/system-files lockfile)))
;;     (iter
;;       (for system-file := (pop all-system-files))
;;       (while system-file)
;;       (for release := (system-file-release system-file))
;;       (for system-files-in-release := (list* system-file
;;                                              (remove-if-not (lambda (x)
;;                                                               (eql release (system-file-release x)))
;;                                                             all-system-files)))
;;       (setf all-system-files (remove-if (lambda (x)
;;                                           (eql release (system-file-release x)))
;;                                         all-system-files))

;;       (cond
;;         ((typep release 'fs-release)
;;          (dolist (file system-files-in-release)
;;            (collect `(:local-asd
;;                       :path ,(system-file-asd-enough-namestring file)))))
;;         ;; TODO: Make this handle local vs remote git repos
;;         ((typep release 'vcs-release)
;;          (let* ((version (release-version release))
;;                 (project (release-project release))
;;                 (repo (project-repo project)))
;;            (assert (listp version))
;;            (assert (eql :commit (first version)))
;;            (assert (stringp (second version)))

;;            (collect `(,(etypecase repo
;;                          (gitlab-repo
;;                           :gitlab))
;;                       :name ,(project-name (release-project release))
;;                       :host ,(gitlab-repo-host repo)
;;                       :path ,(gitlab-repo-path repo)
;;                       :commit ,(second version)
;;                       :system-files ,(mapcar #'system-file-asd-enough-namestring system-files-in-release)))))
;;         (t
;;          (collect `(:project
;;                     :name ,(project-name (release-project release))
;;                     :version ,(release-version release)
;;                     :source ,(source-name (release-source release))
;;                     :system-files ,(mapcar #'system-file-asd-enough-namestring system-files-in-release))))))))

;; (defun write-lockfile-to-stream (lockfile stream)
;;   "Write ~lockfile~ to ~stream~."
;;   (uiop:with-safe-io-syntax ()
;;     (let ((*print-case* :downcase)
;;           (clpmfile (lockfile/clpmfile lockfile)))
;;       (write-string ";;; -*- mode: common-lisp -*-" stream)
;;       (terpri stream)

;;       (write '(:api-version "0.2") :stream stream)
;;       (terpri stream)

;;       (write `(:user-global-sources ,@(mapcar #'source-to-form (clpmfile-user-global-sources clpmfile)))
;;              :stream stream)
;;       (terpri stream)

;;       (write `(:user-asd-files ,@(clpmfile-user-asd-files clpmfile))
;;              :stream stream)
;;       (terpri stream)

;;       (write `(:user-reqs ,@(clpmfile/user-raw-requirements clpmfile))
;;              :stream stream
;;              :pretty t
;;              :right-margin 120)
;;       (terpri stream)

;;       (write `(:all-system-files
;;                ,@(lockfile-system-file-sexps lockfile))
;;              :stream stream
;;              :pretty t
;;              :right-margin 120)
;;       (terpri stream))))

;; (defun clpmfile/lockfile-pathname (clpmfile)
;;   "The pathname to the lockfile associated with this clpmfile."
;;   (merge-pathnames (make-pathname :type "lock")
;;                    (clpmfile-pathname clpmfile)))

;; (defun lockfile-clpmfile-pathname (lockfile-pathname)
;;   "The pathname to the clpmfile associated with this lockfile pathname."
;;   (make-pathname :type nil :defaults lockfile-pathname))

;; (defun clpmfile/lockfile (clpmfile)
;;   "Return the lockfile object associated with this clpmfile."
;;   (read-lockfile (clpmfile/lockfile-pathname clpmfile)))

;; (defun clpmfile-asd-file-requirements (clpmfile)
;;   "Return a list of requirements gathered from the ~:asd~ statements in
;; ~clpmfile~."
;;   (let ((fs-source (clpmfile-fs-source clpmfile)))
;;     (mapcan (lambda (x)
;;               (destructuring-bind (asd-file &key systems)
;;                   x
;;                 (if systems
;;                     (mapcar (lambda (system-name)
;;                               (make-instance 'fs-system-requirement
;;                                              :name system-name
;;                                              :source fs-source))
;;                             systems)
;;                     (list (make-instance 'fs-system-file-requirement
;;                                          :source fs-source
;;                                          :name (merge-pathnames asd-file
;;                                                                 (clpmfile-pathname clpmfile)))))))
;;             (clpmfile-user-asd-files clpmfile))))

;; (defun clpmfile/all-requirements (clpmfile)
;;   "Return a list of all requirements specified by ~clpmfile~."
;;   (append
;;    (clpmfile-asd-file-requirements clpmfile)
;;    (clpmfile-user-requirements clpmfile)))

;; (defun read-clpmfile (pathname)
;;   "Read a ~clpmfile~ instance from ~pathname~."
;;   (let* ((*default-pathname-defaults* (uiop:pathname-directory-pathname pathname))
;;          (forms (uiop:with-safe-io-syntax ()
;;                   (uiop:read-file-forms pathname)))
;;          (clpmfile (make-instance 'clpmfile
;;                                   :pathname pathname)))
;;     (parse-clpmfile-forms clpmfile forms)
;;     clpmfile))

;; (defun read-lockfile (pathname)
;;   "Read a ~lockfile~ instance from ~pathname~."
;;   (let* ((*default-pathname-defaults* (uiop:pathname-directory-pathname pathname))
;;          (forms (uiop:with-safe-io-syntax ()
;;                   (uiop:read-file-forms pathname)))
;;          (lockfile (make-instance 'lockfile
;;                                   :pathname pathname)))
;;     (parse-lockfile-forms lockfile forms)
;;     lockfile))
