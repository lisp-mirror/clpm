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
                                         :name asd-file
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
