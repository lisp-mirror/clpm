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
          #:clpm/context
          #:clpm/repos
          #:clpm/requirement
          #:clpm/source
          #:clpm/utils)
  (:export #:clpmfile-lockfile-pathname
           #:clpmfile-pathname
           #:get-clpmfile))

(in-package #:clpm/clpmfile)


;; * Class Definitions

(defun clpmfile-pathname (clpmfile)
  (assert (context-anonymous-p clpmfile))
  (context-name clpmfile))

(defun clpmfile-lockfile-pathname (clpmfile)
  (merge-pathnames (make-pathname :type "lock")
                   (clpmfile-pathname clpmfile)))

(defgeneric get-clpmfile (clpmfile-designator))

(defmethod get-clpmfile ((clpmfile-designator context))
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
    (let ((fs-source (context-vcs-source clpmfile)))
      ;; Register the system with the fs-source
      (fs-source-register-asd fs-source asd-file)
      (if systems
          (dolist (system-name systems)
            (context-add-requirement! clpmfile
                                      (make-instance 'fs-system-requirement
                                                     :name system-name
                                                     :source fs-source
                                                     :pathname asd-file
                                                     :why t)))
          (context-add-requirement! clpmfile
                                    (make-instance 'fs-system-file-requirement
                                                   :source fs-source
                                                   :name asd-file
                                                   :why t))))))

(defmethod parse-clpmfile-form (clpmfile (type (eql :github)) args)

  "Parse a :github statement from a clpmfile into a vcs-project-requirement
instance."
  (destructuring-bind (name &key (host "github.com") path branch commit tag systems)
      args
    (let ((source (context-vcs-source clpmfile))
          (repo (make-repo-from-description (list :github :host host :path path))))
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
            (context-requirements clpmfile)))))

(defmethod parse-clpmfile-form (clpmfile (type (eql :gitlab)) args)

  "Parse a :gitlab statement from a clpmfile into a vcs-project-requirement
instance."
  (destructuring-bind (name &key (host "gitlab.com") path branch commit tag systems)
      args
    (let ((source (context-vcs-source clpmfile))
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
            (context-requirements clpmfile)))))

(defmethod parse-clpmfile-form (clpmfile (type (eql :project)) args)
  (destructuring-bind (name &key source vcs version systems) args
    (if vcs
        (destructuring-bind (&key branch commit tag)
            vcs
          (push (make-instance 'vcs-project-requirement
                               :name (string-downcase (string name))
                               :source (when source
                                         (find-source-or-error (context-user-sources clpmfile)
                                                               source))
                               :branch branch
                               :commit commit
                               :tag tag
                               :systems systems
                               :why t)
                (context-requirements clpmfile)))
        (push (make-instance 'project-requirement
                             :name (string-downcase (string name))
                             :source (when source
                                       (find-source-or-error (context-user-sources clpmfile)
                                                             source))
                             :version-spec (when version
                                             (cons (first version)
                                                   (second version)))
                             :why t)
              (context-requirements clpmfile)))))

(defmethod parse-clpmfile-form (clpmfile (type (eql :source)) args)
  "Load a :source statement from a clpmfile and add it to the list of sources."
  (unless (stringp (first args))
    (error "The first argument to :SOURCE must be a string"))
  (push (load-source-from-form args)
        (context-user-sources clpmfile)))

(defmethod parse-clpmfile-form (clpmfile (type (eql :system)) args)
  "Parse a :system statement from a clpmfile into a system-requirement
instance."
  (destructuring-bind (name &key source version) args
    (push (make-instance 'system-requirement
                         :name (string-downcase (string name))
                         :source (when source
                                   (find-source-or-error (context-user-sources clpmfile)
                                                         source))
                         :version-spec version
                         :why t)
          (context-requirements clpmfile))))

(defun read-clpmfile-from-stream (stream pathname)
  (let ((clpmfile (make-instance 'context :name pathname))
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
            (nreversef (context-user-sources clpmfile))
            (setf source-allowed-p nil))
          (parse-clpmfile-form clpmfile type args))))
    (nreversef (context-requirements clpmfile))
    clpmfile))

(defun read-clpmfile (pathname)
  "Read a clpmfile located at pathname. Returns a context object with only
sources and requirements populated."
  (let* ((*default-pathname-defaults* (uiop:pathname-directory-pathname pathname)))
    (with-open-file (stream pathname)
      (read-clpmfile-from-stream stream pathname))))
