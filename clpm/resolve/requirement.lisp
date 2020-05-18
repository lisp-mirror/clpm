;;;; Requirement resolution
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/resolve/requirement
    (:use #:cl
          #:alexandria
          #:clpm/log
          #:clpm/resolve/defs
          #:clpm/resolve/node
          #:clpm/requirement
          #:clpm/source)
  (:export #:*releases-sort-function*
           #:find-project-source
           #:find-requirement-source
           #:find-system-source
           #:requirement-state
           #:resolve-requirement))

(in-package #:clpm/resolve/requirement)

(setup-logger)


;; * Find Requirement Source

(defun find-project-source (project-name)
  (loop
    :for source :in *sources*
    :when (source-project source project-name nil)
      :do (return source)))

(defun find-system-source (system-name)
  (loop
    :for source :in *sources*
    :when (source-system source system-name nil)
      :do (return source)))

(defgeneric find-requirement-source (req &optional errorp)
  (:documentation "Given a requirement, return the source which provides the
requirement. ERRORP defaults to T, and if T, an error is raised if no suitable
source can be found."))

(defmethod find-requirement-source :around (req &optional (errorp t))
  "If the requirement has a specific source set, use that. Otherwise, fallback
to searching."
  (let ((out (or (get-source (requirement-source req))
                 (call-next-method))))
    (or out
        (when errorp
          (error "Unable to find a source for requirement ~S" req)))))

(defmethod find-requirement-source ((req project-requirement) &optional errorp)
  "Find the first source in *SOURCES* that provides the project. ERRORP is
handled by an :AROUND method."
  (declare (ignore errorp))
  (find-project-source (requirement-name req)))

(defmethod find-requirement-source ((req system-requirement) &optional errorp)
  "Find the first source in *SOURCES* that provides the system. ERRORP is andled
by an :AROUND method."
  (declare (ignore errorp))
  (find-system-source (requirement-name req)))

(defmethod find-requirement-source ((req vcs-project-requirement) &optional errorp)
  "Find the first source in *SOURCES* that provides the project. ERRORP is
handled by an :AROUND method."
  (declare (ignore errorp))
  (find-project-source (requirement-name req)))


;; * Requirement States

(defgeneric requirement-state (req node)
  (:documentation "Given a search node, determine if the requirement is
satisfied. Returns one of :SAT, :UNSAT, or :UNKNOWN."))

(defmethod requirement-state ((req fs-system-requirement) node)
  (let* ((system-name (requirement-name req))
         (system-release (node-find-system-if-active node system-name)))
    (cond
      ((not system-release)
       :unknown)
      ((not (eql (system-release-source system-release)
                 (requirement-source req)))
       :unsat)
      (t
       :sat))))

(defmethod requirement-state ((req fs-system-file-requirement) node)
  :unknown)

(defmethod requirement-state ((req project-requirement) node)
  "A project requirement is satisfied if a release for the project is active in
the search node, its version satisfies the requested version, and all of its
system releases are active."
  (let* ((project-name (requirement-name req))
         (version-spec (requirement-version-spec req))
         (release (node-find-project-if-active node project-name))
         (system-releases (when release (release-system-releases release))))
    (cond
      ((not release)
       :unknown)
      ((and (release-satisfies-version-spec-p release version-spec)
            (subsetp system-releases (node-activated-system-releases node)))
       (values :sat nil))
      ((release-satisfies-version-spec-p release version-spec)
       :unknown)
      (t
       :unsat))))

(defmethod requirement-state ((req system-requirement) node)
  "A system requirement is satisfied if there is an active system release that
provides the system."
  (let* ((system-name (requirement-name req))
         (version-spec (requirement-version-spec req))
         (system-release (node-find-system-if-active node system-name)))
    (cond
      ((provided-system-p system-name)
       :sat)
      ((not system-release)
       :unknown)
      ((system-release-satisfies-version-spec-p system-release version-spec)
       (values :sat system-release))
      (t
       :unsat))))

(defmethod requirement-state ((req vcs-requirement) node)
  "Right now, all VCS requirements are :UNKNOWN. This doesn't affect the search
at all, since we try to always resolve VCS requirements first."
  :unknown)


;; * Resolve Requirement

(defvar *releases-sort-function* nil)

(defgeneric resolve-requirement (req node)
  (:documentation "Given a requirement and a search node, returns an alist
representing ways to satisfy the requirement. The alist maps release objects to
a plist. This plist can contain :system-releases or :system-files."))

(defmethod resolve-requirement :around (req node)
  (let ((result (call-next-method)))
    (when *releases-sort-function*
      (setf result (funcall *releases-sort-function* result)))
    result))

(defmethod resolve-requirement ((req fs-system-requirement) node)
  ;; Make a release from the file system.
  (let* ((system-name (requirement-name req))
         (fs-source (requirement-source req))
         (system (source-system fs-source system-name))
         (releases (system-releases system))
         (release (first releases))
         (system-release (release-system-release release system-name)))
    (assert (length= 1 releases))

    (list (list release
                :system-files
                (list (cons (system-release-system-file system-release) (list system-name)))))))

(defmethod resolve-requirement ((req fs-system-file-requirement) node)
  ;; Make a release from the file system.
  (let* ((asd-pathname (requirement-name req))
         (fs-source (requirement-source req))
         (release (source-project-release fs-source (namestring asd-pathname) :newest)))
    (list (list release
                :system-files (list (cons (release-system-file release asd-pathname) t))))))

(defmethod resolve-requirement ((req project-requirement) node)
  "A project requirement is resolved by finding releases of the project that
satisfy the version spec (if any) and including every system release per
satisfying release."
  (when-let*
      ((project-name (requirement-name req))
       (source (find-requirement-source req nil))
       (project (source-project source project-name))
       (releases (project-releases project))
       (applicable-releases (remove-if-not (rcurry #'release-satisfies-version-spec-p (requirement-version-spec req))
                                           releases)))
    (mapcar (lambda (x)
              (list x :system-releases (release-system-releases x)))
            (sort applicable-releases #'release->))))

(defmethod resolve-requirement ((req system-requirement) node)
  "A system requirement is resolved by any release that provides the system."
  (when-let*
      ((system-name (requirement-name req))
       (source (find-requirement-source req))
       (system (source-system source system-name)))
    (let* ((system-releases (system-system-releases system))
           (applicable-system-releases (remove-if-not (rcurry #'system-release-satisfies-version-spec-p
                                                              (requirement-version-spec req))
                                                      system-releases)))
      (unless system-releases
        (error "No releases for ~S" system-name))
      (mapcar (lambda (x)
                (list (system-release-release x)
                      :system-releases (list x)))
              (sort applicable-system-releases #'system-release->)))))

(defmethod resolve-requirement ((req vcs-project-requirement) node)
  (declare (ignore node))
  (let* ((project-name (requirement-name req))
         (systems (requirement-systems req))
         (system-files (requirement-system-files req))
         (ref (requirement-ref req))
         (branch (requirement-branch req))
         (commit (requirement-commit req))
         (tag (requirement-tag req))
         (source (find-requirement-source req nil))
         (vcs-project (source-project source project-name nil))
         (vcs-release (project-vcs-release vcs-project
                                           :tag tag
                                           :commit commit
                                           :branch branch
                                           :ref ref))
         (release-system-files (release-system-files vcs-release)))
    (assert (null system-files))
    ;; If the systems are defined, we only need to grovel the files in which
    ;; those systems are defined. Otherwise we need to grovel all systems in
    ;; all files.
    (list (list vcs-release
                :system-files
                (if systems
                    (loop
                      :with out := nil
                      :for system :in systems
                      :for primary-system-name := (asdf:primary-system-name system)
                      :for system-file := (find primary-system-name release-system-files
                                                :test #'equalp
                                                :key (lambda (x)
                                                       (pathname-name (system-file-absolute-asd-pathname x))))
                      :do
                         (assert system-file)
                         (push system (assoc-value out system-file))
                      :finally (return out))
                    (mapcar (rcurry #'cons t) release-system-files))))))
