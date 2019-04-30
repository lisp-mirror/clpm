;;;; Requirement resolution

;; * package definition

(uiop:define-package #:clpm/resolve
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/deps
          #:clpm/install
          #:clpm/requirement
          #:clpm/source
          #:iterate)
  (:export #:resolve-requirement
           #:resolve-requirements))

(in-package #:clpm/resolve)


;; * Search nodes

(defclass search-node ()
  ((sources
    :initarg :sources
    :accessor search-node/sources
    :documentation "The sources available during the search.")
   (activated-system-releases
    :initarg :activated-system-releases
    :initform nil
    :accessor search-node/activated-system-releases
    :documentation "A list of system release objects that are currently active.")
   (unresolved-reqs
    :initarg :unresolved-reqs
    :initform nil
    :accessor search-node/unresolved-reqs
    :documentation "A list of requirements that have not yet been resolved.")
   (data
    :accessor search-node/data
    :documentation "Slot to store data for the next children.")))

(defun copy-search-node (search-node)
  "Given a search node, return a shallow copy of it, excluding the data slot."
  (make-instance 'search-node
                 :sources (search-node/sources search-node)
                 :unresolved-reqs (search-node/unresolved-reqs search-node)
                 :activated-system-releases (search-node/activated-system-releases search-node)))

(defun search-done-p (search-node)
  "Returns T iff the search is complete at SEARCH-NODE."
  (null (search-node/unresolved-reqs search-node)))

(defmethod print-object ((node search-node) stream)
  "Print a representation of NODE to STREAM."
  (print-unreadable-object (node stream :type t)
    (terpri stream)
    (prin1 :activated-system-releases stream)
    (write-char #\Space stream)
    (write (search-node/activated-system-releases node) :stream stream)

    (terpri stream)
    (prin1 :unresolved-reqs stream)
    (write-char #\Space stream)
    (write (search-node/unresolved-reqs node) :stream stream)

    (when (slot-boundp node 'data)
      (terpri stream)
      (prin1 :data stream)
      (write-char #\Space stream)
      (write (search-node/data node) :stream stream))))

(defun search-node-find-system (search-node system)
  "Find a system object, if it is activated in the search node."
  (find system (search-node/activated-system-releases search-node)
        :test #'string-equal
        :key (compose #'system/name #'system-release/system)))

(defun find-system-in-sources (system-name sources)
  "Given a list of sources (typically taken from a search node), find a system
by name."
  (loop
    :for source :in sources
    :for s := (source/system source system-name)
    :when s
      :do (return s)))

(defun find-project-in-sources (project-name sources)
  "Given a list of sources (typically taken from a search node), find a project
by name."
  (loop
    :for source :in sources
    :for p := (source/project source project-name)
    :when p
      :do (return p)))


;; * resolve individual requirements

(defgeneric resolve-requirement (req sources)
  (:documentation "Given a requirement and a list of sources, returns an alist
representing satisfying solutions of the requirement. The alist maps release
objects to a list of system-releases."))

(defmethod resolve-requirement ((req git-project-requirement) sources)
  (let* ((project-name (requirement/name req))
         (systems (requirement/systems req))
         (system-files (requirement/system-files req))
         (branch (requirement/branch req))
         (commit (requirement/commit req))
         (tag (requirement/tag req))
         (source (requirement/source req))
         (vcs-project (source/project source project-name))
         (vcs-release (project/release vcs-project
                                       (cond
                                         (commit
                                          `(:commit ,commit))
                                         (branch
                                          `(:branch ,branch))
                                         (tag
                                          `(:tag ,tag))))))
    (assert (null system-files))
    (if systems
        (setf systems (mapcar (lambda (system-name)
                                (release/system-release vcs-release system-name))
                              systems))
        (setf systems (release/system-releases vcs-release)))
    (list (cons vcs-release systems))))

(defmethod resolve-requirement ((req project-requirement) sources)
  (let* ((project-name (requirement/name req))
         (version-spec (requirement/version-spec req))
         (project (find-project-in-sources project-name sources))
         (releases (project/releases project))
         (applicable-releases (remove-if-not (rcurry #'release-satisfies-version-spec-p version-spec)
                                             releases)))
    (mapcar (lambda (x)
              (cons x (release/system-releases x)))
            (sort applicable-releases #'release->))))

(defmethod resolve-requirement ((req system-requirement) sources)
  (let* ((system-name (requirement/name req))
         (version-spec (requirement/version-spec req))
         (system (find-system-in-sources system-name sources))
         (system-releases (system/system-releases system))
         (applicable-system-releases (remove-if-not (rcurry #'system-release-satisfies-version-spec-p
                                                            version-spec)
                                                    system-releases)))
    (mapcar (lambda (x)
              (cons (system-release/release x)
                    (list x)))
            (sort applicable-system-releases #'system-release->))))

(defmethod resolve-requirement ((req fs-system-requirement) sources)
  ;; Make a release from the file system.
  (let* ((system-name (requirement/name req))
         (fs-source (requirement/source req))
         (system (source/system fs-source system-name))
         (releases (system/releases system)))

    (mapcar (lambda (release)
              (cons release
                    (list (release/system-release release system-name))))
            releases)))

(defmethod resolve-requirement ((req fs-system-file-requirement) sources)
  (let* ((system-pathname (requirement/name req))
         (fs-source (requirement/source req))
         (system-file (fs-source-register-asd fs-source system-pathname)))

    (list (cons (system-file/release system-file) (system-file/system-releases system-file)))))


;; * requirement (un)satisfication

(defparameter *sb-contribs*
  '("sb-aclrepl" "sb-bsd-sockets" "sb-capstone" "sb-cltl2" "sb-concurrency" "sb-cover"
    "sb-executable" "sb-gmp" "sb-grovel" "sb-introspect" "sb-md5" "sb-mpfr" "sb-posix"
    "sb-queue" "sb-rotate-byte" "sb-rt" "sb-simple-streams" "sb-sprof")
  "SBCL contrib systems.")

(defun provided-system-p (system-name)
  (or (equal "asdf" (asdf:primary-system-name system-name))
      (equal "uiop" (asdf:primary-system-name system-name))
      (member system-name *sb-contribs* :test #'equal)))

(defgeneric requirement-state (search-node req)
  (:documentation "Given a search node, determine if the requirement is
satisfied. Returns one of :SAT, :UNSAT, or :UNKNOWN."))

(defmethod requirement-state (search-node (req fs-system-requirement))
  (let* ((system-name (requirement/name req))
         (system-release (search-node-find-system search-node system-name)))
    (cond
      ((not system-release)
       :unknown)
      ((not (eql (system-release/source system-release)
                 (requirement/source req)))
       :unsat)
      (t
       :sat))))

(defmethod requirement-state (search-node (req system-requirement))
  (let* ((system-name (requirement/name req))
         (version-spec (requirement/version-spec req))
         (system-release (search-node-find-system search-node system-name)))
    (cond
      ((provided-system-p system-name)
       :sat)
      ((not system-release)
       :unknown)
      ((system-release-satisfies-version-spec-p system-release version-spec)
       (values :sat system-release))
      (t
       :unsat))))

(defmethod requirement-state (search-node (req git-requirement))
  :unknown)


;; * cleanup

(defun cleanup-search-node! (search-node)
  "Returns two values. The first is the search node with all satisfied
requirements removed. The second is T iff no requirements were violated, NIL
otherwise."
  (setf (search-node/unresolved-reqs search-node)
        (iter
          (for r :in (search-node/unresolved-reqs search-node))
          (for state := (requirement-state search-node r))
          (ecase state
            (:unknown
             (collect r))
            (:sat)
            (:unsat
             (return-from cleanup-search-node!
               (values search-node nil))))))
  (values search-node t))


;; * next child

(defun next-child (search-node)
  "Given a search node, return the search node representing its next child or
nil (if there are no remaining children)."
  ;; First time we're visiting this node. Generate its children.
  (handler-bind
      ((groveler-dependency-missing
         (lambda (c)
           (let* ((missing-system-spec (groveler-dependency-missing/system c))
                  (missing-req (convert-asd-system-spec-to-req missing-system-spec)))
             (multiple-value-bind (status satisfying-system-release)
                 (requirement-state search-node missing-req)

               (ecase status
                 (:sat
                  (unless (release-installed-p (system-release/release satisfying-system-release))
                    (install-release (system-release/release satisfying-system-release)))
                  (invoke-restart 'add-asd-and-retry
                                  (system-release/absolute-asd-pathname satisfying-system-release)))
                 (:unsat
                  (return-from next-child nil))
                 (:unknown
                  (setf (search-node/data search-node) nil)
                  (return-from next-child
                    (values
                     (aprog1 (copy-search-node search-node)
                       (push missing-req (search-node/unresolved-reqs it)))
                     t)))))))))
    (unless (slot-boundp search-node 'data)
      ;; Look at the first unresolved requirement and resolve it.
      (let* ((this-req (first (search-node/unresolved-reqs search-node)))
             (resolutions (resolve-requirement this-req (search-node/sources search-node))))
        (setf (search-node/data search-node) resolutions)))

    ;; While we still have children, generate search nodes for them.
    (when (search-node/data search-node)
      (let ((out (copy-search-node search-node)))
        (destructuring-bind (release . system-releases)
            (pop (search-node/data search-node))
          (declare (ignore release))
          ;; Pop off this satisfied requirement
          (pop (search-node/unresolved-reqs out))
          ;; Add all activated system releases.
          (setf (search-node/activated-system-releases out)
                (append
                 system-releases
                 (search-node/activated-system-releases out)))

          ;; Add the unresolved requirements to the new search node.
          (setf (search-node/unresolved-reqs out)
                (append (apply #'append (mapcar #'system-release/requirements system-releases))
                        (search-node/unresolved-reqs out)))
          ;; Cleanup the search node and return it
          (cleanup-search-node! out))))))


;; * resolve all requirements

(defun req-implies-system-file-p (req)
  (or (typep req 'fs-system-file-requirement)
      (typep req 'fs-system-requirement)
      (typep req 'git-project-requirement)))

(defun resolve-requirements (reqs sources &key no-deps)
  "Given a list of sources and requirements, returns an alist representing a
satisfying solution of the requirements. The alist maps release objects to a
list of system-releases. If NO-DEPS is non-NIL, don't resolve dependencies."
  ;; This is probably the most complicated part of CLPM. It could still benefit
  ;; from improvements (especially since some pathological cases could be
  ;; constructed that take an extremely long time to resolve), but this
  ;; implementation should be correct. The non-trivial aspect of this is, of
  ;; course, grovelling for dependencies from ASD files (particularly for :asd
  ;; or :req (with VCS options enabled) directives in clpmfiles) instead of
  ;; relying on metadata, especially when such systems have the
  ;; :defsystem-depends-on option specified.
  (let* ((root-node (make-instance 'search-node
                                   :sources sources
                                   :unresolved-reqs reqs)))
    (if no-deps
        (mapcar #'system-release/release
                (search-node/activated-system-releases (next-child root-node)))
        (iter
          (with tree := (list root-node))
          (for node-to-expand := (first tree))
          (for (values next-child valid-p) := (next-child node-to-expand))
          (unless next-child
            (pop tree)
            (when tree
              (next-iteration))
            (error "Unable to resolve requirements."))
          (when valid-p
            (push next-child tree)
            (when (search-done-p next-child)
              (return (values (remove-duplicates
                               (mapcar #'system-release/release
                                       (search-node/activated-system-releases next-child)))
                              (search-node/activated-system-releases next-child)))))))))
