;;;; Requirement resolution
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

;; * package definition

(uiop:define-package #:clpm/resolve-2
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/context
          #:clpm/deps
          #:clpm/groveler
          #:clpm/install/defs
          #:clpm/log
          #:clpm/requirement
          #:clpm/source
          #:iterate)
  (:export #:resolve-requirement
           #:resolve-requirements))

(in-package #:clpm/resolve-2)

(setup-logger)

;; * Explanation
;;
;;   This is probably the most complicated part of CLPM. It could still benefit
;;   from improvements, but this implementation should be correct. The
;;   non-trivial aspect of this is, of course, grovelling for dependencies from
;;   ASD files (particularly for :asd or vcs directives in clpmfiles) instead
;;   of relying on metadata, especially when such systems have the
;;   :defsystem-depends-on option specified.
;;
;;   The primary task performed by this package is going from requirements to a
;;   minimal set of releases that resolve all the requirements. The general
;;   approach taken is to perform a search over the space of all releases until
;;   a satisfying one is found. However, the space of all releases is enormous,
;;   so we need to guide it somehow.
;;
;;   The state of the search at any point in the search tree is described by
;;   five things:
;;
;;   1. A list of unresolved requirements. It starts with all provided
;;      requirements.
;;   2. A list of unresolved requirements needed to grovel. It starts empty and
;;      the search is complete when both this list and the previous list are
;;      empty.
;;   3. A list of activated releases. These are the releases chosen so far to
;;      satisfy some requirements.
;;   4. A list of activated system releases. These represent systems provided
;;      by the releases that are actually required (either directly or
;;      indirectly).
;;   5. A list of system files that need to be added to the groveler.
;;
;;
;; * Search state

(defclass search-state ()
  ((unresolved-reqs
    :initarg :unresolved-reqs
    :initform nil
    :accessor search-state-unresolved-reqs
    :documentation
    "A list of requirements that have not yet been resolved.")
   (unresolved-grovel-req
    :initarg :unresolved-grovel-reqs
    :initform nil
    :accessor search-state-unresolved-grovel-reqs
    :documentation
    "A list of requirements that need to be resolved before groveling the
    dependencies of the next system release.")
   (activated-releases
    :initarg :activated-releases
    :initform nil
    :accessor search-state-activated-releases
    :documentation
    "An alist of releases that are activated and the requirements they
    satisfy.")
   (activated-system-releases
    :initarg :activated-system-releases
    :initform nil
    :accessor search-state-activated-system-releases
    :documentation
    "A list of system releases that are activated.")
   (system-files-pending-groveling
    :initarg :system-files-pending-groveling
    :initform nil
    :accessor search-state-system-files-pending-groveling
    :documentation
    "An alist of system files that need groveling.")
   (grovler-loaded-asds
    :initarg :groveler-loaded-asds
    :initform nil
    :accessor search-state-groveler-loaded-asds
    :documentation
    "A list of system files loaded into the groveler.")))

(defun copy-search-state (search-state)
  "Given a search state, return a shallow copy of it."
  (make-instance 'search-state
                 :unresolved-reqs (search-state-unresolved-reqs search-state)
                 :unresolved-grovel-reqs (search-state-unresolved-grovel-reqs search-state)
                 :activated-releases (copy-alist (search-state-activated-releases search-state))
                 :activated-system-releases (search-state-activated-system-releases search-state)
                 :system-files-pending-groveling (copy-alist (search-state-system-files-pending-groveling search-state))
                 :groveler-loaded-asds (search-state-groveler-loaded-asds search-state)))

(defun collapse-system-files-needing-groveling (left right)
  (if (or (eql left t)
          (eql right t))
      t
      (union left right :test #'equalp)))

(defun search-state-add-resolution! (search-state release system-releases system-files
                                     reason)
  (push reason (assoc-value (search-state-activated-releases search-state) release))

  (dolist (sr system-releases)
    (unless (member sr (search-state-activated-system-releases search-state))
      (push sr (search-state-activated-system-releases search-state))))

  (dolist (sf-desc system-files)
    (setf (assoc-value (search-state-system-files-pending-groveling search-state)
                       (car sf-desc))
          (collapse-system-files-needing-groveling
           (assoc-value (search-state-system-files-pending-groveling search-state)
                        (car sf-desc))
           (cdr sf-desc))))

  search-state)

(defun search-state-find-project (search-state project-name)
  "Find a release object for a project, if it is activated in the search state."
  (car (find project-name (search-state-activated-releases search-state)
             :test #'string-equal
             :key (compose #'project-name #'release-project #'car))))

(defun search-state-find-system (search-state system)
  "Find a system object, if it is activated in the search state."
  (find system (search-state-activated-system-releases search-state)
        :test #'string-equal
        :key (compose #'system-name #'system-release-system)))


;; * Search nodes

(defclass search-node ()
  ((state
    :initarg :state
    :accessor search-node-state)
   (groveler
    :initarg :groveler
    :accessor search-node-groveler)
   (child-generator
    :accessor search-node-child-generator)))

(defun make-search-node (old-search-node search-state)
  (make-instance 'search-node
                 :state search-state
                 :groveler (search-node-groveler old-search-node)))

(defun search-done-p (search-node)
  "Returns T iff the search is complete at SEARCH-NODE."
  (and (null (search-state-unresolved-reqs (search-node-state search-node)))
       (null (search-state-unresolved-grovel-reqs (search-node-state search-node)))
       (null (search-state-system-files-pending-groveling (search-node-state search-node)))))

(defun search-node-load-asd-in-groveler! (search-node asd-pathname)
  "Load the asd file into the groveler. If the current groveler is
incompatible, a new one is created."
  (let* ((search-state (search-node-state search-node))
         (asd-pathname (uiop:ensure-absolute-pathname asd-pathname))
         (asd-namestring (namestring asd-pathname)))
    (log:debug "Attempting to add ~S to groveler" asd-pathname)
    (cond
      ((set-equal (search-state-groveler-loaded-asds search-state)
                  (groveler-loaded-asds (search-node-groveler search-node))
                  :test #'equal)
       ;; This groveler is compatible, load the asd.
       (groveler-load-asd! (search-node-groveler search-node) asd-pathname)
       (push asd-namestring (search-state-groveler-loaded-asds search-state)))
      ((set-equal (list* asd-namestring (search-state-groveler-loaded-asds search-state))
                  (groveler-loaded-asds (search-node-groveler search-node))
                  :test #'equal)
       ;; This groveler already has the target system loaded. Don't need to do
       ;; anything.
       (push asd-namestring (search-state-groveler-loaded-asds search-state)))
      (t
       ;; Grovelers are incompatible. Need to launch a new one.
       (log:debug "Starting new groveler.~%search state asds: ~S~%groveler asds:~S"
                  (search-state-groveler-loaded-asds search-state)
                  (groveler-loaded-asds (search-node-groveler search-node)))
       (setf (search-node-groveler search-node) (make-groveler))
       (dolist (f (reverse (search-state-groveler-loaded-asds search-state)))
         (groveler-load-asd! (search-node-groveler search-node) f))
       (groveler-load-asd! (search-node-groveler search-node) asd-pathname)
       (push asd-namestring (search-state-groveler-loaded-asds search-state))))))

(defun compute-child-generator/unresolved-reqs (search-node)
  (let* ((search-state (search-node-state search-node))
         (*active-groveler* (search-node-groveler search-node))
         (unresolved-req (first (search-state-unresolved-reqs search-state)))
         (resolutions (resolve-requirement unresolved-req
                                           search-state)))
    (lambda ()
      (destructuring-bind (release &key system-releases) (pop resolutions)
        (let* ((new-search-state (copy-search-state search-state))
               (new-search-node (make-search-node search-node new-search-state))
               (*active-groveler* (search-node-groveler new-search-node)))
          ;; Remove the thing we just resolved.
          (pop (search-state-unresolved-reqs new-search-state))
          ;; Add the chosen resolution.
          (search-state-add-resolution! new-search-state release system-releases nil unresolved-req)
          ;; And push its requirements.
          (unless (requirement/no-deps-p unresolved-req)
            (dolist (sr system-releases)
              (let ((new-reqs (system-release-requirements sr)))
                ;; Mark the reason for the new reqs
                (mapc (lambda (r)
                        (setf (requirement/why r) sr))
                      new-reqs)
                (setf (search-state-unresolved-reqs new-search-state)
                      (append new-reqs
                              (search-state-unresolved-reqs new-search-state))))))
          (values new-search-node (not (null resolutions))))))))

(defun compute-child-generator (search-node)
  (let* ((search-state (search-node-state search-node))
         (unresolved-vcs-or-fs-req (find-if (lambda (x)
                                              (or (typep x 'vcs-requirement)
                                                  (typep x 'fs-system-requirement)
                                                  (typep x 'fs-system-file-requirement)))
                                            (search-state-unresolved-reqs search-state))))
    (cond
      ;; (unresolved-vcs-or-fs-req
      ;;  (log:debug "Have an unresolved vcs or fs req")
      ;;  ;; We will generate one child. That child will resolve this req, without
      ;;  ;; adding its dependencies yet...
      ;;  (let* ((new-search-state (copy-search-state search-state))
      ;;         (new-search-node (make-search-node search-node new-search-state))
      ;;         (resolutions (resolve-requirement unresolved-vcs-or-fs-req search-state)))
      ;;    ;; If there's not exactly one resolution available, something is
      ;;    ;; wrong.
      ;;    (assert (length= 1 resolutions))
      ;;    (destructuring-bind ((release &key system-files)) resolutions
      ;;      (search-state-add-resolution! new-search-state release nil system-files)
      ;;      ;; Install the release so we can grovel it later...
      ;;      (unless (release-installed-p release)
      ;;        (install-release release)))
      ;;    (removef (search-state-unresolved-reqs new-search-state)
      ;;             unresolved-vcs-or-fs-req)
      ;;    (lambda ()
      ;;      (values new-search-node nil))))
      ;; ((search-state-unresolved-grovel-reqs search-state)
      ;;  (log:debug "Have an unresolved grovel requirement")
      ;;  ;; We need to resolve some requirements before we can grovel further.
      ;;  (let* ((*active-groveler* (search-node-groveler search-node))
      ;;         (resolutions (resolve-requirement (first (search-state-unresolved-grovel-reqs search-state))
      ;;                                           search-state)))
      ;;    (lambda ()
      ;;      (destructuring-bind (release &key system-releases) (pop resolutions)
      ;;        (let* ((new-search-state (copy-search-state search-state))
      ;;               (new-search-node (make-search-node search-node new-search-state)))
      ;;          ;; Remove the thing we just resolved.
      ;;          (pop (search-state-unresolved-grovel-reqs new-search-state))
      ;;          ;; Add the chosen resolution.
      ;;          (search-state-add-resolution! new-search-state release system-releases nil)
      ;;          (values new-search-node (not (null resolutions))))))))
      ;; ((search-state-system-files-pending-groveling search-state)
      ;;  (log:debug "Have a system file to grovel")
      ;;  (destructuring-bind (sf . system-names)
      ;;      (first (search-state-system-files-pending-groveling search-state))

      ;;    (let* ((new-search-state (copy-search-state search-state))
      ;;           (new-search-node (make-search-node search-node new-search-state)))
      ;;      (handler-bind
      ;;          ((groveler-dependency-missing
      ;;             (lambda (c)
      ;;               (let* ((missing-system-spec (groveler-dependency-missing/system c))
      ;;                      (missing-req (convert-asd-system-spec-to-req missing-system-spec)))
      ;;                 (multiple-value-bind (status satisfying-system-release)
      ;;                     (requirement-state missing-req search-state)

      ;;                   (ecase status
      ;;                     (:sat
      ;;                      (unless (release-installed-p (system-release-release satisfying-system-release))
      ;;                        (install-release (system-release-release satisfying-system-release)))
      ;;                      (log:debug "Groveler is missing ~S, but it is already in resolution. Adding it."
      ;;                                 missing-system-spec)
      ;;                      (invoke-restart 'add-asd-and-retry
      ;;                                      (system-release-absolute-asd-pathname satisfying-system-release)
      ;;                                      (lambda ()
      ;;                                        (log:debug "Groveler successfully added ~S. Adding ~S to search state."
      ;;                                                   missing-system-spec
      ;;                                                   (namestring (system-release-absolute-asd-pathname satisfying-system-release)))
      ;;                                        (push (namestring (system-release-absolute-asd-pathname satisfying-system-release))
      ;;                                              (search-state-groveler-loaded-asds new-search-state)))))
      ;;                     (:unsat
      ;;                      (return-from compute-child-generator (lambda () (values nil nil))))
      ;;                     (:unknown
      ;;                      (return-from compute-child-generator
      ;;                        (lambda ()
      ;;                          (push missing-req (search-state-unresolved-grovel-reqs new-search-state))
      ;;                          (values new-search-node nil))))))))))
      ;;        (search-node-load-asd-in-groveler! new-search-node (system-file-absolute-asd-pathname sf))

      ;;        ;; Groveller is primed. Get the requirements from it.
      ;;        (let ((*active-groveler* (search-node-groveler new-search-node))
      ;;              (system-releases nil))
      ;;          (if (eql t system-names)
      ;;              (setf system-releases (system-file-system-releases sf))
      ;;              (setf system-releases (mapcar (lambda (sn)
      ;;                                              (release-system-release (system-file-release sf) sn))
      ;;                                            system-names)))

      ;;          ;; Remove the SF we just computed the deps for.
      ;;          (pop (search-state-system-files-pending-groveling new-search-state))

      ;;          (dolist (sr system-releases)
      ;;            (let ((reqs (system-release-requirements sr)))
      ;;              ;; Add the deps we just computed.
      ;;              (setf (search-state-unresolved-reqs new-search-state)
      ;;                    (append reqs (search-state-unresolved-reqs new-search-state)))))
      ;;          ;; activate the system releases
      ;;          (search-state-add-resolution! new-search-state (system-file-release sf)
      ;;                                        system-releases nil)
      ;;          (lambda ()
      ;;            (values new-search-node nil)))))))
      ((search-state-unresolved-reqs search-state)
       (log:debug "Have an unresolved requirement")
       (compute-child-generator/unresolved-reqs search-node))
      (t
       (error "Should not get here")))))

(defmethod slot-unbound (class (self search-node) (slot-name (eql 'child-generator)))
  (setf (search-node-child-generator self) (compute-child-generator self)))


;; * cleanup

(defun cleanup-search-node! (search-node)
  "Returns two values. The first is the search node with all satisfied
requirements removed. The second is T iff no requirements were violated, NIL
otherwise."
  (log:debug "In cleanup-search-node")
  (let ((search-state (search-node-state search-node)))
    (setf (search-state-unresolved-reqs search-state)
          (iter
            (for r :in (search-state-unresolved-reqs search-state))
            (for (values state satisfying-system-release) := (requirement-state r search-state))
            (ecase state
              (:unknown
               (collect r))
              (:sat
               (awhen satisfying-system-release
                 (search-state-add-resolution! search-state (system-release-release it) nil nil r)))
              (:unsat
               (return-from cleanup-search-node!
                 (values search-node nil)))))))
  (values search-node t))



(defvar *sources* nil)

(defmethod find-sources-for-req ((req system-requirement))
  (aif (requirement/source req)
       (list it)
       (remove-if-not (rcurry #'source-system (requirement/name req) nil) *sources*)))

(defun find-system-in-sources (system-name)
  "Given a list of sources (typically taken from a search node), find a system
by name."
  (loop
    :for source :in *sources*
    :for s := (source-system source system-name nil)
    :when s
      :do (return s)))

(defun find-project-in-sources (project-name)
  "Given a list of sources (typically taken from a search node), find a project
by name."
  (loop
    :for source :in *sources*
    :for p := (source-project source project-name nil)
    :when p
      :do (return p)))


;; * Requirement testing

(defparameter *sb-contribs*
  '("sb-aclrepl" "sb-bsd-sockets" "sb-capstone" "sb-cltl2" "sb-concurrency" "sb-cover"
    "sb-executable" "sb-gmp" "sb-grovel" "sb-introspect" "sb-md5" "sb-mpfr" "sb-posix"
    "sb-queue" "sb-rotate-byte" "sb-rt" "sb-simple-streams" "sb-sprof")
  "SBCL contrib systems.")

(defun provided-system-p (system-name)
  (or (equal "asdf" (asdf:primary-system-name system-name))
      (equal "uiop" (asdf:primary-system-name system-name))
      (member system-name *sb-contribs* :test #'equal)))

(defgeneric requirement-state (req search-state)
  (:documentation "Given a search state, determine if the requirement is
satisfied. Returns one of :SAT, :UNSAT, or :UNKNOWN."))

(defmethod requirement-state ((req fs-system-requirement) search-state)
  (let* ((system-name (requirement/name req))
         (system-release (search-state-find-system search-state system-name)))
    (cond
      ((not system-release)
       :unknown)
      ((not (eql (system-release-source system-release)
                 (requirement/source req)))
       :unsat)
      (t
       :sat))))

(defmethod requirement-state ((req project-requirement) search-state)
  (let* ((project-name (requirement/name req))
         (version-spec (requirement/version-spec req))
         (release (search-state-find-project search-state project-name)))
    (cond
      ((not release)
       :unknown)
      ((release-satisfies-version-spec-p release version-spec)
       (values :sat nil))
      (t
       :unsat))))

(defmethod requirement-state ((req system-requirement) search-state)
  (let* ((system-name (requirement/name req))
         (version-spec (requirement/version-spec req))
         (system-release (search-state-find-system search-state system-name)))
    (cond
      ((provided-system-p system-name)
       :sat)
      ((not system-release)
       :unknown)
      ((system-release-satisfies-version-spec-p system-release version-spec)
       (values :sat system-release))
      (t
       :unsat))))

(defmethod requirement-state ((req vcs-requirement) search-state)
  :unknown)


;; * Resolve requirement source
(defgeneric resolve-requirement-source! (req)
  (:documentation
   "Given a requirement, modify it in place so that its source is fixed."))

(defmethod resolve-requirement-source! :around (req)
  (unless (requirement/source req)
    (call-next-method))
  (assert (requirement/source req))
  req)

(defmethod resolve-requirement-source! (req)
  (assert (requirement/source req)))

(defmethod resolve-requirement-source! ((req project-requirement))
  (let ((source (project-source (find-project-in-sources (requirement/name req)))))
    (setf (requirement/source req) source))
  req)

(defmethod resolve-requirement-source! ((req vcs-project-requirement))
  (let ((source (project-source (find-project-in-sources (requirement/name req)))))
    (setf (requirement/source req) source))
  req)

(defmethod resolve-requirement-source! ((req system-requirement))
  (let ((source (system-source (find-system-in-sources (requirement/name req)))))
    (setf (requirement/source req) source))
  req)


;; * Requirement Resolution

(defgeneric resolve-requirement (req search-state)
  (:documentation "Given a requirement and a search-state, returns an alist
representing satisfying solutions of the requirement. The alist maps release
objects to a list of system-releases."))

(defmethod resolve-requirement ((req vcs-project-requirement) search-state)
  (declare (ignore search-state))
  (let* ((project-name (requirement/name req))
         (systems (requirement/systems req))
         (system-files (requirement/system-files req))
         (branch (requirement/branch req))
         (commit (requirement/commit req))
         (tag (requirement/tag req))
         (vcs-project (find-project-in-sources project-name))
         (vcs-release (project-release vcs-project
                                       (cond
                                         (commit
                                          `(:commit ,commit))
                                         (branch
                                          `(:branch ,branch))
                                         (tag
                                          `(:tag ,tag)))))
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
                    (mapcar (rcurry #'cons t)
                            (release-system-files vcs-release)))))))

(defmethod resolve-requirement ((req project-requirement) search-state)
  (let* ((project-name (requirement/name req))
         (version-spec (requirement/version-spec req))
         (project (find-project-in-sources project-name))
         (releases (project-releases project))
         (applicable-releases (remove-if-not (rcurry #'release-satisfies-version-spec-p version-spec)
                                             releases)))
    (mapcar (lambda (x)
              (list x :system-releases (release-system-releases x)))
            (sort applicable-releases #'release->))))

(defmethod resolve-requirement ((req system-requirement) search-state)
  (let ((system-name (requirement/name req))
        (version-spec (requirement/version-spec req))
        (sources (find-sources-for-req req)))
    (when sources
      (let* ((system (source-system (first sources) system-name))
             (system-releases (system-system-releases system))
             (applicable-system-releases (remove-if-not (rcurry #'system-release-satisfies-version-spec-p
                                                                version-spec)
                                                        system-releases)))
        (mapcar (lambda (x)
                  (list (system-release-release x)
                        :system-releases (list x)))
                (sort applicable-system-releases #'system-release->))))))

(defmethod resolve-requirement ((req fs-system-requirement) search-state)
  ;; Make a release from the file system.
  (let* ((system-name (requirement/name req))
         (fs-source (requirement/source req))
         (system (source-system fs-source system-name))
         (releases (system-releases system))
         (release (first releases))
         (system-release (release-system-release release system-name)))
    (assert (length= 1 releases))

    (list (list release
                :system-files
                (list (cons (system-release-system-file system-release) (list system-name)))))
    ;; (mapcar (lambda (release)
    ;;           (list release :system-files (list (cons (release-system-release)))(release-system-files release))
    ;;           ;;(list release :system-releases (list (release-system-release release system-name)))
    ;;           )
    ;;         releases)
    ))

;; (defmethod resolve-requirement ((req fs-system-file-requirement) sources)
;;   (let* ((system-pathname (requirement/name req))
;;          (fs-source (requirement/source req))
;;          (system-file (fs-source-register-asd fs-source system-pathname)))

;;     (list (cons (system-file-release system-file) (system-file-system-releases system-file)))))


;; * Installing VCS requirements

(defun ensure-vcs-req-installed-and-rewrite-req (req vcs-source)
  "Given a VCS requirement, install it and return a new requirement on the
correct commit."
  (let* ((project-name (requirement/name req))
         (branch (requirement/branch req))
         (commit (requirement/commit req))
         (tag (requirement/tag req))
         (source (requirement/source req))
         (repo (or (requirement/repo req)
                   (project-repo (source-project source project-name)))))
    (unless (typep source 'vcs-source)
      ;; We need to rehome this requirement so we get all the groveling
      ;; capabilities.
      (setf source vcs-source)
      (vcs-source-register-project! vcs-source repo project-name))
    (let* ((vcs-project (source-project source project-name))
           (vcs-release (project-release vcs-project
                                         (cond
                                           (commit
                                            `(:commit ,commit))
                                           (branch
                                            `(:branch ,branch))
                                           (tag
                                            `(:tag ,tag))))))
      (install-release vcs-release)
      (make-instance 'vcs-project-requirement
                     :commit (vcs-release/commit vcs-release)
                     :repo repo
                     :source source
                     :name project-name
                     :systems (requirement/systems req)
                     :system-files (requirement/system-files req)))))



;; * Resolve requirements

(defun perform-search (root-node)
  (let ((q (list root-node)))
    (iter
      (for node := (pop q))
      (unless node
        (error "unable to resolve requirements"))
      (when (search-done-p node)
        (return node))
      (for generator := (search-node-child-generator node))
      (for (values next-node-unclean requeue-p) := (funcall generator))
      (when requeue-p
        (push node q))
      (when next-node-unclean
        (for (values next-node valid-p) := (cleanup-search-node! next-node-unclean))
        (when valid-p
          (push next-node q))))))


(defun resolve-requirements (context &key update-p)
  "Given a context, return a new context that has the same requirements but the
set of installed releases is updated to be the minimum set that satisfies all
the requirements."
  (declare (ignore update-p))
  (let* ((*sources* (context-sources context))
         (reqs (context-requirements context))
         (out-context (copy-context context))
         (root-node (make-instance 'search-node
                                   :groveler (make-groveler)
                                   :state (make-instance 'search-state
                                                         :unresolved-reqs reqs)))
         (final-node (perform-search root-node)))
    (setf (context-releases out-context)
          (mapcar #'car (search-state-activated-releases (search-node-state final-node))))
    (setf (context-reverse-dependencies out-context)
          (sort (copy-alist (search-state-activated-releases (search-node-state final-node)))
                #'string< :key (compose #'project-name #'release-project #'car)))
    (setf (context-system-releases out-context) (search-state-activated-system-releases
                                                 (search-node-state final-node)))
    out-context))

(defun %resolve-requirements (reqs sources &key no-deps)
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
  (let* ((vcs-source (find-if (lambda (x) (typep x 'vcs-source)) sources))
         (*sources* (if vcs-source
                        sources
                        (progn
                          (setf vcs-source (make-instance 'vcs-source
                                                          :name "%resolve-vcs-source"))
                          (list* vcs-source sources)))))
    (mapc #'resolve-requirement-source! reqs)

    ;; Make sure all vcs releases are installed and replace their requirements
    ;; with a requirement on the commit.
    (let* ((reqs (loop
                   :for r :in reqs
                   :when (typep r 'vcs-requirement)
                     :collect (ensure-vcs-req-installed-and-rewrite-req r vcs-source)
                   :else
                     :collect r))
           (root-node (make-instance 'search-node
                                     :groveler (make-groveler)
                                     :state (make-instance 'search-state
                                                           :unresolved-reqs reqs))))
      (assert (not no-deps))
      ;; (when no-deps
      ;;   (mapcar #'system-release-release
      ;;           (search-node/activated-system-releases (next-child root-node))))
      (iter
        (with q := (list root-node))
        (for node := (pop q))
        (unless node
          (error "unable to resolve requirements"))
        (for generator := (search-node-child-generator node))
        (for (values next-node-unclean requeue-p) := (funcall generator))
        (when requeue-p
          (push node q))
        (when next-node-unclean
          (for (values next-node valid-p) := (cleanup-search-node! next-node-unclean))
          (when valid-p
            (push next-node q)
            (when (search-done-p next-node)
              (return (values (remove-duplicates
                               (search-state-activated-releases (search-node-state next-node)))
                              (search-state-activated-system-releases (search-node-state next-node)))))))))))
