;;;; Requirement resolution search
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/resolve/search
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/groveler
          #:clpm/install/defs
          #:clpm/log
          #:clpm/requirement
          #:clpm/resolve/node
          #:clpm/resolve/requirement
          #:clpm/source
          #:iterate)
  (:export #:perform-search))

(in-package #:clpm/resolve/search)

(setup-logger)

(defun make-terminal-generator ()
  "Make a node generator representing a terminal node of the tree."
  (constantly nil))

(defun handle-groveler-missing-dependency (parent-node nominal-node c)
  (let* ((missing-system-spec (groveler-dependency-missing/system c))
         (missing-req (convert-asd-system-spec-to-req missing-system-spec :why :grovel)))
    (multiple-value-bind (status satisfying-system-release)
        (requirement-state missing-req nominal-node)
      (ecase status
        (:sat
         (unless (release-installed-p (system-release-release satisfying-system-release))
           (install-release (system-release-release satisfying-system-release)))
         (log:debug "Groveler is missing ~S, but it is already in resolution. Adding it."
                    missing-system-spec)
         (invoke-restart 'add-asd-and-retry
                         (system-release-absolute-asd-pathname satisfying-system-release)
                         (lambda ()
                           (log:debug "Groveler successfully added ~S. Adding ~S to search state."
                                      missing-system-spec
                                      (namestring (system-release-absolute-asd-pathname satisfying-system-release)))
                           (node-register-asd-loaded-in-groveler!
                            nominal-node
                            (system-release-absolute-asd-pathname satisfying-system-release))))
         nil)
        (:unsat
         (make-terminal-generator))
        (:unknown
         ;; If the missing requirement is a system requirement that looks like
         ;; it'd be provided by another system file on our list of files to
         ;; grovel, reorder the list so that it's first. Otherwise, add the
         ;; requirement to the unresolved grovel-reqs list.
         (let ((new-search-node (copy-node parent-node)))
           (log:trace "When groveling, found ~A is unsatisfied" missing-req)
           (if-let ((candidate-file
                     (and (typep missing-req 'system-requirement)
                          (find (asdf:primary-system-name (requirement/name missing-req))
                                (node-system-files-pending-groveling new-search-node)
                                :key (compose #'system-file-primary-system-name #'car)
                                :test #'equal))))
             (progn
               (log:trace "Candidate file: ~S" candidate-file)
               (lambda ()
                 (setf (node-system-files-pending-groveling new-search-node)
                       (list* candidate-file (remove candidate-file
                                                     (node-system-files-pending-groveling new-search-node))))
                 (values new-search-node nil)))
             (progn
               (log:trace "No candidate file")
               (lambda ()
                 (node-add-unresolved-grovel-req! new-search-node missing-req)
                 (values new-search-node nil))))))))))

(defun compute-child-generator/grovel (node)
  (let ((new-search-node (copy-node node
                                    :system-files-pending-groveling (rest (node-system-files-pending-groveling node)))))
    (destructuring-bind (sf . system-names)
        (first (node-system-files-pending-groveling node))
      (handler-bind
          ((groveler-dependency-missing
             (lambda (c)
               (log:trace "HERE")
               (let ((potential-generator (handle-groveler-missing-dependency node new-search-node c)))
                 (when potential-generator
                   (log:trace "Reordering list")
                   (return-from compute-child-generator/grovel potential-generator))))))
        (node-load-asd-in-groveler! new-search-node (system-file-absolute-asd-pathname sf))

        ;; Groveller is primed. Get the requirements from it.
        (let ((*active-groveler* (node-groveler new-search-node))
              (system-releases nil))
          (if (eql t system-names)
              (setf system-releases (system-file-system-releases sf))
              (setf system-releases (mapcar (lambda (sn)
                                              (release-system-release (system-file-release sf) sn))
                                            system-names)))

          (dolist (sr system-releases)
            (let ((reqs (system-release-requirements sr)))
              ;; Mark the reason for the new reqs
              (mapc (lambda (r)
                      (setf (requirement/why r) sr))
                    reqs)
              ;; Add the deps we just computed.
              (node-add-unresolved-reqs! new-search-node reqs)))
          ;; activate the system releases
          ;;
          ;; TODO: Put actual reason here. Should probably be captured from
          ;; whatever added this to the queue.
          (node-add-resolution! new-search-node (system-file-release sf) system-releases nil t)
          (lambda ()
            (values new-search-node nil)))))))

(defun make-unresolved-req-generator (resolutions unresolved-req parent-node &optional grovel-req-p)
  (let ((new-unresolved-reqs (if grovel-req-p
                                 (node-unresolved-reqs parent-node)
                                 (remove unresolved-req (node-unresolved-reqs parent-node))))
        (new-unresolved-grovel-reqs (if grovel-req-p
                                        (remove unresolved-req (node-unresolved-grovel-reqs parent-node))
                                        (node-unresolved-grovel-reqs parent-node))))
    (lambda ()
      (destructuring-bind (release &key system-releases) (pop resolutions)
        (let* ((new-node (copy-node parent-node
                                    :unresolved-reqs new-unresolved-reqs
                                    :unresolved-grovel-reqs new-unresolved-grovel-reqs)))
          (log:debug "Resolving ~A with release ~A" unresolved-req release)
          ;; Add the chosen resolution.
          (node-add-resolution! new-node release system-releases nil unresolved-req)
          ;; And push its requirements, unless the requirement says to not
          ;; include deps.
          (unless (requirement/no-deps-p unresolved-req)
            (dolist (sr system-releases)
              (let ((new-reqs (system-release-requirements sr)))
                ;; Mark the reason for the new reqs
                (mapc (lambda (r)
                        (setf (requirement/why r) sr))
                      new-reqs)
                (node-add-unresolved-reqs! new-node new-reqs))))
          (values new-node (not (null resolutions))))))))

(defun compute-child-generator/unresolved-grovel-req (node)
  "Compute a function that returns search nodes, descending from NODE that
resolve a single, unresolved requirement. Same as /unresolve-req, but just pulls
from a different list."
  ;; We need to resolve some requirements before we can grovel further.
  (let* ((unresolved-req (first (node-unresolved-grovel-reqs node)))
         (resolutions (resolve-requirement unresolved-req node)))
    (log:trace "Making child generator to resolve grovel req ~A with resolutions ~A"
               unresolved-req resolutions)
    (if resolutions
        (make-unresolved-req-generator resolutions unresolved-req node t)
        (make-terminal-generator))))

(defun compute-child-generator/unresolved-reqs (node)
  "Compute a function that returns search nodes, descending from NODE that
resolve a single, unresolved requirement."
  ;; Choose to reoslve the first unresolved requirement from the list.
  (let* ((unresolved-req (first (node-unresolved-reqs node)))
         (resolutions (resolve-requirement unresolved-req node)))
    (log:trace "Making child generator to resolve ~A with resolutions ~A"
               unresolved-req resolutions)
    (if resolutions
        (make-unresolved-req-generator resolutions unresolved-req node)
        ;; There are no resolutions...
        (make-terminal-generator))))

(defun compute-child-generator/unresolved-vcs-or-fs-req (node unresolved-vcs-or-fs-req)
  ;; We will generate one child. That child will resolve this req, without
  ;; adding its dependencies yet, as they need to be groveled.
  (let* ((new-search-node (copy-node node
                                     :unresolved-reqs (remove unresolved-vcs-or-fs-req
                                                              (node-unresolved-reqs node))))
         (resolutions (resolve-requirement unresolved-vcs-or-fs-req node)))
    (log:trace "Making child generator to resolve ~A with resolutions ~A"
               unresolved-vcs-or-fs-req resolutions)
    ;; If there's not exactly one resolution available, something is horribly
    ;; wrong.
    (assert (length= 1 resolutions))
    (destructuring-bind ((release &key system-files)) resolutions
      (node-add-resolution! new-search-node release nil system-files unresolved-vcs-or-fs-req))
    (lambda ()
      (values new-search-node nil))))

(defun compute-child-generator (node)
  (let* ((unresolved-vcs-or-fs-req (find-if (lambda (x)
                                              (or (typep x 'vcs-requirement)
                                                  (typep x 'fs-system-requirement)
                                                  (typep x 'fs-system-file-requirement)))
                                            (node-unresolved-reqs node))))
    (cond
      (unresolved-vcs-or-fs-req
       (log:trace "Have an unresolved vcs or fs req")
       (compute-child-generator/unresolved-vcs-or-fs-req node unresolved-vcs-or-fs-req))
      ((node-unresolved-grovel-reqs node)
       (log:trace "Have an unresolved grovel requirement")
       (compute-child-generator/unresolved-grovel-req node))
      ((node-system-files-pending-groveling node)
       (log:trace "Have a system file to grovel")
       (compute-child-generator/grovel node))
      ((node-unresolved-reqs node)
       (log:trace "Have an unresolved requirement")
       (compute-child-generator/unresolved-reqs node))
      (t
       (error "Should not get here")))))

(defmethod slot-unbound (class (self node) (slot-name (eql 'node-child-generator)))
  (setf (node-child-generator self) (compute-child-generator self)))

(defun search-done-p (node)
  "Returns T iff the search is complete at NODE."
  (and (null (node-unresolved-reqs  node))
       (null (node-unresolved-grovel-reqs node))
       (null (node-system-files-pending-groveling node))))

(defun dead-system-requirement-p (req node)
  "Used to remove requirements that seem to be broken. That is, the system
cannot be found and the primary system is already activated. This has been
observed in package-inferred-systems that are groveled after a refactoring
leaves some dead files around."
  (and (typep req 'system-requirement)
       (null (find-requirement-source req nil))
       (node-find-system-if-active node (asdf:primary-system-name (requirement/name req)))))

(defun cleanup-search-node! (node)
  "Returns two values. The first is the search node with all satisfied
requirements removed. The second is T iff no requirements were violated, NIL
otherwise."
  (setf (node-unresolved-reqs node)
        (iter
          (for r :in (node-unresolved-reqs node))
          (when (and (not (node-system-files-pending-groveling node))
                     (dead-system-requirement-p r node))
            (log:debug "Killing requirement ~A as it seems incorrect" r)
            (next-iteration))
          (for (values state satisfying-system-release) := (requirement-state r node))
          (ecase state
            (:unknown
             (collect r))
            (:sat
             (awhen satisfying-system-release
               (node-add-resolution! node (system-release-release it) nil nil r)))
            (:unsat
             (return-from cleanup-search-node!
               (values node nil))))))
  (values node t))

(defun perform-search (root-node)
  (let ((q (list root-node)))
    (iter
      (for node := (pop q))
      (unless node
        (error "unable to resolve requirements"))
      (when (search-done-p node)
        (return node))
      (for generator := (node-child-generator node))
      (for (values next-node-unclean requeue-p) := (funcall generator))
      (when requeue-p
        (push node q))
      (when next-node-unclean
        (for (values next-node valid-p) := (cleanup-search-node! next-node-unclean))
        (when valid-p
          (push next-node q))))))
