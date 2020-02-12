;;;; Requirement resolution
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

;; * package definition

(uiop:define-package #:clpm/resolve
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/context
          #:clpm/groveler
          #:clpm/log
          #:clpm/requirement
          #:clpm/resolve/defs
          #:clpm/resolve/node
          #:clpm/resolve/requirement
          #:clpm/resolve/search
          #:clpm/source)
  (:export #:resolve-requirements))

(in-package #:clpm/resolve)

(setup-logger)

;; * Explanation
;;
;;   The primary task performed by this package is going from requirements to a
;;   minimal set of releases that resolve all the requirements. The general
;;   approach taken is to perform a search over the space of all releases until
;;   a satisfying combination is found. However, the space of all release
;;   combinations is enormous, so we need to guide it somehow.
;;
;;   This is probably the most complicated part of CLPM. It could likely still
;;   benefit from improvements, but this implementation should be correct. The
;;   non-trivial aspect of this is, of course, grovelling for dependencies from
;;   ASD files. In this case, we have little to no metadata to work from and
;;   must instead rely on locally examining ASD files to extract the information
;;   needed. However, pulling information from these files could result in
;;   running arbitrary Lisp code. Additionally, if any ASD system is a
;;   package-inferred-system, then dependency information will be scattered
;;   throughout the release files *and* there is no guarantee that all the
;;   subsystems will be reachable from the primary system... This is where the
;;   groveller comes in. However, the groveller must be properly managed, and
;;   new grovellers may need to be instantiated as we backtrack while performing
;;   the search.
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

(defun make-result-sorter (context update-projects)
  (lambda (release-alist)
    (let ((existing-release (find-if (lambda (x)
                                       (member x (context-releases context)))
                                     release-alist
                                     :key #'car)))
      (if (and existing-release
               (not (member (project-name (release-project (car existing-release)))
                            update-projects :test #'equal)))
          (list* existing-release (remove existing-release release-alist))
          release-alist))))

(defun rewrite-vcs-req (req context)
  "Given a VCS requirement and a context, "
  (let ((existing-release (find (requirement/name req) (context-releases context)
                                :test #'equal :key (compose #'project-name #'release-project))))
    (if (and existing-release (listp (release-version existing-release)))
        (progn
          (make-instance 'vcs-project-requirement
                         :name (requirement/name req)
                         :source (requirement/source req)
                         :why (requirement/why req)
                         :no-deps-p (requirement/no-deps-p req)
                         :commit (second (release-version existing-release))))
        req)))

(defun rewrite-vcs-reqs (reqs context update-projects)
  (mapcar (lambda (req)
            (if (and (not (eql update-projects t))
                     (typep req 'vcs-requirement)
                     (not (member (requirement/name req) update-projects :test #'equal)))
                (rewrite-vcs-req req context)
                req))
          reqs))

(defun resolve-requirements (context &key update-projects)
  "Given a context, return a new context that has the same requirements but the
set of installed releases is updated to be the minimum set that satisfies all
the requirements."
  (let* ((*sources* (context-sources context))
         (*releases-sort-function* (unless (eql update-projects t)
                                     (make-result-sorter context update-projects)))
         (reqs (rewrite-vcs-reqs (context-requirements context) context update-projects))
         (out-context (copy-context context))
         (root-node (make-instance 'node
                                   :unresolved-reqs reqs
                                   :groveler (make-groveler)))
         (final-node (perform-search root-node)))
    (setf (context-requirements out-context) reqs)
    (setf (context-releases out-context)
          (mapcar #'car (node-activated-releases final-node)))
    (setf (context-reverse-dependencies out-context)
          (sort (copy-alist (node-activated-releases final-node))
                #'string< :key (compose #'project-name #'release-project #'car)))
    (setf (context-system-releases out-context) (node-activated-system-releases final-node))
    out-context))
