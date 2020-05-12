;;;; Interacting with bundles
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)

(defun bundle-init (clpmfile &key asds)
  "Create a new clpmfile located at CLPMFILE. Adds all ASDS (must be relative to
CLPMFILE) to the new clpmfile."
  (setf clpmfile (uiop:ensure-absolute-pathname clpmfile))
  (with-clpm-proc (proc)
    (clpm-proc-print
     proc
     `(with-bundle-default-pathname-defaults (,clpmfile)
        (with-bundle-local-config (,clpmfile)
          (bundle-init ,clpmfile :asds ',asds)))))
  clpmfile)

(defun bundle-install (&key clpmfile no-resolve (validate 'context-diff-approved-p))
  "Ensure a bundle is installed. Returns a source registry form if the install completed.

CLPMFILE must be a pathname pointing a clpmfile.

If NO-RESOLVE is non-NIL, then the bundle will be installed completely from its
lock file, without reresolving any requirements.

VALIDATE is a function that takes a CONTEXT-DIFF instance and returns non-NIL if
the diff is approved and the install can continue. If NIL, the install is
aborted."
  (let (diff-description)
    (with-clpm-proc (proc)
      (clpm-proc-print
       proc
       `(with-bundle-default-pathname-defaults (,clpmfile)
          (with-bundle-local-config (,clpmfile)
            (bundle-source-registry
             (bundle-install ,clpmfile
                             :validate ,(make-diff-validator-fun)
                             :no-resolve ,no-resolve)))))
      (setf diff-description (clpm-proc-read proc))
      (let ((validate-result (funcall validate (make-context-diff-from-description diff-description)))
            source-registry)
        (clpm-proc-print proc validate-result)
        (setf source-registry (clpm-proc-read proc))
        (when validate-result
          source-registry)))))

(defun bundle-update (&key projects systems clpmfile (validate 'context-diff-approved-p))
  "Update a bundle.

PROJECTS is a list of projects to update. SYSTEMS is a list of systems to
update. If both are NIL, all projects are available for updating.

CLPMFILE must be a pathname pointing a clpmfile.

VALIDATE is a function that takes a CONTEXT-DIFF instance and returns non-NIL if
the diff is approved and the install can continue. If NIL, the install is
aborted."
  (let (diff-description)
    (with-clpm-proc (proc)
      (clpm-proc-print
       proc
       `(with-bundle-default-pathname-defaults (,clpmfile)
          (with-bundle-local-config (,clpmfile)
            (bundle-update ,clpmfile
                           :update-systems ',systems
                           :update-projects ',projects
                           :validate ,(make-diff-validator-fun)))))
      (setf diff-description (clpm-proc-read proc))
      (let ((validate-result (funcall validate (make-context-diff-from-description diff-description))))
        (clpm-proc-print proc validate-result)
        (clpm-proc-read proc)
        validate-result))))
