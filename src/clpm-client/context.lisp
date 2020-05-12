;;;; CLPM Contexts
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)

(defvar *default-context* nil
  "If non-NIL, the default context to use for most operations. Can either be a
string (naming a global CLPM context) or a pathname to a clpmfile.")

(defvar *active-context* nil
  "Non-NIL if we have entered a context in this session.")

(defvar *active-context-ignore-inherited-source-registry* nil
  "Non-NIL if we are ignoring inherited source registries.")

(defvar *active-context-splice-source-registry* nil
  "If non-NIL, will be spliced into source registry if inherited config is not
ignored.")

(defun configure-from-env ()
  "Determine if there is any active context by looking at environment
variables."
  (let ((env-clpmfile (uiop:getenvp "CLPM_BUNDLE_CLPMFILE"))
        (env-context (uiop:getenvp "CLPM_EXEC_CONTEXT")))
    (cond
      (env-clpmfile
       ;; We're operating within a bundle context.
       (setf *active-context* (pathname env-clpmfile))
       (setf *active-context-ignore-inherited-source-registry* t))
      (env-context
       ;; We're operating within a global CLPM context.
       (setf *active-context* env-context)
       (setf *active-context-ignore-inherited-source-registry*
             (uiop:getenvp "CLPM_EXEC_IGNORE_INHERITED_SOURCE_REGISTRY"))
       (setf *active-context-splice-source-registry*
             (uiop:getenvp "CLPM_EXEC_SPLICE_INHERITED_SOURCE_REGISTRY"))))))
(uiop:register-image-restore-hook 'configure-from-env)

(defun clear-active-context ()
  (setf *active-context* nil
        *active-context-ignore-inherited-source-registry* nil
        *active-context-splice-source-registry* nil))
(uiop:register-image-dump-hook 'clear-active-context)

(register-clpm-cleanup-hook
 (lambda ()
   (setf uiop:*image-restore-hook* (remove 'configure-from-env uiop:*image-restore-hook*))
   (setf uiop:*image-dump-hook* (remove 'clear-active-context uiop:*image-dump-hook*))))

(defun active-context ()
  "Returns the currently active context. Is either a string naming a global CLPM
context or a pathname to a clpmfile. Returns NIL if there is no currently active
context."
  *active-context*)

(defun default-context ()
  "Returns the default context for operations. Defaults to *DEFAULT-CONTEXT*,
then ACTIVE-CONTEXT, then \"default\"."
  (or *default-context*
      *active-context*
      "default"))

(defun context-bundle-p (context)
  "A context names a bundle if it is a pathname."
  (pathnamep context))

(defun context-asd-pathnames (&optional (context (default-context)))
  "Given a context, return a list of pathnames to .asd files installed in that
context."
  (assert (not (context-bundle-p context)))
  (with-clpm-proc (proc)
    (clpm-proc-print
     proc
     `(context-asd-pathnames ,context))
    (clpm-proc-read proc)))

(defun context-asd-directories (&optional (context (default-context)))
  "Return the directories containing the .asd files installed in CONTEXT."
  (asd-pathnames-to-directories (context-asd-pathnames context)))

(defun asd-pathnames-to-directories (pathnames)
  "Given a list of pathnames to .ASD files, return a list of pathnames to the
directories containing the files."
  (remove-duplicates (mapcar 'uiop:pathname-directory-pathname pathnames)
                     :test 'uiop:pathname-equal))

(defun context-find-system-asd-pathname (system-name &optional (context (default-context)))
  "Find the pathname to a system in the given context."
  (assert (not (context-bundle-p context)))
  (with-clpm-proc (proc)
    (clpm-proc-print
     proc
     `(context-find-system-asd-pathname ,context ,system-name))
    (clpm-proc-read proc)))

(defun context-output-translations (&optional (context (default-context)))
  "Return an output-translations form for CONTEXT."
  (with-clpm-proc (proc)
    (clpm-proc-print
     proc
     (if (context-bundle-p context)
         `(with-bundle-default-pathname-defaults (,@(when (pathnamep context) (list context)))
            (with-bundle-local-config (,@(when (pathnamep context) (list context)))
              (bundle-output-translations ,(if (pathnamep context) context '(bundle-clpmfile-pathname)))))
         `(context-output-translations ,context)))
    (clpm-proc-read proc)))

(defun context-source-registry (&key (context (default-context)) ignore-inherited)
  "Return a source-registry form for the CONTEXT."
  (with-clpm-proc (proc)
    (clpm-proc-print
     proc
     (if (context-bundle-p context)
         `(with-bundle-default-pathname-defaults (,context)
            (with-bundle-local-config (,context)
              (bundle-source-registry ,context)))
         `(context-to-asdf-source-registry-form ,context
                                                :ignore-inherited ,ignore-inherited)))
    (clpm-proc-read proc)))

(defun activate-context (context &key activate-asdf-integration
                                   ignore-inherited-source-registry)
  "Activate a CLPM context. This clears ASDF's current configuration and
replaces it with configuration appropriate for CONTEXT.

CONTEXT can be either a string (naming a global CLPM context) or a pathname
pointing to a bundle's clpmfile.

If IGNORE-INHERITED-SOURCE-REGISTRY is non-NIL, then source registry
configuration from other sources is ignored. This argument is ignored if CONTEXT
is a bundle.

If ACTIVATE-ASDF-INTEGRATION is non-NIL, ACTIVATE-ASDF-INTEGRATION is also
called."
  (when (and (active-context) (not (equal (active-context) context)))
    (cerror "Continue" "You are attempting to activate a context when a context is already active."))
  (let ((source-registry (context-source-registry :context context
                                                  :ignore-inherited ignore-inherited-source-registry))
        (output-translations (context-output-translations context))
        (old-context (active-context)))
    (setf *active-context* context
          *active-context-ignore-inherited-source-registry* ignore-inherited-source-registry)
    (unless old-context
      (setf *active-context-splice-source-registry* asdf:*source-registry-parameter*))
    (asdf-configure-source-registry source-registry)
    (when output-translations
      (asdf:initialize-output-translations output-translations))
    (when activate-asdf-integration
      (activate-asdf-integration))
    t))
