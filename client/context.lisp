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

(defvar *active-context-editable-primary-system-names* nil
  "List of systems installed in the active context via an editable requirement.")

(defvar *active-context-installed-primary-system-names* nil
  "List of systems installed in the active context.")

(defvar *active-context-visible-primary-system-names* nil
  "List of primary system names visible to ASDF in the active context.")

(defun configure-from-env ()
  "Determine if there is any active context by looking at environment
variables."
  (let ((env-clpmfile (uiop:getenvp "CLPM_EXEC_CLPMFILE"))
        (env-context (uiop:getenvp "CLPM_EXEC_CONTEXT")))
    (when (or env-clpmfile env-context)
      (setf *active-context* (if env-clpmfile (pathname env-clpmfile) env-context))
      (uiop:if-let ((installed-systems-string (uiop:getenvp "CLPM_EXEC_INSTALLED_PRIMARY_SYSTEMS")))
        (uiop:with-safe-io-syntax ()
          (setf *active-context-installed-primary-system-names* (read-from-string installed-systems-string))))
      (uiop:if-let ((visible-systems-string (uiop:getenvp "CLPM_EXEC_VISIBLE_PRIMARY_SYSTEMS")))
        (uiop:with-safe-io-syntax ()
          (setf *active-context-visible-primary-system-names* (read-from-string visible-systems-string))))
      (uiop:if-let ((editable-systems-string (uiop:getenvp "CLPM_EXEC_EDITABLE_PRIMARY_SYSTEMS")))
        (uiop:with-safe-io-syntax ()
          (setf *active-context-editable-primary-system-names* (read-from-string editable-systems-string))))
      (if (uiop:getenvp "CLPM_EXEC_IGNORE_INHERITED_SOURCE_REGISTRY")
          (progn
            (setf *active-context-ignore-inherited-source-registry* t)
            (setf *active-context-splice-source-registry* nil))
          (progn
            (setf *active-context-ignore-inherited-source-registry* nil)
            (setf *active-context-splice-source-registry* (uiop:getenvp "CLPM_EXEC_SPLICE_INHERITED_SOURCE_REGISTRY")))))))
(uiop:register-image-restore-hook 'configure-from-env)

(defun clear-active-context ()
  (setf *active-context* nil
        *active-context-ignore-inherited-source-registry* nil
        *active-context-splice-source-registry* nil
        *active-context-visible-primary-system-names* nil
        *active-context-installed-primary-system-names* nil
        *active-context-visible-primary-system-names* nil))
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
      (active-context)
      "default"))

(defun context-bundle-p (context)
  "A context names a bundle if it is a pathname."
  (pathnamep context))

(defun context-asd-pathnames (&key (context (default-context)))
  "Given a context, return a list of pathnames to .asd files installed in that
context."
  (with-clpm-proc (proc)
    (clpm-proc-print proc `(asd-pathnames :context ,context))
    (clpm-proc-read proc)))

(defun asd-pathnames-to-directories (pathnames)
  "Given a list of pathnames to .ASD files, return a list of pathnames to the
directories containing the files."
  (remove-duplicates (mapcar 'uiop:pathname-directory-pathname pathnames)
                     :test 'uiop:pathname-equal))

(defun context-asd-directories (&key (context (default-context)))
  "Return the directories containing the .asd files installed in CONTEXT."
  (asd-pathnames-to-directories (context-asd-pathnames :context context)))

(defun context-editable-primary-system-names (&key (context (default-context)))
  "Return the names of primary systems editable in CONTEXT."
  (with-clpm-proc (proc)
    (clpm-proc-print proc `(editable-primary-system-names :context ,context))
    (clpm-proc-read proc)))

(defun context-find-system-asd-pathname (system-name &key (context (default-context)))
  "Find the pathname to a system in the given context."
  (with-clpm-proc (proc)
    (clpm-proc-print proc `(find-system-asd-pathname ,system-name :context ,context))
    (clpm-proc-read proc)))

(defun context-installed-system-names (&key (context (default-context)))
  "Return the names of systems installed in CONTEXT."
  (with-clpm-proc (proc)
    (clpm-proc-print proc `(installed-system-names :context ,context))
    (clpm-proc-read proc)))

(defun context-installed-primary-system-names (&key (context (default-context)))
  "Return the names of primary systems installed in CONTEXT."
  (with-clpm-proc (proc)
    (clpm-proc-print proc `(installed-primary-system-names :context ,context))
    (clpm-proc-read proc)))

(defun context-visible-primary-system-names (&key (context (default-context)))
  "Return the names of primary systems visible to ASDF in CONTEXT."
  (with-clpm-proc (proc)
    (clpm-proc-print proc `(visible-primary-system-names :context ,context))
    (clpm-proc-read proc)))

(defun context-output-translations (&key (context (default-context)))
  "Return an output-translations form for CONTEXT."
  (with-clpm-proc (proc)
    (clpm-proc-print proc `(output-translations :context ,context))
    (clpm-proc-read proc)))

(defun context-source-registry (&key (context (default-context))
                                  (ignore-inherited (context-bundle-p context)))
  "Return a source-registry form for the CONTEXT."
  (with-clpm-proc (proc)
    (clpm-proc-print proc `(source-registry :context ,context :ignore-inherited-source-registry ,ignore-inherited :with-client-p t))
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
        (output-translations (context-output-translations :context context))
        (old-context (active-context)))
    (setf *active-context* context
          *active-context-ignore-inherited-source-registry* ignore-inherited-source-registry
          *active-context-installed-primary-system-names* nil
          *active-context-visible-primary-system-names* nil
          *active-context-editable-primary-system-names* nil)
    (unless old-context
      (setf *active-context-splice-source-registry* asdf:*source-registry-parameter*))
    (asdf-configure-source-registry source-registry)
    (unless (context-bundle-p context)
      (setf *active-context-editable-primary-system-names* (context-editable-primary-system-names :context context)
            *active-context-installed-primary-system-names* (context-installed-primary-system-names :context context)
            *active-context-visible-primary-system-names* (context-visible-primary-system-names :context context)))
    (when output-translations
      (asdf:initialize-output-translations output-translations))
    (when activate-asdf-integration
      (activate-asdf-integration))
    t))
