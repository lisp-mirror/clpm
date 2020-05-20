;;;; ASDF Integration with CLPM client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)


;; * Variables

(defvar *asdf-system-not-found-behavior* :error
  "The behavior if ASDF is unable to find a system and CLPM's ASDF integration
is active. One of:

+ :ERROR :: (default) Signal a MISSING-SYSTEM condition. The restarts
INSTALL-AND-RELOAD-CONFIG, RERESOLVE-REQUIREMENTS-AND-RELOAD-CONFIG,
INSTALL-WITHOUT-DEPENDENCIES-AND-RELOAD-CONFIG, and RELOAD-CONFIG will be
available.

+ :INSTALL :: Install the system and its dependencies without prompting. If the
active context is a bundle, the system is currently not added to the bundle (you
will need to either add it to the clpmfile or as a dependency to some other
included system for this to have any effect).

+ :INSTALL-WITHOUT-DEPS :: Install the system without its dependencies without
prompting. If the active context is a bundle, the system is currently not added
to the bundle (you will need to either add it to the clpmfile or as a dependency
to some other included system for this to have any effect).

+ NIL :: Do nothing.")


;; * Conditions and restarts

(define-condition missing-system (error)
  ((name
    :initarg :name
    :reader missing-system-name))
  (:report (lambda (c s)
             (format s "System ~A is not installed"
                     (missing-system-name c))))
  (:documentation
   "A condition signaled when CLPM thinks a system is not installed."))

(defun reload-config (&optional condition)
  "Invoke the RELOAD-CONFIG restart or return NIL if the restart does not
exist."
  (let ((restart (find-restart 'reload-config condition)))
    (when restart
      (invoke-restart restart))))

(defun install-and-reload-config (&optional condition)
  "Invoke the INSTALL-AND-RELOAD-CONFIG restart or return NIL if the restart
does not exist."
  (let ((restart (find-restart 'install-and-reload-config condition)))
    (when restart
      (invoke-restart restart))))

(defun install-without-dependencies-and-reload-config (&optional condition)
  "Invoke the INSTALL-WITHOUT-DEPENDENCIES-AND-RELOAD-CONFIG restart or return
NIL if the restart does not exist."
  (let ((restart (find-restart 'install-without-dependencies-and-reload-config condition)))
    (when restart
      (invoke-restart restart))))

(defun reresolve-requirements-and-reload-config (&optional condition)
  "Invoke the RERESOLVE-REQUIREMENTS-AND-RELOAD-CONFIG restart or return NIL if
the restart does not exist."
  (let ((restart (find-restart 'reresolve-requirements-and-reload-config condition)))
    (when restart
      (invoke-restart restart))))

(defun signal-missing-system (system-name)
  "Raise a MISSING-SYSTEM error for SYSTEM-NAME."
  (error 'missing-system :name system-name))


;; * ASDF Search Functions

(defun find-system-without-clpm (system-name)
  "Call ASDF:SEARCH-FOR-SYSTEM-DEFINITION in a dynamic environment with
*ASDF-SYSTEM-NOT-FOUND-BEHAVIOR* bound to NIL. Used to prevent infinite
recursion."
  (let ((*asdf-system-not-found-behavior* nil))
    (asdf:search-for-system-definition system-name)))

(defun asdf-configure-source-registry (source-registry
                                       &optional
                                         (splice (and (not *active-context-ignore-inherited-source-registry*)
                                                      *active-context-splice-source-registry*)))
  (when splice
    (let* ((inherit-configuration-position (position :inherit-configuration source-registry)))
      (when inherit-configuration-position
        (setf source-registry (append (butlast source-registry (- (length source-registry)
                                                                  inherit-configuration-position))
                                      (rest splice)
                                      (nthcdr (1+ inherit-configuration-position) source-registry))))))
  (asdf:clear-source-registry)
  (asdf:initialize-source-registry source-registry))

(defun handle-missing-system (system-name active-context)
  (labels ((%do-install (systems no-deps)
             (let ((new-source-registry
                     (install :systems systems
                              :no-deps no-deps
                              :update-asdf-config t)))
               (when new-source-registry
                 (find-system-without-clpm system-name))))
           (%reresolve ()
             (%do-install nil nil))
           (%install (no-deps)
             (%do-install (unless (context-bundle-p active-context) (list system-name)) no-deps)))
    (ecase *asdf-system-not-found-behavior*
      (:install (%install nil))
      (:install-without-deps (%install t))
      (:error
       (restart-case
           ;; Prevent RESTART-CASE from using WITH-CONDITION-RESTARTS,
           ;; otherwise things can get messed up for defsystem-depends-on
           (signal-missing-system system-name)
         (reresolve-requirements-and-reload-config ()
           :report "Reresolve requirements and try again."
           (%reresolve))
         (reload-config ()
           :report "Reload the source registry for the context and try again."
           (asdf-configure-source-registry (context-source-registry :context active-context))
           (unless (context-bundle-p active-context)
             (setf *active-context-installed-systems* (context-installed-system-names :context active-context)
                   *active-context-visible-primary-system-names* (context-visible-primary-system-names :context active-context)))
           (asdf:search-for-system-definition system-name))
         (install-and-reload-config ()
           :report "Attempt to install the system and try again."
           (%install nil))
         (install-without-dependencies-and-reload-config ()
           :report "Attempt to install the system without dependencies and try again."
           (%install t))))
      ((nil)
       nil))))

(defun clpm-system-definition-pre-search (system-name)
  "When used in conjunction with CLPM-SYSTEM-DEFINITION-SEARCH, this creates a
poor man's :around method for locating systems. This function checks to see if
it's likely that one of ASDF's built-in search functions would find a system
that's not installed, but is visible in the the source registry for the active
context.

This happens because the smallest granularity availble in ASDF's source registry
is the directory. So, for instance, someone may install CFFI (defined in
cffi.asd). Then, some time later, they decide they want to run CFFI's tests,
defined by the system cffi-tests in cffi-tests.asd *in the same directory as
cffi.asd*. The cffi-tests system will be found by ASDF, but its dependencies,
such as rt, are not. If this function didn't exist, then
CLPM-SYSTEM-DEFINITION-SEARCH would prompt the user to install rt and, if the
user accepts, it will be recorded in the context that rt is a toplevel
requirement. However, this is incorrect as cffi-tests should be the new toplevel
requirement."
  (let ((primary-name (asdf:primary-system-name system-name))
        (active-context (active-context)))
    (when (and active-context
               (not (equal "asdf" primary-name))
               (not (equal "uiop" primary-name))
               (not (member system-name *active-context-installed-systems* :test #'equal))
               (not (member primary-name *active-context-editable-primary-system-names* :test #'equal))
               (member primary-name *active-context-visible-primary-system-names* :test #'equal))
      (handle-missing-system system-name active-context))))

(defun clpm-system-definition-search (system-name)
  "A search function for ASDF's *SYSTEM-DEFINITION-SEARCH-FUNCTIONS* list.
Does nothing if there is no active context.

As this only works with an active context, we can assume that if control gets
here, ASDF was unable to find the system (as activating a context involves
configuring the source registry to include every system). That means we don't
even have to search for the system as all config resides in the source registry.

See *ASDF-SYSTEM-NOT-FOUND-BEHAVIOR* for details on how this function can be
configured to behave."
  (let ((primary-name (asdf:primary-system-name system-name))
        (active-context (active-context)))
    (when (and active-context
               ;; Don't handle ASDF or UIOP for the moment. Primary reason is I
               ;; don't want to deal with ASDF's self upgrades and refusal to
               ;; downgrade yet.
               (not (equal "asdf" primary-name))
               (not (equal "uiop" primary-name)))
      (handle-missing-system system-name active-context))))

(defun asdf-integration-active-p ()
  "Returns non-NIL iff 'CLPM-SYSTEM-DEFINITION-SEARCH is registered with ASDF's
search functions."
  (and
   (member 'clpm-system-definition-pre-search asdf:*system-definition-search-functions*)
   (member 'clpm-system-definition-search asdf:*system-definition-search-functions*)))

(defun activate-asdf-integration ()
  "Add 'CLPM-SYSTEM-DEFINITION-PRE-SEARCH and 'CLPM-SYSTEM-DEFINITION-SEARCH to
ASDF's system definition search functions list.

Does nothing if ASDF-INTEGRATION-ACTIVE-P returns T."
  (unless (asdf-integration-active-p)
    (setf asdf:*system-definition-search-functions*
          (list* 'clpm-system-definition-pre-search
                 (append asdf:*system-definition-search-functions*
                         (list 'clpm-system-definition-search))))))

(defun deactivate-asdf-integration ()
  "Removes CLPM-SYSTEM-DEFINITION-SEARCH from ASDF's search functions."
  (setf asdf:*system-definition-search-functions*
        (remove 'clpm-system-definition-search
                (remove 'clpm-system-definition-pre-search
                        asdf:*system-definition-search-functions*))))

;; Unregister CLPM from ASDF on CLPM cleanup.
(register-clpm-cleanup-hook 'deactivate-asdf-integration)
