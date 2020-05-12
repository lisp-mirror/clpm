;;;; ASDF Integration with CLPM client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)


;; * Variables

(defvar *asdf-system-not-found-behavior* :error
  "The behavior if ASDF asks CLPM to find a system and CLPM is unable to find
it. One of:

+ :ERROR :: Signal a MISSING-SYSTEM condition. If the current context is a
bundle, the INSTALL-AND-RELOAD-BUNDLE and RELOAD-BUNDLE restarts will be
available. If the current context is not a bundle, the INSTALL-SYSTEM and
INSTALL-SYSTEM-WITHOUT-DEPENDENCIES restarts will be available.

+ :INSTALL :: Install the system and its dependencies without prompting.

+ :INSTALL-WITHOUT-DEPS :: Install the system without its dependencies without
prompting.

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

(defun install-and-reload-bundle (&optional condition)
  "Invoke the INSTALL-AND-RELOAD-BUNDLE restart or return NIL if the restart
does not exist."
  (let ((restart (find-restart 'install-and-reload-bundle condition)))
    (when restart
      (invoke-restart restart))))

(defun install-system (&optional condition)
  "Invoke the INSTALL-SYSTEM restart or return NIL if the restart does not
exist."
  (let ((restart (find-restart 'install-system condition)))
    (when restart
      (invoke-restart restart))))

(defun install-system-without-dependencies (&optional condition)
  "Invoke the INSTALL-SYSTEM-WITHOUT-DEPENDENCIES restart or return NIL if the
restart does not exist."
  (let ((restart (find-restart 'install-system-without-dependencies condition)))
    (when restart
      (invoke-restart restart))))

(defun reload-bundle (&optional condition)
  "Invoke the RELOAD-BUNDLE restart or return NIL if the restart does not
exist."
  (let ((restart (find-restart 'reload-bundle condition)))
    (when restart
      (invoke-restart restart))))

(defun signal-missing-system (system-name)
  "Raise a MISSING-SYSTEM error for SYSTEM-NAME."
  (error 'missing-system :name system-name))


;; * ASDF Search Functions

(defun find-system-without-clpm (system-name)
  "Call ASDF:FIND-SYSTEM in a dynamic environment with
*ASDF-SYSTEM-NOT-FOUND-BEHAVIOR* bound to NIL. Used to prevent infinite
recursion."
  (let ((*asdf-system-not-found-behavior* nil))
    (asdf:find-system system-name)))

(defun should-auto-reload-source-registry-p ()
  (or (and *active-context*
           (equal *active-context* (context)))
      (and (inside-bundle-exec-p)
           (eql (context) :bundle))
      (and (clpm-exec-context)
           (equal (context) (clpm-exec-context)))))

(defun clpm-system-definition-search (system-name)
  "A search function for ASDF's *SYSTEM-DEFINITION-SEARCH-FUNCTIONS* list.

Given a system name, it tries to find it. If the system cannot be found, it
either does nothing, signals a condition, and/or installs the system (see:
*ASDF-SYSTEM-NOT-FOUND-BEHAVIOR*)."
  (let ((primary-name (asdf:primary-system-name system-name)))
    ;; Don't handle ASDF or UIOP for the moment...
    (unless (or (equal "asdf" primary-name)
                (equal "uiop" primary-name))
      (flet ((lookup-system ()
               (context-find-system-asd-pathname system-name))
             (reload-config (&optional source-registry)
               (asdf:clear-source-registry)
               (asdf:initialize-source-registry (or source-registry (context-source-registry)))))
        (or
         (unless (inside-bundle-exec-p)
           ;; The ASDF config is exhaustive when inside a bundle. No need for us
           ;; to look it up.
           (lookup-system))
         (ecase *asdf-system-not-found-behavior*
           (:install
            (let ((new-source-registry (install :systems (unless (inside-bundle-exec-p) (list system-name)))))
              (when new-source-registry
                (if (should-auto-reload-source-registry-p)
                    (progn
                      (reload-config new-source-registry)
                      (find-system-without-clpm system-name))
                    (lookup-system)))))
           (:install-without-deps
            (let ((new-source-registry (install :systems (unless (inside-bundle-exec-p) (list system-name))
                                                :no-deps (not (inside-bundle-exec-p)))))
              (when new-source-registry
                (if (should-auto-reload-source-registry-p)
                    (progn
                      (reload-config new-source-registry)
                      (find-system-without-clpm system-name))
                    (lookup-system)))))
           (:error
            (restart-case
                ;; Prevent RESTART-CASE from using WITH-CONDITION-RESTARTS.
                (signal-missing-system system-name)
              (install-system ()
                :report "Attempt to install the system using CLPM"
                :test (lambda (c) (declare (ignore c)) (not (inside-bundle-exec-p)))
                (let ((new-source-registry (install :systems (list system-name))))
                  (when new-source-registry
                    (if (should-auto-reload-source-registry-p)
                        (progn
                          (reload-config new-source-registry)
                          (find-system-without-clpm system-name))
                        (lookup-system)))))
              (install-system-without-dependencies ()
                :report "Attempt to install the system using CLPM without also installing its dependencies"
                :test (lambda (c) (declare (ignore c)) (not (inside-bundle-exec-p)))
                (let ((new-source-registry (install :systems (list system-name) :no-deps t)))
                  (when new-source-registry
                    (if (should-auto-reload-source-registry-p)
                        (progn
                          (reload-config new-source-registry)
                          (find-system-without-clpm system-name))
                        (lookup-system)))))
              (install-and-reload-bundle ()
                :report "Install the bundle (reresolving all requiremsnts) and reload configuration from clpmfile.lock"
                :test (lambda (c) (declare (ignore c)) (inside-bundle-exec-p))
                (let ((new-source-registry (install)))
                  (when new-source-registry
                    (reload-config new-source-registry)
                    (asdf:find-system system-name))))
              (reload-bundle ()
                :report "Clear ASDF configuration and reload from clpmfile.lock."
                :test (lambda (c) (declare (ignore c)) (inside-bundle-exec-p))
                (reload-config)
                (asdf:find-system system-name))))
           ((nil)
            nil)))))))

(defun asdf-integration-active-p ()
  "Returns non-NIL iff 'CLPM-SYSTEM-DEFINITION-SEARCH is registered with ASDF's
search functions."
  (member 'clpm-system-definition-search asdf:*system-definition-search-functions*))

(defun activate-asdf-integration (&key force
                                    (order :last))
  "Add 'CLPM-SYSTEM-DEFINITION-SEARCH to ASDF's system definition search functions list.

Does nothing if ASDF-INTEGRATION-ACTIVE-P returns T and :FORCE is NIL.

:ORDER can be one of:

+ :LAST :: (default) The clpm search function is added to the end of ASDF's
 search functions.

+ :FIRST :: The CLPM search function is added to the front of ASDF's search
functions."
  (when (or force
            (not (asdf-integration-active-p)))
    (ecase order
      (:last
       (setf asdf:*system-definition-search-functions*
             (append asdf:*system-definition-search-functions* (list 'clpm-system-definition-search))))
      (:first
       (push 'clpm-system-definition-search asdf:*system-definition-search-functions*)))))

(defun deactivate-asdf-integration ()
  "Removes CLPM-SYSTEM-DEFINITION-SEARCH from ASDF's search functions."
  (setf asdf:*system-definition-search-functions*
        (remove 'clpm-system-definition-search asdf:*system-definition-search-functions*)))

;; Unregister CLPM from ASDF on CLPM cleanup.
(register-clpm-cleanup-hook 'deactivate-asdf-integration)
