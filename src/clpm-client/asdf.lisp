;;;; ASDF Integration with CLPM client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/asdf
    (:use #:cl
          #:clpm-client/bundle
          #:clpm-client/cleanup
          #:clpm-client/clpm
          #:clpm-client/context
          #:clpm-client/install)
  (:import-from #:asdf
                #:*system-definition-search-functions*)
  (:export #:*clpm-system-not-found-behavior*
           #:activate-clpm-asdf-integration
           #:clpm-asdf-integration-active-p
           #:clpm-missing-system
           #:clpm-system-search
           #:deactivate-clpm-asdf-integration
           #:install-and-reload-bundle
           #:install-system
           #:install-system-and-dependencies
           #:reload-bundle))

(in-package #:clpm-client/asdf)

(defvar *clpm-system-not-found-behavior* :error
  "The behavior if CLPM is asked to find a system that is not installed. One of:

+ :ERROR :: Signal a CLPM-MISSING-SYSTEM condition with the INSTALL-SYSTEM
restart available.

+ :INSTALL :: Just install the system without prompting.

+ :INSTALL-WITH-DEPS :: Install the system and its dependencies without
prompting.

+ NIL :: Do nothing.")

(define-condition clpm-missing-system (error)
  ((system-name
    :initarg :system-name))
  (:report (lambda (c s)
             (format s "System ~A is not installed"
                     (slot-value c 'system-name))))
  (:documentation "A condition signaled when a system is not installed."))

(define-condition clpm-bundle-install-diff ()
  ((diff
    :initarg :diff))
  (:report (lambda (c s)
             (format s "Bundle install would perform the following changes:~%~S"
                     (slot-value c 'diff))))
  (:documentation "A condition signaled when bundle install produces a diff."))

(defun signal-missing-system (system-name)
  (error 'clpm-missing-system :system-name system-name))

(defun find-system-without-clpm (system-name)
  (let ((*clpm-system-not-found-behavior* nil))
    (asdf:find-system system-name)))

(defun clpm-bundle-install-validate-diff (diff)
  (restart-case
      (progn (error 'clpm-bundle-install-diff :diff diff))
    (continue ()
      :report "Accept and continue"
      t)
    (abort ()
      :report "Do not accept the diff"
      nil)))

(defun clpm-system-search-bundle (system-name)
  "ASDF search function for use inside a bundle. When inside a bundle exec, ASDF
is configured entirely through environment variables and that configuration is
exhaustive. If a system is missing, the best we can do is either reload the
config from the lock file (in case the lockfile has been modified outside this
process), or run a bundle install to propagate new dependencies and requirements
into the lock file and then reload the config."
  (flet ((reload-config ()
           (asdf:clear-source-registry)
           (asdf:initialize-source-registry (clpm-bundle-source-registry))))
    (ecase *clpm-system-not-found-behavior*
      ((:install :install-with-deps)
       (clpm-bundle-install)
       (reload-config)
       (find-system-without-clpm system-name))
      (:error
       (restart-case
           ;; Prevent RESTART-CASE from using WITH-CONDITION-RESTARTS
           (signal-missing-system system-name)
         (install-and-reload-bundle ()
           :report "Run bundle install and reload configuration form clpmfile.lock"
           (when (clpm-bundle-install :validate 'clpm-bundle-install-validate-diff)
             (reload-config)
             (asdf:find-system system-name)))
         (reload-bundle ()
           :report "Clear ASDF configuration and reload from clpmfile.lock."
           (reload-config)
           (asdf:find-system system-name))))
      ((nil)
       nil))))

(defun clpm-system-search-no-bundle (system-name)
  "ASDF search function for use outside of a bundle, when global contexts are in
use."
  (let ((primary-name (asdf:primary-system-name system-name)))
    (flet ((lookup-system ()
             (clpm-context-find-system primary-name)))
      (or
       (lookup-system)
       (ecase *clpm-system-not-found-behavior*
         (:install
          (clpm-install-system system-name :no-deps-p t)
          (lookup-system))
         (:install-with-deps
          (clpm-install-system system-name)
          (lookup-system))
         (:error
          (restart-case
              ;; Prevent RESTART-CASE from using WITH-CONDITION-RESTARTS
              (signal-missing-system system-name)
            (install-system ()
              :report "Attempt to install the system using CLPM."
              (let ((*clpm-system-not-found-behavior* nil))
                (clpm-install-system system-name :no-deps-p t)
                (lookup-system)))
            (install-system-and-dependencies ()
              :report "Attempt to install the system and its dependencies using CLPM."
              (let ((*clpm-system-not-found-behavior* nil))
                (clpm-install-system system-name)
                (lookup-system)))
            (continue ()
              :report "Return control to ASDF."
              nil)))
         ((nil)
          nil))))))

(defun clpm-system-search (system-name)
  "A search function for ASDF's *SYSTEM-DEFINITION-SEARCH-FUNCTIONS* list.

Given a system name, it tries to find it using the CLPM executable (see:
*CLPM-EXECUTABLE*). If the system cannot be found, it either does nothing,
signals a condition, and/or installs the system with CLPM-INSTALL-SYSTEM (see:
*CLPM-SYSTEM-NOT-FOUND-BEHAVIOR*)."
  (let ((primary-name (asdf:primary-system-name system-name)))
    (unless (or (equal "asdf" primary-name)
                (equal "uiop" primary-name))
      (if (clpm-inside-bundle-exec-p)
          (clpm-system-search-bundle system-name)
          (clpm-system-search-no-bundle system-name)))))

(defun clpm-asdf-integration-active-p ()
  "Returns T iff 'CLPM-SYSTEM-SEARCH is registered with ASDF's search functions."
  (member 'clpm-system-search *system-definition-search-functions*))

(defun activate-clpm-asdf-integration (&key force
                                         (order :last))
  "Add 'CLPM-SYSTEM-SEARCH to ASDF's system definition search functions list.

Does nothing if CLPM-ASDF-INTEGRATION-ACTIVE-P returns T and :FORCE is NIL.

:ORDER can be one of:

+ :LAST :: (default) The clpm search function is added to the end of ASDF's
 search functions.

+ :FIRST :: The CLPM search function is added to the front of ASDF's search functions."
  (when (or force
            (not (clpm-asdf-integration-active-p)))
    (ecase order
      (:last
       (setf *system-definition-search-functions*
             (append *system-definition-search-functions* (list 'clpm-system-search))))
      (:first
       (push 'clpm-system-search *system-definition-search-functions*)))))

(defun deactivate-clpm-asdf-integration ()
  "Removes ~clpm-system-search~ from ASDF's search functions."
  (setf *system-definition-search-functions*
        (remove 'clpm-system-search *system-definition-search-functions*)))

;; Unregister CLPM from ASDF on CLPM cleanup.
(register-clpm-cleanup-hook 'deactivate-clpm-asdf-integration)
