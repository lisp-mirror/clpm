;;;; ASDF Integration with CLPM client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/asdf
    (:use #:cl
          #:clpm-client/bundle
          #:clpm-client/cleanup
          #:clpm-client/clpm
          #:clpm-client/install)
  (:import-from #:asdf
                #:*system-definition-search-functions*)
  (:export #:*clpm-system-not-found-behavior*
           #:activate-clpm
           #:clpm-active-p
           #:clpm-missing-system
           #:clpm-system-search
           #:deactivate-clpm))

(in-package #:clpm-client/asdf)

(defvar *clpm-system-not-found-behavior* :error
  "The behavior if CLPM is asked to find a system that is not installed. One of:

+ :ERROR :: Signal a CLPM-MISSING-SYSTEM condition with the INSTALL-SYSTEM
restart available.

+ :INSTALL :: Just install the system without prompting.

+ :INSTALL-WITH-DEPS :: Install the system and its dependencies without
prompting.

+ NIL :: Do nothing.")

(defvar *clpm-installed-systems* nil
  "A cache to hold a mapping from primary system names to .asd pathnames for
systems installed by CLPM during this session. Used because there doesn't seem
to be an exposed, stable way to update ASDF's source registry once we install a
new system.")

(define-condition clpm-missing-system (error)
  ((system-name
    :initarg :system-name))
  (:report (lambda (c s)
             (format s "System ~A is not installed"
                     (slot-value c 'system-name))))
  (:documentation "A condition signaled when a system is not installed."))

(defmacro maybe-restart-case (condition form &rest cases)
  `(if ,condition
       (restart-case ,form
         ,@cases)
       ,form))

(defun install-system-and-record-pathnames (system-name &key no-deps-p)
  (let ((pathnames (ignore-errors
                    (clpm-install-system system-name
                                         :no-deps-p no-deps-p))))
    (dolist (p pathnames)
      (setf (gethash (pathname-name p) *clpm-installed-systems*)
            (pathname p)))))

(defun populate-installed-systems ()
  (setf *clpm-installed-systems* (make-hash-table :test 'equal))
  (let ((pathnames (ignore-errors
                    (run-clpm `("context" "pathnames" "--output=sexp" ,*clpm-context*)
                              :output '(:string :stripped t)))))
    (when pathnames
      (dolist (p (uiop:with-safe-io-syntax () (read-from-string pathnames)))
        (setf (gethash (pathname-name p) *clpm-installed-systems*)
              (pathname p))))))

(defun clpm-system-search (system-name)
  "A search function for ASDF's *SYSTEM-DEFINITION-SEARCH-FUNCTIONS* list.

Given a system name, it tries to find it using the CLPM executable (see:
*CLPM-EXECUTABLE*). If the system cannot be found, it either does nothing,
signals a condition, and/or installs the system with CLPM-INSTALL-SYSTEM (see:
*CLPM-SYSTEM-NOT-FOUND-BEHAVIOR*)."
  ;; If this is the first time we've called this, try to seed the installed
  ;; systems hash table first.
  (unless *clpm-installed-systems*
    (populate-installed-systems))
  (let ((primary-name (asdf:primary-system-name system-name)))
    (unless (or (equal "asdf" primary-name)
                (equal "uiop" primary-name))
      (flet ((lookup-system ()
               (gethash primary-name *clpm-installed-systems*)))
        (or
         (lookup-system)
         (ecase *clpm-system-not-found-behavior*
           (:install
            (install-system-and-record-pathnames system-name :no-deps-p t)
            (lookup-system))
           (:install-with-deps
            (install-system-and-record-pathnames system-name)
            (lookup-system))
           (:error
            (maybe-restart-case
             (inside-bundle-exec-p)
             (restart-case
                 ;; The progn is necessary here to prevent RESTART-CASE from
                 ;; using WITH-CONDITION-RESTARTS to associate the restarts with
                 ;; only this condition. This becomes an issue for
                 ;; :DEFSYSTEM-DEPENDS-ON, becuase this error will be signaled
                 ;; in the middle of an ASDF DEFINE-OP and ASDF will catch it,
                 ;; wrap it, and signal a new condition. If
                 ;; WITH-CONDITION-RESTARTS is used in the expansion of this
                 ;; macro, then these restarts won't be available in the
                 ;; interactive debugger in this case.
                 (progn
                   (error 'clpm-missing-system :system-name system-name))
               (install-system ()
                 :report "Attempt to install the system using CLPM."
                 (let ((*clpm-system-not-found-behavior* nil))
                   (install-system-and-record-pathnames system-name :no-deps-p t)
                   (lookup-system)))
               (install-system-and-dependencies ()
                 :report "Attempt to install the system and its dependencies using CLPM."
                 (let ((*clpm-system-not-found-behavior* nil))
                   (install-system-and-record-pathnames system-name)
                   (lookup-system)))
               (continue ()
                 :report "Return control to ASDF."
                 nil))
             (reload-bundle ()
                            :report "Clear ASDF configuration and reload from clpmfile.lock."
                            (asdf:clear-configuration)
                            (asdf:initialize-source-registry 'bundle-pathnames-for-asd-config)
                            (clpm-system-search system-name))))
           ((nil)
            nil)))))))

(defun clpm-active-p ()
  "Returns T iff 'CLPM-SYSTEM-SEARCH is registered with ASDF's search functions."
  (member 'clpm-system-search *system-definition-search-functions*))

(defun activate-clpm (&key force
                        (order :last))
  "Add 'CLPM-SYSTEM-SEARCH to ASDF's system definition search functions list.

Does nothing if CLPM-ACTIVE-P returns T and :FORCE is NIL.

:ORDER can be one of:

+ :LAST :: (default) The clpm search function is added to the end of ASDF's
 search functions.

+ :FIRST :: The CLPM search function is added to the front of ASDF's search functions."
  (when (or force
            (not (clpm-active-p)))
    (ecase order
      (:last
       (setf *system-definition-search-functions*
             (append *system-definition-search-functions* (list 'clpm-system-search))))
      (:first
       (push 'clpm-system-search *system-definition-search-functions*)))))

(defun deactivate-clpm ()
  "Removes ~clpm-system-search~ from ASDF's search functions."
  (setf *system-definition-search-functions*
        (remove 'clpm-system-search *system-definition-search-functions*)))

(defun clear-clpm-cache ()
  (setf *clpm-installed-systems* (make-hash-table :test 'equal)))
(uiop:register-clear-configuration-hook 'clear-clpm-cache)

(defun clear-clpm-cache-hook ()
  (setf uiop:*clear-configuration-hook*
        (remove 'clear-clpm-cache uiop:*clear-configuration-hook*)))

;; Unregister CLPM from ASDF on CLPM cleanup.
(register-clpm-cleanup-hook 'deactivate-clpm)
(register-clpm-cleanup-hook 'clear-clpm-cache-hook)
