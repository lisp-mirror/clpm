;;;; ASDF Integration with CLPM client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/asdf
    (:use #:cl
          #:clpm-client/bundle
          #:clpm-client/cleanup
          #:clpm-client/clpm)
  (:import-from #:asdf
                #:*system-definition-search-functions*)
  (:export #:*clpm-system-not-found-behavior*
           #:activate-clpm
           #:clpm-active-p
           #:clpm-install-system
           #:clpm-missing-system
           #:clpm-system-search
           #:deactivate-clpm))

(in-package #:clpm-client/asdf)

(defvar *clpm-system-not-found-behavior* :error
  "The behavior if CLPM is asked to find a system that is not installed. One of:

+ :ERROR :: Signal a CLPM-MISSING-SYSTEM condition with the INSTALL-SYSTEM
restart available.

+ :INSTALL :: Just install the system without prompting.

+ NIL :: Do nothing.")

(defun clpm-install-system (system-name)
  "Install a system using CLPM (see: *CLPM-EXECUTABLE*). Returns T iff the
system was successfully installed."
  (multiple-value-bind (output error-output exit-code)
      (run-clpm (list "install" "-s" system-name)
                :ignore-error-status t
                :input nil
                :output '(:string :stripped t)
                :error-output '(:string :stripped t))
    (declare (ignore output error-output))
    (= exit-code 0)))

(define-condition clpm-missing-system (error)
  ((system-name
    :initarg :system-name))
  (:report (lambda (c s)
             (format s "CLPM unable to find system locally: ~A"
                     (slot-value c 'system-name))))
  (:documentation "A condition signaled when CLPM is unable to find a system
locally."))

(defmacro maybe-restart-case (condition form &rest cases)
  `(if ,condition
       (restart-case ,form
         ,@cases)
       ,form))

(defun clpm-system-search (system-name)
  "A search function for ASDF's *SYSTEM-DEFINITION-SEARCH-FUNCTIONS* list.

Given a system name, it tries to find it using the CLPM executable (see:
*CLPM-EXECUTABLE*). If the system cannot be found, it either does nothing,
signals a condition, and/or installs the system with CLPM-INSTALL-SYSTEM (see:
*CLPM-SYSTEM-NOT-FOUND-BEHAVIOR*)."
  (unless (string-equal "asdf" system-name)
    (multiple-value-bind (output error-output exit-code)
        (values nil nil 4)
      ;; (run-clpm (list "find" "-VV" system-name)
      ;;             :ignore-error-status t
      ;;             :input nil
      ;;             :output '(:string :stripped t)
      ;;             :error-output '(:string :stripped t))
      (declare (ignore error-output))
      (if (= 0 exit-code)
          (pathname output)
          (ecase *clpm-system-not-found-behavior*
            (:install
             (when (clpm-install-system system-name)
               (clpm-system-search system-name)))
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
                  :report "Attempt to install the system"
                  (let ((*clpm-system-not-found-behavior* nil))
                    (when (clpm-install-system system-name)
                      (clpm-system-search system-name))))
                (continue ()
                  :report "Return control to ASDF"
                  nil))
              (reload-bundle ()
                :report "Clear ASDF configuration and reload from clpmfile.lock"
                (asdf:clear-configuration)
                (asdf:initialize-source-registry 'bundle-pathnames-for-asd-config)
                (clpm-system-search system-name))))
            ((nil)
             nil))))))

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

;; Unregister CLPM from ASDF on CLPM cleanup.
(register-clpm-cleanup-hook 'deactivate-clpm)
