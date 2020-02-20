;;;; Cleaning up the client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/cleanup
    (:use #:cl)
  (:export #:*clpm-cleanup-on-dump-p*
           #:cleanup-clpm!
           #:register-clpm-cleanup-hook))

(in-package #:clpm-client/cleanup)

(defvar *clpm-cleanup-on-dump-p* t
  "If non-NIL, CLPM will cleanup after itself if an image is dumped in a manner
that uses UIOP's image dump hooks. See CLEANUP-CLPM! for details.")

(defvar *clpm-cleanup-hooks* nil
  "List of functions to run in CLEANUP-CLPM!.")

(defun register-clpm-cleanup-hook (hook)
  "Register a new function to be called as part of CLEANUP-CLPM!."
  (pushnew hook *clpm-cleanup-hooks*))

(defun clpm-client-package-p (p)
  "Returns T iff P is a package belonging to the client."
  (let ((package-name (package-name p)))
    (and package-name
         (string-equal "clpm-client"
                       (first (uiop:split-string package-name :separator '(#\/)))))))

(defun cleanup-clpm! ()
  "Performs the following steps:

1. Removes :CLPM-CLIENT from *FEATURES*

2. Runs all hooks in *CLPM-CLEANUP-HOOKS*.

3. Removes 'MAYBE-CLEANUP-CLPM! from UIOP:*IMAGE-DUMP-HOOK*

4. Deletes all packages that start with CLPM-CLIENT/."
  ;; Remove ourselves from *FEATURES*
  (setf *features* (remove :clpm-client *features*))
  ;; Run all cleanup hooks
  (mapc 'funcall *clpm-cleanup-hooks*)
  ;; Remove ourselves from UIOP's image dump hook
  (setf uiop:*image-dump-hook* (remove 'cleanup-clpm! uiop:*image-dump-hook*))
  ;; Find all clpm-client packages and delete them.
  (let ((packages (remove-if-not 'clpm-client-package-p (list-all-packages))))
    (handler-bind ((package-error #'continue))
      (mapc #'delete-package packages))))

(defun maybe-cleanup-clpm! ()
  "Run CLEANUP-CLPM! iff *CLPM-CLEANUP-ON-DUMP-P* is non-NIL."
  (when *clpm-cleanup-on-dump-p*
    (cleanup-clpm!)))

;; Register our cleanup function with UIOP's image dump facilities.
(uiop:register-image-dump-hook 'maybe-cleanup-clpm!)
