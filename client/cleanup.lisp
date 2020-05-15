;;;; Cleaning up the client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)

(defvar *cleanup-on-dump-p* nil
  "If non-NIL, the CLPM client will cleanup after itself if an image is dumped
in a manner that uses UIOP's image dump hooks. See CLEANUP-CLPM-CLIENT for
details.")

(defvar *cleanup-hooks* nil
  "List of functions to run in CLEANUP-CLPM-CLIENT. Meant for internal use.")

(defun register-clpm-cleanup-hook (hook)
  "Register a new function to be called as part of CLEANUP-CLPM-CLIENT."
  (pushnew hook *cleanup-hooks*))

(defun cleanup-clpm-client ()
  "Performs the following steps:

1. Removes :CLPM-CLIENT from *FEATURES*

2. Runs all hooks in *CLEANUP-HOOKS*.

3. Removes MAYBE-CLEANUP-CLPM-CLIENT from UIOP:*IMAGE-DUMP-HOOK*

4. Deletes the CLPM-CLIENT package."
  ;; Remove ourselves from *FEATURES*
  (setf *features* (remove :clpm-client *features*))
  ;; Run all cleanup hooks
  (mapc 'funcall *cleanup-hooks*)
  ;; Remove ourselves from UIOP's image dump hook
  (setf uiop:*image-dump-hook* (remove 'maybe-cleanup-clpm-client uiop:*image-dump-hook*))
  ;; Delete the clpm-client package.
  (delete-package (find-package :clpm-client)))

(defun maybe-cleanup-clpm-client ()
  "Run CLEANUP-CLPM-CLIENT iff *CLEANUP-ON-DUMP-P* is non-NIL."
  (when *cleanup-on-dump-p*
    (cleanup-clpm-client)))

;; Register our cleanup function with UIOP's image dump facilities.
(uiop:register-image-dump-hook 'maybe-cleanup-clpm-client)
