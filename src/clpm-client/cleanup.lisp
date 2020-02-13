;;;; Cleaning up the client on image dump
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/cleanup
    (:use #:cl
          ;; Everything must depend on header so that it comes first in the
          ;; concatenated file.
          #:clpm-client/header)
  (:export #:*clpm-cleanup-on-dump-p*
           #:cleanup-clpm!
           #:register-clpm-cleanup-hook))

(in-package #:clpm-client/cleanup)

(defvar *clpm-cleanup-on-dump-p* t
  "If non-NIL, CLPM will cleanup after itself if an image is dumped in a manner
that uses UIOP's image dump hooks. See ~cleanup-clpm!~ for details.")

(defvar *clpm-cleanup-hooks* nil
  "List of functions to run in ~cleanup-clpm!~.")

(defun register-clpm-cleanup-hook (hook)
  (pushnew hook *clpm-cleanup-hooks*))

(defun clpm-package-p (p)
  (let ((package-name (package-name p)))
    (and package-name
         (string-equal "clpm-client"
                       (first (uiop:split-string package-name :separator '(#\/)))))))

(defun cleanup-clpm! (&key force)
  "If *CLPM-CLEANUP-ON-DUMP-P* is non-NIL or :FORCE is non-NIL, this function
performs the following steps:

1. Removes ~:clpm-client~ from ~*features*~

2. Runs all hooks in ~*clpm-cleanup-hooks*~.

3. Removes ~'cleanup-clpm!~ from ~uiop:*image-dump-hook*~

4. Deletes all packages that start with ~clpm-client/~."
  (when (or *clpm-cleanup-on-dump-p* force)
    ;; Remove ourselves from *FEATURES*
    (setf *features* (remove :clpm-client *features*))
    ;; Run all cleanup hooks
    (mapc 'funcall *clpm-cleanup-hooks*)
    ;; Remove ourselves from UIOP's image dump hook
    ;; (setf asdf:*system-definition-search-functions*
    ;;       (remove 'clpm-system-search asdf:*system-definition-search-functions*))
    (setf uiop:*image-dump-hook* (remove 'cleanup-clpm! uiop:*image-dump-hook*))
    ;; Find all clpm-client packages and delete them.
    (let ((packages (remove-if-not 'clpm-package-p (list-all-packages))))
      (handler-bind ((package-error #'continue))
        (mapc #'delete-package packages)))))

;; Register our cleanup function with UIOP's image dump facilities.
(uiop:register-image-dump-hook 'cleanup-clpm!)
