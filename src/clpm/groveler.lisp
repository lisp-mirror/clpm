;;;; Interface for using a groveler to determine .asd file contents and
;;;; dependencies as well as system dependencies.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/groveler
    (:use #:cl
          #:anaphora
          #:asdf-system-groveler
          #:clpm/log
          #:clpm/sandbox
          #:exit-hooks)
  (:shadow #:make-groveler)
  (:reexport #:asdf-system-groveler)
  (:export #:*active-groveler*
           #:active-groveler-ensure-asd-loaded
           #:active-groveler-load-asd
           #:active-groveler-system-deps
           #:active-groveler-systems-in-file))

(in-package #:clpm/groveler)

(setup-logger)

(defvar *active-groveler* nil)

(defun make-groveler ()
  (let ((dir (asdf-system-groveler:mkdtemp (merge-pathnames "clpm"
                                                            (uiop:temporary-directory)))))
    (flet ((rewrite (args)
             (sandbox-augment-command args :read-write-pathnames (list dir))))
      (aprog1 (asdf-system-groveler:make-groveler :sbcl
                                                  :asdf-fasl-cache-dir dir
                                                  :keep-asdf-fasl-cache-dir nil
                                                  :rewrite-arguments-callback #'rewrite)
        (add-exit-hook (lambda ()
                         (asdf-system-groveler:groveler-destroy it)))))))

(defun active-groveler-load-asd (asd-pathname)
  (unless *active-groveler*
    (error "No active groveler defined!"))
  (groveler-load-asd (if (functionp *active-groveler*)
                         (funcall *active-groveler*)
                         *active-groveler*)
                     asd-pathname))

(defun active-groveler-ensure-asd-loaded (asd-pathname)
  (unless *active-groveler*
    (error "No active groveler defined!"))
  (groveler-ensure-asd-loaded (if (functionp *active-groveler*)
                                  (funcall *active-groveler*)
                                  *active-groveler*)
                              asd-pathname))

(defun active-groveler-system-deps (system-name)
  (unless *active-groveler*
    (error "No active groveler defined!"))
  (groveler-system-info (if (functionp *active-groveler*)
                            (funcall *active-groveler*)
                            *active-groveler*)
                        system-name))

(defun active-groveler-systems-in-file (asd-pathname)
  (unless *active-groveler*
    (error "No active groveler defined!"))
  (groveler-systems-in-file (if (functionp *active-groveler*)
                                (funcall *active-groveler*)
                                *active-groveler*)
                            asd-pathname))
