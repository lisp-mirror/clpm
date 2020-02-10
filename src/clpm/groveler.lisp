;;;; Interface for using a groveler to determine .asd file contents and
;;;; dependencies as well as system dependencies.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/groveler
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/cache
          #:clpm/deps
          #:clpm/log
          #:clpm/sandbox
          #:clpm/sublisp
          #:clpm/utils
          #:exit-hooks)
  (:export #:*active-groveler*
           #:add-asd-and-retry
           #:active-groveler-ensure-asd-loaded!
           #:active-groveler-load-asd!
           #:active-groveler-system-deps
           #:active-groveler-systems-in-file
           #:groveler-dependency-missing
           #:groveler-dependency-missing/system
           #:groveler-load-asd!
           #:groveler-loaded-asds
           #:make-groveler))

(in-package #:clpm/groveler)

(setup-logger)

(defvar *active-groveler* nil)

(define-condition groveler-unknown-error ()
  ((backtrace
    :initarg :backtrace
    :reader groveler-unknown-error/backtrace))
  (:report (lambda (c s)
             (format s "Unknown error while groveling. Perhaps your asd file is malformed?~%Backtrace: ~A"
                     (groveler-unknown-error/backtrace c))))
  (:documentation
   "Condition signaled when the groveler encounters an unknown error."))

(define-condition groveler-dependency-missing ()
  ((system
    :initarg :system
    :reader groveler-dependency-missing/system))
  (:report (lambda (c s)
             (format s "Missing dependency: ~S" (groveler-dependency-missing/system c))))
  (:documentation
   "Condition signaled when the groveler cannot continue because of a missing
dependency."))

(defclass groveler ()
  ((sub-lisp
    :initarg :sub-lisp
    :reader groveler-sub-lisp)
   (loaded-asds
    :initform nil
    :initarg :loaded-asds
    :accessor groveler-loaded-asds)))

(defun make-groveler ()
  (let* ((sub-lisp (launch-sub-lisp))
         (input (sub-lisp-input sub-lisp))
         (output (sub-lisp-output sub-lisp))
         (asd-pathname (deps-asd-pathname)))
    (ensure-deps-system-in-cache!)
    (uiop:with-safe-io-syntax ()
      ;; Load ASDF
      (format input "(progn (require :asdf) (values))~%")
      ;; Nuke any existing ASDF configuration
      (format input "(progn (asdf:clear-configuration) (values))~%")
      ;; Make sure if ASDF reinitializes its configuration that it ignores
      ;; everything but the default (implementation specific) systems.
      (format input "(progn (setf asdf:*default-source-registries* nil) (values))~%")
      ;; Load the groveler
      (format input "(progn (asdf:load-asd ~S) (values))~%" asd-pathname)
      (format input "(progn (asdf:load-system :clpm-deps) (values))~%")
      ;; Start the groveler's REPL.
      (format input "(progn (clpm-deps:start-rel) (values))~%")
      (format input "(print \"clpm-deps ready\")~%")
      (finish-output input)
      ;; Wait until the process reports that it is ready.
      (loop
        :for val := (read output)
        :until (equal val "clpm-deps ready")))
    (aprog1 (make-instance 'groveler
                           :sub-lisp sub-lisp)
      (exit-hooks:add-exit-hook (lambda ()
                                  (kill-groveler! it))))))

(defun kill-groveler! (groveler)
  "Terminate the groveler's sublisp."
  (when (sub-lisp-alive-p (groveler-sub-lisp groveler))
    (stop-sub-lisp (groveler-sub-lisp groveler) :signal t :urgent t)))

(defun groveler-load-asd! (groveler asd-pathname)
  "Load the .asd file at ~asd-pathname~ into the groveler's lisp process."
  (let* ((sub-lisp (groveler-sub-lisp groveler))
         (in-stream (sub-lisp-input sub-lisp))
         (out-stream (sub-lisp-output sub-lisp))
         (asd-pathname (uiop:ensure-absolute-pathname asd-pathname)))
    (tagbody
     start
       (log:debug "Loading ~S into groveler" asd-pathname)
       (uiop:with-safe-io-syntax ()
         (format in-stream "(print (multiple-value-list (clpm-deps:safe-load-asd ~S))) ~%"
                 asd-pathname)
         (format in-stream "(finish-output)~%")
         (finish-output in-stream)
         (multiple-value-bind (success-p unknown-error-backtrace missing-system)
             (values-list (read out-stream))
           (unless success-p
             (when (or (null missing-system) unknown-error-backtrace)
               ;; Uh-oh, something happened that we don't know how to recover
               ;; from here. Signal an error!
               (error 'groveler-unknown-error
                      :backtrace unknown-error-backtrace))
             ;; We can provide a way to recover from this!
             (restart-case
                 (error 'groveler-dependency-missing :system missing-system)
               (add-asd-and-retry (asd &optional callback)
                 :report
                 "Add an asd file to load and try again."
                 :interactive read-asd-path
                 (groveler-load-asd! groveler asd)
                 (when callback
                   (funcall callback))
                 (go start))))
           (push (namestring asd-pathname) (groveler-loaded-asds groveler)))))
    groveler))

(defun groveler-ensure-asd-loaded! (groveler asd-pathname)
  (let ((asd-pathname (uiop:ensure-absolute-pathname asd-pathname)))
    (unless (member (namestring asd-pathname) (groveler-loaded-asds groveler) :test #'equal)
      (groveler-load-asd! groveler asd-pathname))))

(defun groveler-system-deps (groveler system-name)
  "Query and return a list of the dependencies of ~system-name~."
  (let* ((sub-lisp (groveler-sub-lisp groveler))
         (in-stream (sub-lisp-input sub-lisp))
         (out-stream (sub-lisp-output sub-lisp)))
    (log:debug "Querying groveler for dependencies of ~S" system-name)
    (uiop:with-safe-io-syntax ()
      (format in-stream "(print (clpm-deps:system-direct-deps (asdf:find-system ~S)))~%"
              system-name)
      (format in-stream "(finish-output)~%")
      (finish-output in-stream)
      (read out-stream))))

(defun groveler-systems-in-file (groveler asd-pathname)
  "Return a list of systems defined in the provided asd file."
  (let* ((sub-lisp (groveler-sub-lisp groveler))
         (in-stream (sub-lisp-input sub-lisp))
         (out-stream (sub-lisp-output sub-lisp))
         (asd-pathname (uiop:ensure-absolute-pathname asd-pathname)))
    (block nil
      (tagbody
       start
         (log:debug "Querying groveler for systems in file ~S" asd-pathname)
         (uiop:with-safe-io-syntax ()
           (format in-stream "(print (multiple-value-list (clpm-deps:determine-systems-from-file ~S)))~%"
                   asd-pathname)
           (format in-stream "(finish-output)~%")
           (finish-output in-stream)
           (let ((raw-form (read out-stream)))
             (log:debug "Raw form: ~S" raw-form)
             (multiple-value-bind (result unknown-error-backtrace missing-system)
                 (values-list raw-form)
               (when unknown-error-backtrace
                 ;; Uh-oh, something happened that we don't know how to recover from
                 ;; here. Signal an error!
                 (error 'groveler-unknown-error
                        :backtrace unknown-error-backtrace))
               (when missing-system
                 ;; We can provide a way to recover from this!
                 (restart-case
                     (error 'groveler-dependency-missing :system missing-system)
                   (add-asd-and-retry (asd &optional callback)
                     :report
                     "Add an asd file to load and try again."
                     :interactive read-asd-path
                     (groveler-load-asd! groveler asd)
                     (when callback
                       (funcall callback))
                     (go start))))
               (return result))))))))

(defun active-groveler-load-asd! (asd-pathname)
  (unless *active-groveler*
    (error "No active groveler defined!"))
  (groveler-load-asd! (if (functionp *active-groveler*)
                          (funcall *active-groveler*)
                          *active-groveler*)
                      asd-pathname))

(defun active-groveler-ensure-asd-loaded! (asd-pathname)
  (unless *active-groveler*
    (error "No active groveler defined!"))
  (groveler-ensure-asd-loaded! (if (functionp *active-groveler*)
                                   (funcall *active-groveler*)
                                   *active-groveler*)
                               asd-pathname))

(defun active-groveler-system-deps (system-name)
  (unless *active-groveler*
    (error "No active groveler defined!"))
  (groveler-system-deps (if (functionp *active-groveler*)
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

(defun read-asd-path ()
  (format t "Enter a path to an asd file (not evaluated): ")
  (list (read-line)))
