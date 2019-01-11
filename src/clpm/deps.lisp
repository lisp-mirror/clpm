;;;; Interface for using a groveler to determine .asd file contents and
;;;; dependencies as well as system dependencies.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/deps
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/cache
          #:clpm/config
          #:clpm/sandbox
          #:clpm/utils
          #:exit-hooks)
  (:export #:add-asd-and-retry
           #:grovel-system-info
           #:grovel-systems-in-file
           #:groveler-dependency-missing
           #:groveler-dependency-missing/system))

(in-package #:clpm/deps)

(defvar *running-grovelers* (make-hash-table :test 'equal)
  "Hash table that maps asd pathnames to running groveler processes.")

(defvar *deps-system* nil
  "A string containing the contents of the groveler's clpm-deps.asd. Populated
at image dump.")
(defvar *deps-source* nil
  "A string containing the contents of the groveler's main.lisp. Populated at
image dump.")
(defvar *deps-version* nil
  "A string containing the contents of the groveler's version.sexp. Populated at
image dump.")

(defun read-deps-system-string ()
  "Read clpm-deps.asd from file as a string."
  (uiop:read-file-string (asdf:system-relative-pathname :clpm "src/clpm-deps/clpm-deps.asd")))
(defun read-deps-source-string ()
  "Read main.lisp from file as a string."
  (uiop:read-file-string (asdf:system-relative-pathname :clpm "src/clpm-deps/main.lisp")))
(defun read-deps-version-string ()
  "Read version.sexp from file as a string."
  (uiop:read-file-form (asdf:system-relative-pathname :clpm "src/clpm-deps/version.sexp")))

(defun cache-files! ()
  "Cache the groveler files in the variables."
  (setf *deps-system* (read-deps-system-string))
  (setf *deps-source* (read-deps-source-string))
  (setf *deps-version* (read-deps-version-string)))
(uiop:register-image-dump-hook 'cache-files!)

(defun deps-system-string ()
  "Return clpm-deps.asd as a string. Prefers cached version instead of reading
from file."
  (or *deps-system*
      (read-deps-system-string)))
(defun deps-source-string ()
  "Return main.lisp as a string. Prefers cached version instead of reading from
file."
  (or *deps-source*
      (read-deps-source-string)))
(defun deps-version-string ()
  "Return version.sexp as a string. Prefers cached version instead of reading
from file."
  (or *deps-version*
      (read-deps-version-string)))

(defparameter *force-deps-cache-writing-p* t
  "If T, ~ensure-deps-system-in-cache!~ defaults to writing files to the cache
even if they exist.")

(defun unforce-cache-writing ()
  "Set ~*force-deps-cache-writing-p*~ to NIL."
  (setf *force-deps-cache-writing-p* nil))
(uiop:register-image-dump-hook 'unforce-cache-writing)

(defun ensure-deps-system-in-cache! (&optional (forcep *force-deps-cache-writing-p*))
  "Ensure the groveler source code is present in the cache. If ~forcep~ is
non-NIL, overwrite."
  (let* ((deps-version (deps-version-string))
         (deps-system-pathname (clpm-cache-pathname
                                (list
                                 "deps-groveler-system"
                                 deps-version
                                 "clpm-deps.asd")))
         (deps-source-pathname (clpm-cache-pathname
                                (list
                                 "deps-groveler-system"
                                 deps-version
                                 "main.lisp")))
         (deps-version-pathname (clpm-cache-pathname
                                 (list
                                  "deps-groveler-system"
                                  deps-version
                                  "version.sexp"))))
    (ensure-directories-exist deps-system-pathname)
    (unless (and (not forcep)
                 (probe-file deps-system-pathname))
      (write-string-into-file (deps-system-string) deps-system-pathname :if-exists :supersede))
    (unless (and (not forcep)
                 (probe-file deps-source-pathname))
      (write-string-into-file (deps-source-string) deps-source-pathname :if-exists :supersede))
    (unless (and (not forcep)
                 (probe-file deps-version-pathname))
      (with-open-file (s deps-version-pathname :direction :output :if-exists :supersede)
        (print deps-version s)))))

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

(defun launch-sub-lisp ()
  "Start a SBCL process that does not load config files, has a disabled
debugger, and does not print things unless told. Set ASDF_OUTPUT_TRANSLATIONS
for process to confine build artifacts to CLPM's cache. Use
~sandbox-augment-command~ to run the child in a sandbox."
  (let* ((cache-dir (clpm-cache-pathname '("deps-build")
                                         :ensure-directory t))
         (translations
           (format nil
                   "(:output-translations :ignore-inherited-configuration (t (\"~A\" :implementation :**/ :*.*.*)))"
                   cache-dir)))
    (ensure-directories-exist cache-dir)

    (apply
     #'uiop:launch-program
     (sandbox-augment-command
      `("sbcl"
        "--noinform"
        "--noprint"
        "--no-sysinit"
        "--no-userinit"
        "--disable-debugger")
      :read-write-pathnames (list cache-dir))
     :input :stream
     :output :stream
     :error-output :interactive
     (run-program-augment-env-args `(("ASDF_OUTPUT_TRANSLATIONS" . ,translations))))))

(defun start-grovel-process ()
  "Start and return a process (using ~launch-sub-lisp~) that contains an
instance of SBCL with the groveling code loaded."
  (ensure-deps-system-in-cache!)
  (let* ((proc-info (launch-sub-lisp))
         (in-stream (uiop:process-info-input proc-info))
         (out-stream (uiop:process-info-output proc-info))
         (asd-pathname (clpm-cache-pathname
                        (list "deps-groveler-system"
                              (deps-version-string)
                              "clpm-deps.asd"))))
    (uiop:with-safe-io-syntax ()
      (format in-stream "(require :asdf)~%")
      ;; Nuke any existing asdf configuration
      (format in-stream "(asdf:clear-configuration)~%")
      ;; Make sure if ASDF reinitializes its configuration that it ignores
      ;; everything but the default (implementation specific) systems.
      (format in-stream "(setf asdf:*default-source-registries* nil)~%")
      (format in-stream "(asdf:load-asd ~S)~%" asd-pathname)
      (format in-stream "(asdf:load-system :clpm-deps)~%")
      (format in-stream "(print \"ready\")~%")
      (format in-stream "(finish-output)~%")
      (finish-output in-stream)
      ;; Wait until the process reports that it is ready.
      (loop
        :for val := (read out-stream)
        :until (equal val "ready")))
    proc-info))

(defun safe-load-asd-in-process (proc-info asd-pathname)
  "Load the .asd file at ~asd-pathname~ into the ~proc-info~'s lisp process."
  (let* ((in-stream (uiop:process-info-input proc-info))
         (out-stream (uiop:process-info-output proc-info)))
    (uiop:with-safe-io-syntax ()
      (format in-stream "(print (multiple-value-list (clpm-deps/main::safe-load-asd ~S)))~%" asd-pathname)
      (format in-stream "(finish-output)~%")
      (finish-output in-stream)
      (values-list (read out-stream)))))

(defun determine-system-deps-in-process (proc-info system-name)
  "Query and return a list of the dependencies of ~system-name~."
  (let* ((in-stream (uiop:process-info-input proc-info))
         (out-stream (uiop:process-info-output proc-info)))
    (uiop:with-safe-io-syntax ()
      (format in-stream "(print (clpm-deps/main::system-direct-deps (asdf:find-system ~S)))~%" system-name)
      (format in-stream "(finish-output)~%")
      (finish-output in-stream)
      (read out-stream))))

(defun determine-systems-from-file-in-process (proc-info system-pathname)
  "Return a list of systems defined in the .asd file located at
~system-pathname~."
  (let* ((in-stream (uiop:process-info-input proc-info))
         (out-stream (uiop:process-info-output proc-info)))
    (uiop:with-safe-io-syntax ()
      (format in-stream "(print (clpm-deps/main::determine-systems-from-file ~S))~%" system-pathname)
      (format in-stream "(finish-output)~%")
      (finish-output in-stream)
      (read out-stream))))

(defun ensure-process-ready! (proc-info asd-pathname dependency-asd-files)
  "Ensures that the process has asd-pathname successfully loaded. Establishes a
~add-asd-and-retry~ restart."
  (tagbody
   start
     (dolist (file (append dependency-asd-files (list asd-pathname)))
       (multiple-value-bind (success-p unknown-error-backtrace missing-system)
           (safe-load-asd-in-process proc-info file)
         (unless success-p
           (when unknown-error-backtrace
             ;; Uh-oh something happened that we don't know how to recover from
             ;; here. First, kill the process, then signal an error.
             (kill-groveler-process! proc-info asd-pathname)
             (error 'groveler-unknown-error
                    :backtrace unknown-error-backtrace))
           ;; We can provide a way to recover from this!
           (restart-case
               (error 'groveler-dependency-missing :system missing-system)
             (add-asd-and-retry (asd)
               :report "Add an asd file to preload and try again."
               :interactive read-asd-path
               (push asd dependency-asd-files)
               (go start))))))))

(defun kill-groveler-process! (proc-info asd-file)
  "Terminate the sublisp."
  (when (uiop:process-alive-p proc-info)
    (uiop:terminate-process proc-info :urgent t))
  (remhash (truename asd-file) *running-grovelers*)
  (uiop:wait-process proc-info))

(defun kill-groveler-process-for-file! (asd-file)
  "Kill the groveler process associated with ~asd-file~."
  (when-let ((proc-info (gethash (truename asd-file) *running-grovelers*)))
    (kill-groveler-process! proc-info asd-file)))

(defun get-groveler-process (asd-file dependency-asd-files)
  "Get the groveler process that has asd-file loaded and ready to be queried."
  (ensure-gethash (truename asd-file) *running-grovelers*
                  (aprog1 (start-grovel-process)
                    (ensure-process-ready! it asd-file dependency-asd-files))))

(defun grovel-system-info (asd-file system-name)
  "Grovel for the dependencies of ~system-name~ in ~asd-file~."
  (determine-system-deps-in-process (get-groveler-process asd-file nil) system-name))

(defun grovel-systems-in-file (asd-file)
  "Grovel for the systems defined in ~asd-file~."
  (determine-systems-from-file-in-process (get-groveler-process asd-file nil)
                                          (uiop:truename* asd-file)))

(defun exit-hook ()
  "Terminates all groveler processes."
  (maphash-keys (lambda (k)
                  (kill-groveler-process-for-file! k))
                *running-grovelers*))

(add-exit-hook 'exit-hook)

(defun read-asd-path ()
  (format t "Enter a path to an asd file (not evaluated): ")
  (list (read-line)))
