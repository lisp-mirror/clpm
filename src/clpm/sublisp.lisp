;;;; Interface for spawning a new Lisp process as a child.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/sublisp
    (:use #:cl
          #:anaphora
          #:clpm/log
          #:clpm/sandbox
          #:clpm/utils
          #:exit-hooks
          #:lisp-invocation)
  (:export #:launch-sub-lisp
           #:stop-sub-lisp
           #:sub-lisp-alive-p
           #:sub-lisp-input
           #:sub-lisp-output))

(in-package #:clpm/sublisp)

(setup-logger)

(defvar *default-lisp* nil)

(defun clear-default-lisp ()
  (setf *default-lisp* nil))
(uiop:register-clear-configuration-hook 'clear-default-lisp)

(defun default-lisp ()
  (or *default-lisp*
      (and (sbcl-installed-p)
           (setf *default-lisp* :sbcl))
      (and (ccl-installed-p)
           (setf *default-lisp* :ccl))))

(defun test-lisp-installed-p (impl)
  (ignore-errors
   (zerop (nth-value 2
                     (uiop:run-program
                      (lisp-invocation-arglist
                       :implementation-type impl
                       :eval "(print (lisp-implementation-type))")
                      :input nil
                      :output nil
                      :error-output nil
                      :ignore-error-status t)))))

(defun sbcl-installed-p ()
  (test-lisp-installed-p :sbcl))

(defun ccl-installed-p ()
  (test-lisp-installed-p :ccl))

(defclass sub-lisp-process ()
  ((impl
    :initarg :impl
    :reader sub-lisp-process-impl)
   (process-info
    :initarg :process-info
    :reader sub-lisp-process-process-info)
   (asdf-fasl-cache-dir
    :initarg :asdf-fasl-cache-dir
    :reader sub-lisp-process-asdf-fasl-cache-dir)
   (leave-asdf-fasl-cache-dir-p
    :initarg :leave-asdf-fasl-cache-dir-p
    :reader sub-lisp-process-leave-asdf-fasl-cache-dir-p)))

(defun sub-lisp-input (sub-lisp)
  (uiop:process-info-input (sub-lisp-process-process-info sub-lisp)))

(defun sub-lisp-output (sub-lisp)
  (uiop:process-info-output (sub-lisp-process-process-info sub-lisp)))

(defun launch-sub-lisp (&key
                          (impl (default-lisp))
                          (asdf-fasl-cache-dir (mktemp))
                          leave-asdf-fasl-cache-dir-p)
  "Start a SBCL or CCL process that does not load config files, has a disabled
debugger, and does not print things unless told. Set ASDF_OUTPUT_TRANSLATIONS
for process to confine build artifacts to CLPM's cache. Use
~sandbox-augment-command~ to run the child in a sandbox."
  (let ((translations
          (format nil
                  "(:output-translations :ignore-inherited-configuration (t (\"~A\" :implementation :**/ :*.*.*)))"
                  asdf-fasl-cache-dir)))
    (ensure-directories-exist asdf-fasl-cache-dir)
    (make-instance
     'sub-lisp-process
     :impl impl
     :process-info (apply
                    #'uiop:launch-program
                    (sandbox-augment-command
                     (lisp-invocation-arglist
                      :implementation-type impl)
                     :read-write-pathnames (list asdf-fasl-cache-dir))
                    :input :stream
                    :output :stream
                    :error-output :interactive
                    (run-program-augment-env-args `(("ASDF_OUTPUT_TRANSLATIONS" . ,translations))))
     :asdf-fasl-cache-dir asdf-fasl-cache-dir
     :leave-asdf-fasl-cache-dir-p leave-asdf-fasl-cache-dir-p)))

(defun sub-lisp-alive-p (sub-lisp)
  (uiop:process-alive-p (sub-lisp-process-process-info sub-lisp)))

(defun stop-sub-lisp (sub-lisp &key signal urgent)
  (if signal
      (uiop:terminate-process (sub-lisp-process-process-info sub-lisp) :urgent urgent)
      (let ((quit-form (quit-form :implementation-type (sub-lisp-process-impl sub-lisp)))
            (input (sub-lisp-input sub-lisp)))
        (write-string quit-form input)
        (terpri input)))

  (prog1 (uiop:wait-process (sub-lisp-process-process-info sub-lisp))
    (unless (sub-lisp-process-leave-asdf-fasl-cache-dir-p sub-lisp)
      (uiop:delete-directory-tree (sub-lisp-process-asdf-fasl-cache-dir sub-lisp)
                                  :validate t
                                  :if-does-not-exist :ignore))))

;; (defun start-grovel-process ()
;;   "Start and return a process (using ~launch-sub-lisp~) that contains an
;; instance of SBCL or CCL with the groveling code loaded."
;;   (ensure-deps-system-in-cache!)
;;   (let* ((proc-info (launch-sub-lisp))
;;          (in-stream (uiop:process-info-input proc-info))
;;          (out-stream (uiop:process-info-output proc-info))
;;          (asd-pathname (clpm-cache-pathname
;;                         (list "deps-groveler-system"
;;                               (deps-version-string)
;;                               "clpm-deps.asd"))))
;;     (uiop:with-safe-io-syntax ()
;;       (format in-stream "(progn (require :sb-sprof) (values))~%")
;;       (format in-stream "(progn (require :asdf) (values))~%")
;;       ;; Nuke any existing asdf configuration
;;       (format in-stream "(progn (asdf:clear-configuration) (values))~%")
;;       ;; Make sure if ASDF reinitializes its configuration that it ignores
;;       ;; everything but the default (implementation specific) systems.
;;       (format in-stream "(progn (setf asdf:*default-source-registries* nil) (values))~%")
;;       (format in-stream "(progn (asdf:load-asd ~S) (values))~%" asd-pathname)
;;       (format in-stream "(progn (asdf:load-system :clpm-deps) (values))~%")
;;       (format in-stream "(progn (print \"ready\") (values))~%")
;;       (format in-stream "(progn (finish-output) (values))~%")
;;       (finish-output in-stream)
;;       ;; Wait until the process reports that it is ready.
;;       (loop
;;         :for val := (read out-stream)
;;         :until (equal val "ready")))
;;     proc-info))

;; (defun safe-load-asd-in-process (proc-info asd-pathname)
;;   "Load the .asd file at ~asd-pathname~ into the ~proc-info~'s lisp process."
;;   (let* ((in-stream (uiop:process-info-input proc-info))
;;          (out-stream (uiop:process-info-output proc-info)))
;;     (log:error "Starting ~S" asd-pathname)
;;     (setf *trace-output* *error-output*)
;;     (progn
;;      (uiop:with-safe-io-syntax ()
;;        (format in-stream "(progn (print (multiple-value-list (clpm-deps/main::safe-load-asd ~S))) (values))~%"
;;                asd-pathname)
;;        (format in-stream "(progn (finish-output) (values))~%")
;;        (finish-output in-stream)
;;        (values-list (read out-stream))))))

;; (defun determine-system-deps-in-process (proc-info system-name)
;;   "Query and return a list of the dependencies of ~system-name~."
;;   (let* ((in-stream (uiop:process-info-input proc-info))
;;          (out-stream (uiop:process-info-output proc-info)))
;;     (log:error "Starting ~S" system-name)
;;     (setf *trace-output* *error-output*)
;;     (progn
;;      (uiop:with-safe-io-syntax ()
;;        (format in-stream "(progn (print (clpm-deps/main::system-direct-deps (asdf:find-system ~S))) (values))~%"
;;                system-name)
;;        (format in-stream "(progn (finish-output) (values))~%")
;;        (finish-output in-stream)
;;        (read out-stream)))))

;; (defun determine-systems-from-file-in-process (proc-info system-pathname)
;;   "Return a list of systems defined in the .asd file located at
;; ~system-pathname~."
;;   (let* ((in-stream (uiop:process-info-input proc-info))
;;          (out-stream (uiop:process-info-output proc-info)))
;;     (log:error "Starting ~S" system-pathname)
;;     (setf *trace-output* *error-output*)
;;     (progn
;;      (uiop:with-safe-io-syntax ()
;;        (format in-stream "(progn (print (clpm-deps/main::determine-systems-from-file ~S)) (values))~%"
;;                system-pathname)
;;        (format in-stream "(progn (finish-output) (values))~%")
;;        (finish-output in-stream)
;;        (read out-stream)))))

;; (defun ensure-process-ready! (proc-info asd-pathname dependency-asd-files)
;;   "Ensures that the process has asd-pathname successfully loaded. Establishes a
;; ~add-asd-and-retry~ restart."
;;   (tagbody
;;    start
;;      (dolist (file (append dependency-asd-files (list asd-pathname)))
;;        (multiple-value-bind (success-p unknown-error-backtrace missing-system)
;;            (safe-load-asd-in-process proc-info file)
;;          (unless success-p
;;            (when unknown-error-backtrace
;;              ;; Uh-oh something happened that we don't know how to recover from
;;              ;; here. First, kill the process, then signal an error.
;;              (kill-groveler-process! proc-info asd-pathname)
;;              (error 'groveler-unknown-error
;;                     :backtrace unknown-error-backtrace))
;;            ;; We can provide a way to recover from this!
;;            (restart-case
;;                (error 'groveler-dependency-missing :system missing-system)
;;              (add-asd-and-retry (asd)
;;                :report "Add an asd file to preload and try again."
;;                :interactive read-asd-path
;;                (push asd dependency-asd-files)
;;                (go start))))))))

;; (defun kill-groveler-process! (proc-info asd-file)
;;   "Terminate the sublisp."
;;   (when (uiop:process-alive-p proc-info)
;;     (uiop:terminate-process proc-info :urgent t))
;;   (remhash (truename asd-file) *running-grovelers*)
;;   (uiop:wait-process proc-info))

;; (defun kill-groveler-process-for-file! (asd-file)
;;   "Kill the groveler process associated with ~asd-file~."
;;   (when-let ((proc-info (gethash (truename asd-file) *running-grovelers*)))
;;     (kill-groveler-process! proc-info asd-file)))

;; (defun get-groveler-process (asd-file dependency-asd-files)
;;   "Get the groveler process that has asd-file loaded and ready to be queried."
;;   (ensure-gethash (truename asd-file) *running-grovelers*
;;                   (aprog1 (start-grovel-process)
;;                     (ensure-process-ready! it asd-file dependency-asd-files))))

;; (defun grovel-system-info (asd-file system-name)
;;   "Grovel for the dependencies of ~system-name~ in ~asd-file~."
;;   (determine-system-deps-in-process (get-groveler-process asd-file nil) system-name))

;; (defun grovel-systems-in-file (asd-file)
;;   "Grovel for the systems defined in ~asd-file~."
;;   (determine-systems-from-file-in-process (get-groveler-process asd-file nil)
;;                                           (uiop:truename* asd-file)))

;; (defun exit-hook ()
;;   "Terminates all groveler processes."
;;   (maphash-keys (lambda (k)
;;                   (kill-groveler-process-for-file! k))
;;                 *running-grovelers*))

;; (add-exit-hook 'exit-hook)

;; (defun read-asd-path ()
;;   (format t "Enter a path to an asd file (not evaluated): ")
;;   (list (read-line)))
