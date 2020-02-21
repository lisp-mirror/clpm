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
