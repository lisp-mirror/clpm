;;;; Calling the CLPM executable
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/clpm
    (:use #:cl)
  (:import-from #:uiop
                #:run-program)
  (:export #:*clpm-context*
           #:*clpm-executable*
           #:run-clpm))

(in-package #:clpm-client/clpm)

(define-condition clpm-condition ()
  ((error-output
    :initarg :error-output
    :reader clpm-condition-error-output)
   (code
    :initarg :code
    :reader clpm-condition-code))
  (:report (lambda (c s)
             (format s "CLPM exited with code ~D and error output:~%~A"
                     (clpm-condition-code c)
                     (clpm-condition-error-output c)))))

(defvar *clpm-context* "default"
  "The context to operate in when not operating in a bundle.")

(defvar *clpm-executable* nil
  "The command to execute CLPM. If the clpm executable is not on your PATH,
change this to be an absolute path to the executable.")

(defun compute-clpm-executable ()
  "If we're running inside a bundle exec environment, try to use the same clpm
executable that spawned us."
  (setf *clpm-executable*
        (or (uiop:getenv "CLPM_BUNDLE_BIN_PATH")
            "clpm")))

(uiop:register-image-restore-hook 'compute-clpm-executable)

(defun run-clpm (command &rest args &key ignore-error-status &allow-other-keys)
  "Run CLPM with ~command~. ~args~ are passed directly to ~uiop:run-program~."
  (multiple-value-bind (out err-out code)
      (apply #'run-program
             (list* *clpm-executable* command)
             :ignore-error-status t
             :error-output '(:string :stripped t)
             args)
    (unless (or ignore-error-status (zerop code))
      (error 'clpm-condition :error-output err-out :code code))
    (values out err-out code)))
