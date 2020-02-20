;;;; Calling the CLPM executable
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/clpm
    (:use #:cl
          #:clpm-client/cleanup
          #:clpm-client/env)
  (:import-from #:uiop
                #:launch-program
                #:run-program)
  (:export #:*clpm-dribble*
           #:*clpm-dribble-prefix*
           #:*clpm-executable*
           #:launch-clpm
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

(defvar *clpm-dribble* *error-output*
  "The stream where clpm's stderr is copied.")

(defvar *clpm-dribble-prefix* "CLPM> "
  "The prefix for lines to *CLPM-DRIBBLE*.")

(defvar *clpm-executable* nil
  "The command to execute CLPM. If the clpm executable is not on your PATH,
change this to be an absolute path to the executable.")

(defun compute-clpm-executable ()
  "If we're running inside a bundle exec environment, try to use the same clpm
executable that spawned us."
  (or *clpm-executable*
      (setf *clpm-executable*
            (if (clpm-inside-bundle-exec-p)
                (clpm-bundle-command)
                (list "clpm")))))

(uiop:register-image-restore-hook 'compute-clpm-executable)

(defun clear-clpm-executable ()
  (setf *clpm-executable* nil))
(uiop:register-image-dump-hook 'clear-clpm-executable)

(defun clear-clpm-executable-hooks ()
  (setf uiop:*image-restore-hook* (remove 'compute-clpm-executable uiop:*image-restore-hook*))
  (setf uiop:*image-dump-hook* (remove 'clear-clpm-executable uiop:*image-dump-hook*)))
(register-clpm-cleanup-hook 'clear-clpm-executable-hooks)

(defun ensure-list (arg)
  (if (listp arg)
      arg
      (list arg)))

(defun run-clpm (command &rest args &key ignore-error-status &allow-other-keys)
  "Run CLPM with ~command~. ~args~ are passed directly to ~uiop:run-program~."
  (multiple-value-bind (out err-out code)
      (apply #'run-program
             (append (ensure-list *clpm-executable*) command)
             :ignore-error-status t
             :error-output '(:string :stripped t)
             args)
    (when *clpm-dribble*
      (with-input-from-string (s err-out)
        (uiop:copy-stream-to-stream s *clpm-dribble* :linewise t :prefix *clpm-dribble-prefix*)))
    (unless (or ignore-error-status (zerop code))
      (error 'clpm-condition :error-output err-out :code code))
    (values out err-out code)))

(defun launch-clpm (command &rest args &key &allow-other-keys)
  "Launch CLPM with ~command~. ~args~ are passed directly to ~uiop:launch-program~."
  (apply #'launch-program
         (append (ensure-list *clpm-executable*) command)
         args))
