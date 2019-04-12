;;;; Calling the CLPM executable
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/clpm
    (:use #:cl
          ;; Everything must depend on header so that it comes first in the
          ;; concatenated file.
          #:clpm-client/header)
  (:import-from #:uiop
                #:run-program)
  (:export #:*clpm-executable*
           #:run-clpm))

(in-package #:clpm-client/clpm)

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

(defun run-clpm (command &rest args &key &allow-other-keys)
  "Run CLPM with ~command~. ~args~ are passed directly to ~uiop:run-program~."
  (apply #'run-program
         (list* *clpm-executable* command)
         args))
