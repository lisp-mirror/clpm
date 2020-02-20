;;;; CLPM Context
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/context
    (:use #:cl
          #:clpm-client/clpm)
  (:export #:*clpm-context*
           #:clpm-context-asd-directories
           #:clpm-context-asd-pathnames
           #:clpm-context-find-system))

(in-package #:clpm-client/context)

(defvar *clpm-context* "default"
  "The context to operate in when not operating in a bundle.")

(defun clpm-context-asd-pathnames (&optional (context *clpm-context*))
  "Given a context name, return a list of pathnames to .asd files installed in
that context."
  (let ((pathnames-string (ignore-errors
                           (run-clpm `("context" "pathnames" "--output=sexp" ,context)
                                     :output '(:string :stripped t)))))
    (when pathnames-string
      (uiop:with-safe-io-syntax () (read-from-string pathnames-string)))))

(defun clpm-context-asd-directories (&optional (context *clpm-context*))
  "Return the directories containing the .asd files installed in CONTEXT."
  (remove-duplicates (mapcar #'pathname-directory (clpm-context-asd-pathnames context))
                     :test #'uiop:pathname-equal))

(defun clpm-context-find-system (system-name &key (context *clpm-context*))
  "Find the pathname to a system in the given context."
  (multiple-value-bind (output error-output code)
      (run-clpm `("context" "find"
                            "--context" ,context
                            "--output=sexp"
                            ,system-name)
                :output '(:string :stripped t)
                :ignore-error-status t)
    (declare (ignore error-output))
    (when (zerop code)
      (uiop:with-safe-io-syntax ()
        (read-from-string output)))))
