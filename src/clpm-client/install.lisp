;;;; Interface to clpm install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/install
    (:use #:cl
          #:clpm-client/bundle
          #:clpm-client/clpm
          #:clpm-client/context)
  (:export #:clpm-install-system))

(in-package #:clpm-client/install)

(defun clpm-install-system (system &key
                                     no-deps-p
                                     (context *clpm-context*))
  "Install a system in the given context."
  (assert (not (clpm-inside-bundle-exec-p))
          nil "Cannot currently run CLPM-INSTALL-SYSTEM inside a bundle exec command")
  (let ((output (run-clpm `("install"
                            "--context" ,context
                            ,@(when no-deps-p '("--no-deps"))
                            "-y"
                            "--output=sexp"
                            ,(string-downcase (string system)))
                          :input nil
                          :output '(:string :stripped t))))
    (uiop:with-safe-io-syntax ()
      (read-from-string output))))
