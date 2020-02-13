;;;; Interface to clpm install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/install
    (:use #:cl
          #:clpm-client/bundle
          #:clpm-client/clpm)
  (:export #:clpm-install-system))

(in-package #:clpm-client/install)

(defun clpm-install-system (system &key
                                     no-deps-p
                                     (context *clpm-context* context-provided-p))
  (when (and context-provided-p (inside-bundle-exec-p))
    (warn "Inside a bundle. Ignoring the provided context."))
  (let ((output (run-clpm `(,@(when (inside-bundle-exec-p) '("bundle"))
                            "install"
                            ,@(unless (inside-bundle-exec-p) `("--context" ,context))
                            ,@(when no-deps-p '("--no-deps"))
                            "-y"
                            "--output=sexp"
                            ,(string-downcase (string system)))
                          :input nil
                          :output '(:string :stripped t))))
    (uiop:with-safe-io-syntax ()
      (read-from-string output))))
