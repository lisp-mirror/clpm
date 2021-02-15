;;;; Inferring things from the environment
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)

(defun clpm-exec-context ()
  "Returns the context used for `clpm exec` or NIL"
  (uiop:getenvp "CLPM_EXEC_CONTEXT"))

(defun inside-bundle-exec-p ()
  "Returns non-NIL iff we were spawned by a `clpm bundle exec` command."
  (uiop:getenvp "CLPM_BUNDLE_CLPMFILE"))

(defun bundle-command ()
  "Return a command (list of string) suitable for invoking the same CLPM
executable that spawned this bundle exec environment."
  (let ((bin (uiop:getenv "CLPM_BUNDLE_BIN")))
    (if bin
        (list bin)
        (let ((live-script (uiop:getenv "CLPM_BUNDLE_BIN_LIVE_SCRIPT"))
              (lisp-implementation (uiop:getenv "CLPM_BUNDLE_BIN_LISP_IMPLEMENTATION")))
          (cond
            ((equalp "sbcl" lisp-implementation)
             (list "sbcl" "--script" live-script))
            (t
             (error "Unknown lisp implementation")))))))
