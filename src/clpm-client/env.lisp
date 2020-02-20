;;;; Inferring things from the environment
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/env
    (:use #:cl)
  (:import-from #:uiop
                #:getenv)
  (:export #:clpm-bundle-command
           #:clpm-inside-bundle-exec-p))

(in-package #:clpm-client/env)

(defun clpm-inside-bundle-exec-p ()
  "Returns T iff we were spawned by a `clpm bundle exec` command."
  (not (null (getenv "CLPM_BUNDLE_CLPMFILE"))))

(defun clpm-bundle-command ()
  "Return a command (list of string) suitable for invoking the same CLPM
executable that spawned this bundle exec environment."
  (let ((bin (getenv "CLPM_BUNDLE_BIN")))
    (if bin
        (list bin)
        (let ((live-script (uiop:getenv "CLPM_BUNDLE_BIN_LIVE_SCRIPT"))
              (lisp-implementation (uiop:getenv "CLPM_BUNDLE_BIN_LISP_IMPLEMENTATION")))
          (cond
            ((equalp "sbcl" lisp-implementation)
             (list "sbcl" "--script" live-script))
            (t
             (error "Unknown lisp implementation")))))))
