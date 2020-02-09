;;;; Requirement resolution definitions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/resolve/defs
    (:use #:cl
          #:clpm/log)
  (:export #:*sources*
           #:provided-system-p))

(in-package #:clpm/resolve/defs)

(setup-logger)

(defvar *sources* nil
  "The sources that can be used during resolution.")

(defparameter *sb-contribs*
  '("sb-aclrepl" "sb-bsd-sockets" "sb-capstone" "sb-cltl2" "sb-concurrency" "sb-cover"
    "sb-executable" "sb-gmp" "sb-grovel" "sb-introspect" "sb-md5" "sb-mpfr" "sb-posix"
    "sb-queue" "sb-rotate-byte" "sb-rt" "sb-simple-streams" "sb-sprof")
  "SBCL contrib systems.")

(defun provided-system-p (system-name)
  (or (equal "asdf" (asdf:primary-system-name system-name))
      (equal "uiop" (asdf:primary-system-name system-name))
      (member system-name *sb-contribs* :test #'equal)))
