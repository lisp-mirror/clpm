;;;; CLPM Version
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/version
    (:use #:cl
          #:clpm-client/clpm)
  (:export #:*version*
           #:clpm-client-version
           #:clpm-version))

(in-package #:clpm-client/version)

;; If the location of this changes, the asd file must be updated!
(defvar *version* "0.2.1"
  "The client's version.")

(defun clpm-client-version ()
  "The client's version."
  *version*)

(defun clpm-version (&key verbose)
  "The version information for the clpm executable."
  (nth-value 0
             (run-clpm `("version" ,@(when verbose '("--verbose")))
                       :output '(:string :stripped t))))
