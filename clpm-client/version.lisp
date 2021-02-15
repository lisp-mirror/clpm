;;;; CLPM Version
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)

;; If the location of this changes, the asd file must be updated!
(defvar *version* "0.3.6"
  "The client's version.")

(defun clpm-client-version ()
  "The client's version."
  *version*)

(defun clpm-version ()
  "The version information for the clpm executable."
  (with-clpm-proc (proc)
    (clpm-proc-print proc '(clpm-version))
    (clpm-proc-read proc)))
