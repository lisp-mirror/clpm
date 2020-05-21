;;;; Interacting with bundles
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)

(defun bundle-init (clpmfile &key asds)
  "Create a new clpmfile located at CLPMFILE. Adds all ASDS (must be relative to
CLPMFILE) to the new clpmfile."
  (setf clpmfile (uiop:ensure-absolute-pathname clpmfile))
  (with-clpm-proc (proc)
    (clpm-proc-print
     proc
     `(bundle-init :clpmfile ,clpmfile :asds ',asds))
    (clpm-proc-read proc)))
