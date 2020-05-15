;;;; Interface for caching the clpm-client system location
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/client
    (:use #:cl)
  (:export #:*clpm-client-asd-pathname*
           #:client-asd-pathname))

(in-package #:clpm/client)

(defvar *clpm-client-asd-pathname*
  (asdf:system-relative-pathname :clpm "client/clpm-client.asd"))

(defun client-asd-pathname ()
  "Return the pathname to the client .asd file."
  *clpm-client-asd-pathname*)
