;;;; Context Queries
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/context-queries
    (:use #:cl
          #:clpm/context
          #:clpm/session)
  (:export #:find-system-asd-pathname))

(in-package #:clpm/context-queries)

(defun find-system-asd-pathname (system-name &key context)
  (with-clpm-session ()
    (context-find-system-asd-pathname (get-context context) system-name)))
