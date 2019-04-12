;;;; CLPM Client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/header
    (:use #:cl)
  (:export #:*version*))

(defvar *version* "0.0.1")
