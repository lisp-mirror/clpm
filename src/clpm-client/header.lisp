;;;; CLPM Client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/header
    (:use #:cl)
  (:export #:*version*))

;; If the location of this changes, the asd file must be updated!
(defvar *version* "0.0.9")
