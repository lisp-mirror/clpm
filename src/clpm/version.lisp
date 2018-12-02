(uiop:define-package #:clpm/version
    (:use #:cl)
  (:export #:clpm-version))

(in-package #:clpm/version)


;;; Application Version

;; NOTE: If this form changes position in this file you *must* update clpm.asd as
;; well.
(defparameter *version* "0.0.3")

(defparameter *git-version* "")

(defun clpm-version ()
  (if (string= "" *git-version*)
      *version*
      (concatenate 'string *version* "+" *git-version*)))
