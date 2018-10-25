(uiop:define-package #:clpm/data
    (:use #:cl)
  (:export #:clpm-data))

(in-package #:clpm/data)

(defun clpm-data (x &key ensure-directory)
  (uiop:resolve-absolute-location
   (list*
    (uiop:xdg-data-home "common-lisp" "clpm/")
    x)
   :ensure-directory ensure-directory))
