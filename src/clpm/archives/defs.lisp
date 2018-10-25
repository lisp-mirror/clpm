(uiop:define-package #:clpm/archives/defs
    (:use #:cl)
  (:export #:unarchive))

(in-package #:clpm/archives/defs)

(defgeneric unarchive (archive-type archive-pathname destination-pathname))
