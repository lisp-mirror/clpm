(uiop:define-package #:clpm/archives/defs
    (:use #:cl)
  (:export #:unarchive))

(in-package #:clpm/archives/defs)

(defgeneric unarchive (archive-type archive-stream destination-pathname))
