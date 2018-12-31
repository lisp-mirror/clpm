(uiop:define-package #:clpm/archives/defs
    (:use #:cl)
  (:export #:gzipped-tar-archive
           #:tar-archive
           #:unarchive))

(in-package #:clpm/archives/defs)

(defclass tar-archive ()
  ())

(defclass gzipped-tar-archive (tar-archive)
  ())

(defgeneric unarchive (archive-type archive-stream destination-pathname))

(defmethod unarchive ((archive-type symbol) archive-stream destination-pathname)
  (unarchive (make-instance archive-type) archive-stream destination-pathname))
