(uiop:define-package #:clpm/archives/chipz
    (:use #:cl
          #:chipz
          #:clpm/archives/defs))

(in-package #:clpm/archives/chipz)

(defmethod unarchive ((archive-type gzipped-archive) archive-stream destination-pathname)
  (call-next-method archive-type (chipz:make-decompressing-stream 'chipz:gzip archive-stream)
                    destination-pathname))
