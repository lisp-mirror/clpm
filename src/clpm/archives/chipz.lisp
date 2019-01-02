;;;; Support for inflating archives using the Chipz Common Lisp library.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; COPYING for license information.

(uiop:define-package #:clpm/archives/chipz
    (:use #:cl
          #:chipz
          #:clpm/archives/defs))

(in-package #:clpm/archives/chipz)

(defmethod stream-element-type ((stream chipz::decompressing-stream))
  "Chipz's decompressing streams have unsigned bytes as the elements. This
should be implemented in chipz itself."

  '(unsigned-byte 8))

(defmethod unarchive :around ((archive-type gzipped-archive) archive-stream destination-pathname)
  "If an archive is gzipped, make a decompressing stream using chipz and then
call the next method with that stream instead of ~archive-stream~."
  (with-open-stream (s (chipz:make-decompressing-stream 'chipz:gzip archive-stream))
    (call-next-method archive-type s destination-pathname)))
