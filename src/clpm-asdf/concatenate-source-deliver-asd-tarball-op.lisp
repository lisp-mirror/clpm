;;;; Support for concatenating source and asd files for a package inferred
;;;; system into a tarball.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-asdf/concatenate-source-deliver-asd-tarball-op
    (:use #:cl
          #:archive
          #:asdf
          #:chipz
          #:clpm-asdf/concatenate-source-deliver-asd-op
          #:flexi-streams
          #:salza2)
  (:export #:concatenate-source-deliver-asd-tarball-op))

(in-package #:clpm-asdf/concatenate-source-deliver-asd-tarball-op)

(defclass concatenate-source-deliver-asd-tarball-op (selfward-operation)
  ((selfward-operation
    :initform 'concatenate-source-deliver-asd-op)))

(defmethod input-files ((op concatenate-source-deliver-asd-tarball-op) (s system))
  (output-files (selfward-operation op) s))

(defmethod output-files ((op concatenate-source-deliver-asd-tarball-op) (c system))
  (values (list (uiop:subpathname (component-pathname c)
                                  (asdf::component-build-pathname c)
                                  :type "tar.gz"))
          t))

(defmethod perform ((op concatenate-source-deliver-asd-tarball-op) (c system))
  (with-open-stream (s (make-in-memory-output-stream))
    (let ((archive (open-archive 'tar-archive s :direction :output))
          (*default-pathname-defaults* (uiop:pathname-directory-pathname (output-file op c))))
      (unwind-protect
           (progn
             (dolist (pn (output-files (selfward-operation op) c))
               (let ((entry (create-entry-from-pathname archive (enough-namestring pn))))
                 (write-entry-to-archive archive entry)))
             (finalize-archive archive))
        (close-archive archive)))
    (with-open-file (out (output-file op c) :direction :output
                                            :element-type '(unsigned-byte 8)
                                            :if-exists :supersede)
      (gzip-stream (make-in-memory-input-stream (flexi-streams:get-output-stream-sequence s)) out))))
