;;;; Interface for caching the clpm-client system location
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/client
    (:use #:cl
          #:clpm/archives
          #:clpm/data
          #:clpm/version)
  (:import-from #:flexi-streams
                #:make-in-memory-input-stream)
  (:export #:*clpm-client-asd-pathname*
           #:*clpm-client-concatenated-source*
           #:*clpm-client-tarball-contents*
           #:client-asd-pathname))

(in-package #:clpm/client)

(defvar *clpm-client-tarball-contents* nil)
(defparameter *clpm-client-concatenated-source*
  (uiop:read-file-string (asdf:output-file 'asdf:concatenate-source-op :clpm-client)))

(defun unpack-client ()
  (let ((dir-pathname (clpm-data-pathname `("client"
                                            ,(concatenate 'string "clpm-client-" (clpm-version)))
                                          :ensure-directory t)))
    (unless (probe-file dir-pathname)
      (unarchive 'gzipped-tar-archive (make-in-memory-input-stream *clpm-client-tarball-contents*)
                 dir-pathname :strip-components 1))
    (merge-pathnames "clpm-client.asd" dir-pathname)))

(defun client-asd-pathname ()
  "Return the pathname to the client .asd file."
  (if *clpm-client-tarball-contents*
      (unpack-client)
      (asdf:system-source-file :clpm-client)))
