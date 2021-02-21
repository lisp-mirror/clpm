;;;; Interface for caching the clpm-client system location
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/client
    (:use #:cl
          #:clpm/archives
          #:clpm/data
          #:clpm/version)
  ;; TODO: GROSS, GROSS, GROSS. Need to find a way to get rid of this.
  (:import-from #:clpm/client-helper)
  (:import-from #:alexandria)
  (:import-from #:flexi-streams
                #:make-in-memory-input-stream)
  (:export #:*clpm-client-concatenated-source*
           #:*clpm-client-tarball-contents*
           #:client-asd-pathname
           #:client-user-data-pathname
           #:install-client-to-user-data))

(in-package #:clpm/client)

(defparameter *clpm-client-tarball-contents*
  (alexandria:read-file-into-byte-vector (asdf:output-file 'clpm-asdf:build-clpm-client-tarball-op :clpm)))

(defparameter *clpm-client-concatenated-source*
  (uiop:read-file-string (asdf:output-file 'asdf:concatenate-source-op :clpm-client)))

(defun client-installed-pathname ()
  "Return the pathname where we would expect to find the clpm client if it were
installed in the expected place relative to the clpm executable."
  (let ((clpm-pathname sb-ext:*runtime-pathname*))
    (merge-pathnames (make-pathname :directory '(:relative :up "share" "clpm" "client")
                                    :name "clpm-client"
                                    :type "asd")
                     (uiop:pathname-directory-pathname clpm-pathname))))

(defun client-user-data-pathname ()
  "Return the pathname to the clpm-client.asd file in the user's data
directory."
  (clpm-data-pathname `("client"
                        ,(concatenate 'string "clpm-client-" (clpm-version))
                        "clpm-client.asd")))

(defun install-client-to-user-data ()
  "Install the client to the user data folder if it doesn't already exist."
  (let* ((asd-pathname (client-user-data-pathname))
         (dir-pathname (uiop:pathname-directory-pathname asd-pathname)))
    (unless (probe-file asd-pathname)
      (unarchive 'gzipped-tar-archive (make-in-memory-input-stream *clpm-client-tarball-contents*)
                  dir-pathname :strip-components 1))))

(defun client-asd-pathname ()
  "Return the pathname to the client .asd file. First tries to find the client
relative the the clpm executable. If that fails, it looks to the user data dir
to see if the client is installed there."
  (or (probe-file (client-installed-pathname))
      (probe-file (client-user-data-pathname))))
