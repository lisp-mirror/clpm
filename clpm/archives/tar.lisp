;;;; Support for extracting archives using a tar executable.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/archives/tar
    (:use #:cl
          #:clpm/archives/defs))

(in-package #:clpm/archives/tar)

(defclass tar-client ()
  ((path
    :initarg :path
    :initform "tar"
    :accessor tar-path
    :documentation
    "The path to the tar program."))
  (:documentation
   "Describes an archive extractor that uses a tar executable."))

(register-tar-client :tar 'tar-client)

(defmethod tar-client-available-p ((client tar-client))
  "Returns T iff the tar program exists at the path specified by the client and
its version can be successfully queried."
  (ignore-errors
   (zerop (nth-value 2 (uiop:run-program `(,(tar-path client) "--version")
                                         :ignore-error-status t)))))

(defmethod unarchive-tar ((client tar-client) archive-stream destination-pathname
                          &key strip-components)
  "Ensures ~destination-pathname~ exists and the executes the tar program to
extract the contents of ~archive-stream~ to ~destination-pathname~."
  (ensure-directories-exist destination-pathname)
  (uiop:run-program `(,(tar-path client)
                      "-C" ,(uiop:native-namestring destination-pathname)
                      ,@(when strip-components
                          (list (format nil "--strip-components=~d" strip-components)))
                      "-x" "-f" "-")
                    ;; Ignore errors on windows because a number of Quicklisp
                    ;; tarballs have symlinks in them which Windows does not
                    ;; like.
                    :ignore-error-status #+win32 t #-win32 nil
                    :input `(,archive-stream :element-type (unsigned-byte 8))
                    :output nil
                    :error-output :interactive))
