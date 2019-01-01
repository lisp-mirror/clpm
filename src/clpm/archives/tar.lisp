(uiop:define-package #:clpm/archives/tar
    (:use #:cl
          #:clpm/archives/defs))

(in-package #:clpm/archives/tar)

(defclass tar-client ()
  ((path
    :initarg :path
    :initform "tar"
    :accessor tar-path)))

(defmethod tar-client-available-p ((client tar-client))
  (ignore-errors
   (zerop (nth-value 2 (uiop:run-program `(,(tar-path client) "--version")
                                         :ignore-error-status t)))))

(register-tar-client :tar 'tar-client)

(defmethod unarchive-tar ((client tar-client) archive-stream destination-pathname)
  (ensure-directories-exist destination-pathname)
  (uiop:run-program `(,(tar-path client)
                      "-C" ,(uiop:native-namestring destination-pathname)
                      "-x")
                    :input archive-stream
                    :output :interactive
                    :error-output :interactive))
