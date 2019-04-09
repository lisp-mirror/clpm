;;;; Support for fetching files over HTTP using a curl executable.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/http-client/curl
    (:use #:cl
          #:clpm/http-client/defs
          #:clpm/utils)
  (:export #:curl-client))

(in-package #:clpm/http-client/curl)

(defclass curl-client (http-client)
  ((path
    :initarg :path
    :initform "curl"
    :accessor curl-path
    :documentation
    "The path to the curl program."))
  (:documentation
   "Describes an HTTP client that uses a curl executable.")
  (:default-initargs
   :priority 10))

(register-http-client :curl 'curl-client)

(defmethod http-client-available-p ((client curl-client))
  "Returns T iff the curl program exists at the path specified by the client and
its version can be successfully queried."
  (ignore-errors
   (zerop (nth-value 2 (uiop:run-program `(,(curl-path client) "--version")
                                         :ignore-error-status t)))))

(defmethod http-client-can-handle-url-p ((client curl-client) url)
  "Can handle every URL."
  t)

(defun header-pair-to-string (pair)
  "Convert a header name/value pair to a string of the form

\"Header-Name: Value\""
  (destructuring-bind (name . value) pair
    (check-type name string)
    (check-type value string)
    (format nil "~A: ~A" name value)))

(defmethod %fetch-url-to-stream ((client curl-client) url out-stream
                                 &key headers)
  (flet ((write-headers-to-stream (stream)
           (loop
             :for (key . value) :in headers
             :for name := (string-capitalize (symbol-name key))
             :do (format stream "~A: ~A~%" name value))
           (close stream)))
    (multiple-value-bind (out err exit-code)
        (uiop:run-program `(,(curl-path client)
                            ;; Add the requested headers. Pass them in on stdin
                            ;; to prevent them from being visible in the process
                            ;; list (in case any of them are authentication
                            ;; headers...)
                            "-H" "@-"
                            ;; Follow any redirects
                            "-L"
                            ;; Don't read any user config files
                            "-q"
                            ;; Bail out on non-200s
                            "--fail"
                            ;; Be quiet, but still show the error (if one occurs)
                            "--silent"
                            "--show-error"
                            ,(uri-to-string url))
                          :input #'write-headers-to-stream
                          :output out-stream
                          :error-output '(:string :stripped t)
                          :ignore-error-status t)
      (declare (ignore out))
      (unless (zerop exit-code)
        (error 'http-simple-fetch-error
               :format-control "Error from curl:~%~%~A"
               :format-arguments (list err)))
      t)))
