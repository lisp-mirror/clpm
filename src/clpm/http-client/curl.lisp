(uiop:define-package #:clpm/http-client/curl
    (:use #:cl
          #:clpm/http-client/defs
          #:clpm/utils)
  (:export #:curl-client))

(in-package #:clpm/http-client/curl)

(defclass curl-client ()
  ())

(defun header-pair-to-string (pair)
  (destructuring-bind (name . value) pair
    (check-type name string)
    (check-type value string)
    (format nil "~A: ~A" name value)))

(defmethod fetch-url-to-stream ((client curl-client) url out-stream
                                &key headers)
  (flet ((write-headers-to-stream (stream)
           (loop
             :for (name . value) :in headers
             :do (format stream "~A: ~A~%" name value))
           (close stream)))
    (multiple-value-bind (out err exit-code)
        (uiop:run-program `("curl"
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
        (error 'simple-fetch-error
               :format-control "Error from curl:~%~%~A"
               :format-arguments (list err)))
      t)))

(defmethod fetch-url-to-pathname ((client curl-client) url pathname
                                  &key headers)
  (ensure-directories-exist pathname)
  (flet ((write-headers-to-stream (stream)
           (loop
             :for (name . value) :in headers
             :do (format stream "~A: ~A~%" name value))
           (close stream)))
    (multiple-value-bind (out err exit-code)
        (uiop:run-program `("curl"
                            ;; Add the requested headers. Pass them in on stdin
                            ;; to prevent them from being visibile in the
                            ;; process list (in case any of them are
                            ;; authentication headers...)
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
                          :output pathname
                          :error-output '(:string :stripped t)
                          :ignore-error-status t)
      (declare (ignore out))
      (unless (zerop exit-code)
        (error 'simple-fetch-error
               :format-control "Error from curl:~%~%~A"
               :format-arguments (list err)))
      t)))
