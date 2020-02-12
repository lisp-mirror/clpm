;;;; Support for fetching files over HTTP using a curl executable.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/http-client/curl
    (:use #:cl
          #:clpm/http-client/defs
          #:clpm/utils
          #:clpm/version
          #:trivial-gray-streams)
  (:import-from #:fast-http)
  (:export #:curl-client))

(in-package #:clpm/http-client/curl)

(defclass curl-client (http-client)
  ((path
    :initarg :path
    :initform "curl"
    :accessor curl-path
    :documentation
    "The path to the curl program.")
   (buffer-size
    :initarg :buffer-size
    :initform 8192
    :accessor curl-buffer-size
    :documentation
    "How many bytes to read at once from curl's output."))
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

(defmethod %http-client-manages-streams-p ((client curl-client))
  nil)

(defun header-pair-to-string (pair)
  "Convert a header name/value pair to a string of the form

\"Header-Name: Value\""
  (destructuring-bind (name . value) pair
    (check-type name string)
    (check-type value string)
    (format nil "~A: ~A" name value)))

(defun process-response-and-get-body-stream (client proc response additional-headers)
  "Given a process info object representing a curl process, an http response
object, an alist of additional headers, return a stream containing the body of
the response.

First prints all additional headers to proc's stdin and closes it. Then, it
begins reading proc's stdout, feeding it into an HTTP parser for the provided
response object. After all the headers are processed, a stream is returned
containing the body of the response. When this stream is CLOSEd, the
corresponding curl process info is cleaned up."
  (let* ((out-stream (uiop:process-info-output proc))
         (in-stream (uiop:process-info-input proc))
         (body-started nil)
         (returned-stream nil)
         (parser (fast-http:make-parser
                  response
                  :body-callback
                  (lambda (data start end)
                    ;; There is a body to the response. Unfortunately
                    ;; we've read part of it, so we can't just hand the
                    ;; current stream to the caller... Instead, we make a
                    ;; concatenated stream that starts with the chunk
                    ;; we've read and then continues with the process'
                    ;; stdout.
                    (setf returned-stream
                          (make-instance 'process-stream
                                         :process-info proc
                                         :true-stream
                                         (make-concatenated-stream
                                          (flexi-streams:make-in-memory-input-stream
                                           data :start start :end end)
                                          out-stream)))
                    (setf body-started t)))))
    ;; Write all additional headers to curl's stdin
    (loop
      :for (key . value) :in additional-headers
      :for name := (string-downcase (symbol-name key))
      :do (format in-stream "~A: ~A~%" name value))
    (unless additional-headers
      ;; Curl on Windows seems to get rather upset if the stream is just
      ;; closed without anything being written to it, so just send a
      ;; blank line to make it happy.
      (format in-stream "~%"))
    ;; Close the stream to let curl know we're finished giving it headers.
    (close in-stream)

    ;; Now start pumping the output.
    (loop
      :with buffer := (make-array (curl-buffer-size client) :element-type '(unsigned-byte 8))
      :for pos := (read-sequence buffer out-stream)
      :unless (zerop pos)
        :do (funcall parser buffer :start 0 :end pos)
      :while (and (not (zerop pos))
                  (not body-started)))
    (unless body-started
      ;; If we get here, there's no body. Clean up after the process and
      ;; return a stream containing zero bytes.
      (close (uiop:process-info-error-output proc))
      (close (uiop:process-info-output proc))
      (close (uiop:process-info-input proc))
      (uiop:wait-process proc)
      (setf returned-stream
            (flexi-streams:make-in-memory-input-stream (make-array 0 :element-type '(unsigned-byte 8)))))
    returned-stream))

(defmethod %http-request ((client curl-client) url
                          &key additional-headers
                            want-stream)
  (let* ((response (fast-http:make-http-response))
         (stream
           (process-response-and-get-body-stream
            client
            (uiop:launch-program
             `(,(curl-path client)
               ;; Add the requested headers. Pass them in on stdin
               ;; to prevent them from being visible in the process
               ;; list (in case any of them are authentication
               ;; headers...)
               "-H" "@-"
               ;; Follow any redirects
               "-L"
               ;; Include headers in the output
               "-i"
               ;; Set the user agent
               "--user-agent"
               ,(format nil "CLPM/~A Curl"
                        (clpm-version))
               ;; Don't read any user config files
               "-q"
               ;; fast-http wasn't designed for HTTP 2, so force 1.1.
               "--http1.1"
               ;; Be quiet, but still show the error (if one occurs)
               "--silent"
               "--show-error"
               ,(uri-to-string url))
             :output :stream
             :error-output :stream
             :input :stream
             :element-type '(unsigned-byte 8))
            response
            additional-headers)))
    (values
     (if want-stream
         stream
         (flexi-streams:with-output-to-sequence (s :element-type '(unsigned-byte 8))
           (uiop:copy-stream-to-stream stream s
                                       :element-type '(unsigned-byte 8)
                                       :buffer-size 8192)))
     (fast-http:http-status response))))
