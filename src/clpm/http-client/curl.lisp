;;;; Support for fetching files over HTTP using a curl executable.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/http-client/curl
    (:use #:cl
          #:clpm/http-client/defs
          #:clpm/utils
          #:clpm/version)
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

(defun header-pair-to-string (pair)
  "Convert a header name/value pair to a string of the form

\"Header-Name: Value\""
  (destructuring-bind (name . value) pair
    (check-type name string)
    (check-type value string)
    (format nil "~A: ~A" name value)))

(defun make-curl-thread (client proc response additional-headers lock cv stream-callback)
  "Given a process info object representing a curl process, an http response
object, an alist of additional headers, a lock, a condition variable, and a
callback, return a lambda function that manages the curl process.

When invoked, the lambda first prints all additional headers to proc's stdin
and closes it. Then, it begins reading proc's stdout, feeding it into an HTTP
parser for the provided response object. After all the headers are processed,
the callback is called with a single argument, NIL if there is no body to the
response or a stream that can be read to get the response. After that, the
condition variable is notified."
  (lambda ()
    (unwind-protect
         (let* ((out-stream (uiop:process-info-output proc))
                (in-stream (uiop:process-info-input proc))
                (body-started nil)
                (parser (fast-http:make-parser
                         response
                         :body-callback
                         (lambda (data start end)
                           ;; There is a body to the response. Unfrotunately
                           ;; we've read part of it, so we can't just hand the
                           ;; current stream to the callback... Instead, we
                           ;; make a concatenated stream that starts with the
                           ;; chunk we've read and then continues with the
                           ;; process' stdout.
                           (funcall stream-callback
                                    (make-concatenated-stream
                                     (flexi-streams:make-in-memory-input-stream data :start start :end end)
                                     out-stream))
                           (setf body-started t)
                           (bt:with-lock-held (lock)
                             (bt:condition-notify cv))))))
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
             ;; If we get here, there's no body.
             (funcall stream-callback nil))
           (bt:with-lock-held (lock)
             (bt:condition-notify cv)))
      (uiop:wait-process proc)
      (close (uiop:process-info-input proc))
      (close (uiop:process-info-output proc))
      (close (uiop:process-info-error-output proc)))))

(defmethod %http-request ((client curl-client) url
                          &key additional-headers
                            want-stream)
  (let* ((lock (bt:make-lock))
         (cv (bt:make-condition-variable))
         (stream nil)
         (response (fast-http:make-http-response))
         (thread-lambda (make-curl-thread
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
                         additional-headers
                         lock
                         cv
                         (lambda (s)
                           (setf stream s)))))
    (bt:make-thread thread-lambda)
    (bt:with-lock-held (lock)
      (unless stream
        (bt:condition-wait cv lock)))
    (values
     (if want-stream
         (if stream
             stream
             (flexi-streams:make-in-memory-input-stream (make-array 0 :element-type '(unsigned-byte 8))))
         (if stream
             (flexi-streams:with-output-to-sequence (s :element-type '(unsigned-byte 8))
               (uiop:copy-stream-to-stream stream s
                                           :element-type '(unsigned-byte 8)
                                           :buffer-size 8192))
             nil))
     (fast-http:http-status response))))
