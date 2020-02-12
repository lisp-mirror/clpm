;;;; Support for fetching files over HTTP.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/http-client
    (:use #:cl
          #:clpm/http-client/all
          #:alexandria
          #:clpm/config
          #:clpm/log
          #:iterate)
  (:import-from #:flexi-streams)
  (:import-from #:puri)
  (:export #:ensure-file-fetched
           #:fetch-url
           #:http-request))

(in-package #:clpm/http-client)

(setup-logger)

(defun day-name (day-of-week)
  (ecase day-of-week
    (0
     "Mon")
    (1
     "Tue")
    (2
     "Wed")
    (3
     "Thu")
    (4
     "Fri")
    (5
     "Sat")
    (6
     "Sun")))

(defun month-name (month)
  (ecase month
    (1 "Jan")
    (2 "Feb")
    (3 "Mar")
    (4 "Apr")
    (5 "May")
    (6 "Jun")
    (7 "Jul")
    (8 "Aug")
    (9 "Sep")
    (10 "Oct")
    (11 "Nov")
    (12 "Dec")))

(defun universal-time-to-http-date (universal-time)
  "Given a universal-time return the same date as a string suitable for HTTP
headers."
  (multiple-value-bind (second minute hour date month year day-of-week)
      ;; Times in HTTP Headers are always GMT
      (decode-universal-time universal-time 0)
    (format nil "~A, ~2,'0D ~A ~A ~2,'0D:~2,'0D:~2,'0D GMT"
            (day-name day-of-week) date (month-name month) year hour minute second)))

(defun get-additional-headers-for-hostname (hostname scheme)
  "Given a ~hostname~ and ~scheme~ used (~:http~ or ~:https~), return an alist
mapping header names (as keywords) to values that should be included as part of
the request."
  (let ((header-names (config-table-keys :http :headers hostname)))
    (iter
      (for header-name :in header-names)
      (when (or (eql scheme :https)
                (not (config-value :http :headers hostname header-name :secure-only-p)))
        (let ((value (config-value :http :headers hostname header-name :value))
              (exec (config-value :http :headers hostname header-name :exec))
              (contents (config-value :http :headers hostname header-name :contents)))
          (cond
            (value
             (collect (cons header-name value)))
            (exec
             (collect (cons header-name (uiop:run-program (list (namestring exec))
                                                          :output '(:string :stripped t)))))
            (contents
             (collect (cons header-name (uiop:read-file-string contents))))))))))

(defun fetch-url (url)
  "Given a URL, return a string with the contents of the file located at ~url~."
  (let ((url (puri:parse-uri url)))
    (log:debug "Fetching ~A" url)
    (babel:octets-to-string
     (http-request url :additional-headers (get-additional-headers-for-hostname
                                            (puri:uri-host url)
                                            (puri:uri-scheme url))))))

(defun ensure-file-fetched (pathname url &key force hint)
  "Given a pathname, make sure it exists. If it does not exist, fetch it from
URL (string or puri URI).

If force is non-NIL, fetches the file without sending an If-Modified-Since
header.

HINT provides some hints to use when fetching the file. It supports:

+ :IMMUTABLE :: If the destination file is present, no HTTP requests are sent at
all.

Returns T if the contents of PATHNAME were modified, NIL otherwise."
  (unless (and (eql hint :immutable)
               (probe-file pathname))
    (setf url (puri:parse-uri url))
    (log:debug "Fetching ~A" url)
    ;; Base the tmp pathname off the pathname pathname to try and ensure that
    ;; they are on the same filesystem.
    (let ((tmp-pathname (uiop:tmpize-pathname pathname))
          (additional-headers (get-additional-headers-for-hostname
                               (puri:uri-host url)
                               (puri:uri-scheme url))))
      (when (and (not force)
                 (probe-file pathname))
        (push (cons :if-modified-since (universal-time-to-http-date (file-write-date pathname)))
              additional-headers))
      (ensure-directories-exist pathname)
      (unwind-protect
           (multiple-value-bind (http-stream status-code)
               (http-request url
                             :want-stream t
                             :additional-headers additional-headers)
             (unwind-protect
                  (progn
                    (log:debug "Status code: ~S" status-code)
                    (case status-code
                      (200
                       (with-open-file (file-stream tmp-pathname :direction :output
                                                                 :if-exists :supersede
                                                                 :element-type '(unsigned-byte 8))
                         ;; Save the data to the file.
                         (copy-stream http-stream file-stream
                                      :element-type '(unsigned-byte 8)
                                      :buffer-size 8192))
                       (rename-file tmp-pathname pathname)
                       t)
                      (304
                       ;; No changes
                       nil)
                      (t
                       (error "Can't handle HTTP code ~A" status-code))))
               (unless (http-client-manages-streams-p url)
                 (close http-stream))))
        (when (probe-file tmp-pathname)
          (delete-file tmp-pathname))))))
