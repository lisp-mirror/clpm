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
  (:import-from #:uiop
                #:read-file-form
                #:with-safe-io-syntax)
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

(defgeneric canonicalize-header-value (header-value)
  (:documentation "Given a ~header-value~ from a config file, return a string
that contains the value of the header that should be sent as part of the HTTP
request."))

(defmethod canonicalize-header-value ((header-value string))
  "A string simply returns itself."
  header-value)

#-os-windows
(defun pathname-executable-p (pathname)
  "On non-Windows platforms, a file is executable if its execute bit is set."
  (let* ((stat (sb-posix:stat pathname))
         (mode (sb-posix:stat-mode stat)))
    (or (not (zerop (boole boole-and mode sb-posix:s-ixoth)))
        (not (zerop (boole boole-and mode sb-posix:s-ixgrp)))
        (not (zerop (boole boole-and mode sb-posix:s-ixusr))))))

#+os-windows
(defun pathname-executable-p (pathname)
  "On windows, a file is executable if its type is exe."
  (equalp "exe" (pathname-type pathname)))

(defmethod canonicalize-header-value ((header-value pathname))
  "If given a pathname to an executable file, run it and use the value it prints
to stdout as the value. Otherwise, read the contents of the file and use that."
  (if (pathname-executable-p header-value)
      ;; executable
      (uiop:run-program (namestring header-value) :output '(:string :stripped t))
      ;; not executable
      (uiop:read-file-string header-value)))

(defun get-additional-headers-for-hostname (hostname scheme)
  "Given a ~hostname~ and ~scheme~ used (~:http~ or ~:https~), return an alist
mapping header names (as keywords) to values that should be included as part of
the request."
  (let ((headers-ht (config-value :http :headers hostname)))
    (when headers-ht
      (iter
        (for (key value) :in-hashtable headers-ht)
        (when (or (eql scheme :https)
                  (not (gethash :secure-only-p value)))
          (collect (cons key (canonicalize-header-value (gethash :value value)))))))))

(defun fetch-url (url)
  "Given a URL, return a string with the contents of the file located at ~url~."
  (let ((url (puri:parse-uri url)))
    (log:debug "Fetching ~A" url)
    (babel:octets-to-string
     (http-request url :additional-headers (get-additional-headers-for-hostname
                                            (puri:uri-host url)
                                            (puri:uri-scheme url))))))

(defun ensure-file-fetched (pathname url &key force)
  "Given a pathname, make sure it exists. If it does not exist, fetch it from
URL (string or puri URI).

If force is non-NIL, fetches the file without sending an If-Modified-Since
header.

Returns T if the contents of PATHNAME were modified, NIL otherwise."
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
         (progn
           (multiple-value-bind (http-stream status-code)
               (http-request url
                             :want-stream t
                             :additional-headers additional-headers)
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
                (error "Can't handle HTTP code ~A" status-code)))))
      (when (probe-file tmp-pathname)
        (delete-file tmp-pathname)))))
