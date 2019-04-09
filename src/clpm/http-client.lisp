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
           #:fetch-url))

(in-package #:clpm/http-client)

(setup-logger)

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
        (unless (or (eql scheme :https)
                    (not (gethash :secure-only-p value)))
          (collect (cons key (canonicalize-header-value (gethash :value value)))))))))

(defun fetch-url (url)
  "Given a URL, return a string with the contents of the file located at ~url~."
  (let ((url (puri:parse-uri url)))
    (log:debug "Fetching ~A" url)
    (babel:octets-to-string
     (flexi-streams:with-output-to-sequence (s :element-type '(unsigned-byte 8))
       (fetch-url-to-stream url s
                            :headers (get-additional-headers-for-hostname (puri:uri-host url)
                                                                          (puri:uri-scheme url)))))))

(defun ensure-file-fetched (pathname url &key refresh-time)
  "Given a pathname, make sure it exists. If it does not exist, fetch it from
URL (string or puri URI).

If refresh-time is non-NIL, fetches the file if it already exists and it is
older than refresh-time in seconds."
  (setf url (puri:parse-uri url))
  (log:debug "Fetching ~A" url)
  (when (or (not (probe-file pathname))
            (and refresh-time
                 (> (- (get-universal-time) (file-write-date pathname))
                    refresh-time)))
    ;; Base the tmp pathname off the pathname pathname to try and ensure that
    ;; they are on the same filesystem.
    (let ((tmp-pathname (uiop:tmpize-pathname pathname)))
      (ensure-directories-exist pathname)
      (unwind-protect
           (progn
             (with-open-file (file-stream tmp-pathname :direction :output
                                                       :if-exists :supersede
                                                       :element-type '(unsigned-byte 8))
               (fetch-url-to-stream url file-stream
                                    :headers (get-additional-headers-for-hostname
                                              (puri:uri-host url)
                                              (puri:uri-scheme url))))
             (rename-file tmp-pathname pathname))
          (when (probe-file tmp-pathname)
            (delete-file tmp-pathname))))))
