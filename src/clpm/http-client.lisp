(uiop:define-package #:clpm/http-client
    (:use #:cl
          #:clpm/http-client/curl
          #:clpm/http-client/defs
          #:alexandria
          #:clpm/config
          #:iterate)
  (:import-from #:uiop
                #:read-file-form
                #:with-safe-io-syntax)
  (:export #:ensure-file-fetched
           #:fetch-url))

(in-package #:clpm/http-client)

(defgeneric canonicalize-header-value (header-value))

(defmethod canonicalize-header-value ((header-value string))
  header-value)

#-os-windows
(defun pathname-executable-p (pathname)
  (let* ((stat (sb-posix:stat pathname))
         (mode (sb-posix:stat-mode stat)))
    (or (not (zerop (boole boole-and mode sb-posix:s-ixoth)))
        (not (zerop (boole boole-and mode sb-posix:s-ixgrp)))
        (not (zerop (boole boole-and mode sb-posix:s-ixusr))))))

#+os-windows
(defun pathname-executable-p (pathname)
  (equalp "exe" (pathname-type pathname)))

(defmethod canonicalize-header-value ((header-value pathname))
  (if (pathname-executable-p header-value)
      ;; executable
      (uiop:run-program (namestring header-value) :output '(:string :stripped t))
      ;; not executable
      (uiop:read-file-string header-value)))

(defun host-spec-matches-p (spec hostname scheme)
  (destructuring-bind (host-string &key secure-only-p)
      (ensure-list spec)
    (and (equalp host-string hostname)
         (or (not secure-only-p)
             (eql scheme :https)))))

(defun get-additional-headers-for-hostname (hostname scheme)
  ;; Read the header file if it exists.
  (let ((header-pathname (clpm-config '("headers.conf")))
        (out nil))
    (when header-pathname
      (let* ((host-alist (with-safe-io-syntax ()
                           (read-file-form header-pathname))))
        (iter
          (for (host-spec . host-headers) :in host-alist)
          (when (host-spec-matches-p host-spec hostname scheme)
            (iter
              (for (header-name . header-value) :in host-headers)
              (setf (getf out header-name) (canonicalize-header-value header-value)))))))))

(defun fetch-url (url)
  (let ((url (puri:parse-uri url)))
    (with-output-to-string (s)
      (fetch-url-to-stream (make-instance 'curl-client) url s
                           :headers (get-additional-headers-for-hostname (puri:uri-host url)
                                                                         (puri:uri-scheme url))))))

(defun ensure-file-fetched (pathname url &key refresh-time)
  "Given a pathname, make sure it exists. If it does not exist, fetch it from
URL (string or puri URI).

If refresh-time is non-NIL, fetches the file if it already exists and it is
older than refresh-time in seconds."
  (setf url (puri:parse-uri url))
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
               (fetch-url-to-stream (make-instance 'curl-client) url file-stream
                                    :headers (get-additional-headers-for-hostname
                                              (puri:uri-host url)
                                              (puri:uri-scheme url))))
             (rename-file tmp-pathname pathname))
          (when (probe-file tmp-pathname)
            (delete-file tmp-pathname))))))
