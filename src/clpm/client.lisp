(uiop:define-package #:clpm/client
    (:use #:cl
          #:clpm-asdf
          #:clpm/archives
          #:clpm/data
          #:clpm/version
          #:flexi-streams)
  (:export #:clpm-client-lib-location
           #:ensure-client-written
           #:write-client))

(in-package #:clpm/client)

(defvar *client-tarball* nil
  "When dumping to an image, the client tarball is read in and stored in this
  variable.")

(defun read-client-tarball ()
  "Returns the client tarball as an octet vector."
  (with-output-to-sequence (out :element-type '(unsigned-byte 8))
    (with-open-file (in (asdf:output-file 'concatenate-source-deliver-asd-tarball-op :clpm-client)
                        :element-type '(unsigned-byte 8))
      (uiop:copy-stream-to-stream in out :element-type '(unsigned-byte 8)))))

(defun cache-client-tarball ()
  (setf *client-tarball* (read-client-tarball)))
(uiop:register-image-dump-hook 'cache-client-tarball)

(defun clpm-client-tarball-octets ()
  "Returns an octet vector representing the client tarball. Prefers the cache
  ~*client-tarball*~."
  (or *client-tarball*
      (read-client-tarball)))

(defun clpm-client-lib-location ()
  (clpm-data-pathname `("clpm-client" ,(concatenate 'string "clpm-client-" clpm/version::*version*))
                      :ensure-directory t))

(defun write-client (&key force)
  (when (and (not force)
             (uiop:probe-file* (clpm-client-lib-location)))
    (error "Client already exists!"))
  (unarchive 'gzipped-tar-archive (make-in-memory-input-stream (clpm-client-tarball-octets))
             (uiop:pathname-directory-pathname (clpm-client-lib-location))))

(defun ensure-client-written ()
  (unless (uiop:probe-file* (clpm-client-lib-location))
    (write-client)))
