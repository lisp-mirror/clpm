;;;; Integration with CL+SSL
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/http-client/cl-plus-ssl
    (:use #:cl
          #:cl+ssl)
  (:import-from #:cffi)
  (:import-from #:cl+ssl
                #:libcrypto
                #:libssl
                #:libeay32)
  (:export #:*openssl-available-p*))

(in-package #:clpm/http-client/cl-plus-ssl)

(defvar *openssl-available-p* nil
  "T if OpenSSL libraries are present and available in the image")

(defun maybe-load-openssl ()
  "Hook to be run on image restore that tries to load and initialize OpenSSL."
  ;; Errors could be from libraries not being available, or being unable to
  ;; initialize because an incompatible OpenSSL version was loaded.
  (ignore-errors
   (reload)
   (ensure-initialized)
   (setf *openssl-available-p* t)))

(uiop:register-image-restore-hook 'maybe-load-openssl nil)
