;;;; Integration with CL+SSL
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/http-client/cl-plus-ssl
    (:use #:cl
          #:cl+ssl))

(in-package #:clpm/http-client/cl-plus-ssl)

(defun reinit-openssl ()
  "Hook to be run on image restore that tries to load and initialize OpenSSL."
  ;; Errors could be from libraries not being available, or being unable to
  ;; initialize because an incompatible OpenSSL version was loaded.
  (ignore-errors
   (reload)
   (ensure-initialized)))

(uiop:register-image-restore-hook 'reinit-openssl nil)
