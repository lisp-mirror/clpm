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

(cffi:define-foreign-library clpm-libcrypto-1
  (:windows (:or #+(and windows x86-64) "libcrypto-1_1-x64.dll"
                 #+(and windows x86) "libcrypto-1_1.dll"
                 "libeay32.dll"))
  (:openbsd "libcrypto.so")
  (:darwin (:or "/opt/local/lib/libcrypto.dylib" ;; MacPorts
                "/sw/lib/libcrypto.dylib"        ;; Fink
                "/usr/local/opt/openssl/lib/libcrypto.dylib" ;; Homebrew
                "/usr/local/lib/libcrypto.dylib" ;; personalized install
                "libcrypto.dylib"                ;; default system libcrypto, which may have insufficient crypto
                "/usr/lib/libcrypto.dylib"))
  ((and :unix (not :cygwin)) (:or "libcrypto.so.1.1"
                                  "libcrypto.so.1.0.2m"
                                  "libcrypto.so.1.0.2k"
                                  "libcrypto.so.1.0.2"
                                  "libcrypto.so.1.0.1l"
                                  "libcrypto.so.1.0.1j"
                                  "libcrypto.so.1.0.1f"
                                  "libcrypto.so.1.0.1e"
                                  "libcrypto.so.1.0.1"
                                  "libcrypto.so.1.0.0q"
                                  "libcrypto.so.1.0.0"
                                  "libcrypto.so.0.9.8ze"
                                  "libcrypto.so.0.9.8"
                                  "libcrypto.so.10"
                                  "libcrypto.so.4"
                                  "libcrypto.so"))
  (:cygwin (:or "cygcrypto-1.1.dll" "cygcrypto-1.0.0.dll")))

(cffi:use-foreign-library clpm-libcrypto-1)

(defvar *openssl-available-p* t
  "T if OpenSSL libraries are present and available in the image")

(defun maybe-load-openssl ()
  "Hook to be run on image restore that tries to load and initialize OpenSSL."
  ;; Errors could be from libraries not being available, or being unable to
  ;; initialize because an incompatible OpenSSL version was loaded.

  ;; Do not reload the libraries, deploy should have taken care of that already.
  (let ((*features* (list* ::cl+ssl-foreign-libs-already-loaded *features*)))
    (ignore-errors
     (reload)
     (ensure-initialized)
     (setf *openssl-available-p* t))))

(uiop:register-image-restore-hook 'maybe-load-openssl nil)
