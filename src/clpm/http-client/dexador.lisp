;;;; Support for fetching files over HTTP using dexador
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/http-client/dexador
    (:use #:cl
          #:alexandria
          #:clpm/http-client/cl-plus-ssl
          #:clpm/http-client/defs
          #:clpm/utils
          #:clpm/version)
  (:export #:dexador-client)
  (:import-from #:dexador)
  (:import-from #:puri))

(in-package #:clpm/http-client/dexador)

(defclass dexador-client (http-client)
  ()
  (:documentation
   "An HTTP client that uses Dexador")
  (:default-initargs
   ;; We like Lisp, so prefer Drakma or Dexador. Currenly prefer dexador more as
   ;; it does automatic connection pooling and reusing. Drakma may take the lead
   ;; again (it is more mature, after all), if I get around to implementing
   ;; reusing connections in CLPM. Dexador is also pretty nice in that it can
   ;; use HTTPS on Windows without OpenSSL...
   :priority -5))

(register-http-client :dexador 'dexador-client)

(defmethod http-client-available-p ((client dexador-client))
  t)

(defmethod http-client-can-handle-url-p ((client dexador-client) url)
  "If SSL support is loaded, we can handle every URL. Otherwise can only handle
  :http protocol."
  (or (eql (puri:uri-scheme url) :http)
      (featurep :windows)
      (and (not (featurep :dexador-no-ssl))
           *openssl-available-p*)))

(defmethod %http-client-manages-streams-p ((client dexador-client))
  t)

(defmethod %http-request ((client dexador-client) url
                          &key additional-headers
                            want-stream)
  (dex:get (uri-to-string url)
           :headers (list* (cons :user-agent (format nil "CLPM/~A Dexador"
                                                     (clpm-version)))
                           additional-headers)
           :keep-alive t
           :use-connection-pool t
           :force-binary t
           :want-stream want-stream))
