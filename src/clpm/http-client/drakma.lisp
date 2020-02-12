;;;; Support for fetching files over HTTP using a drakma
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/http-client/drakma
    (:use #:cl
          #:alexandria
          #-:drakma-no-ssl
          #:clpm/http-client/cl-plus-ssl
          #:clpm/http-client/defs
          #:clpm/utils
          #:clpm/version
          #:drakma
          #:puri)
  (:shadowing-import-from #:drakma
                          #:http-request)
  (:export #:drakma-client))

(in-package #:clpm/http-client/drakma)

(defclass drakma-client (http-client)
  ()
  (:documentation
   "Describes an HTTP client that uses drakma.")
  (:default-initargs
   ;; we like lisp, so if drakma is available, prefer to use it
   :priority 0))

(register-http-client :drakma 'drakma-client)

(defmethod http-client-available-p ((client drakma-client))
  (if (uiop:featurep :drakma-no-ssl)
      t
      *openssl-available-p*))

(defmethod http-client-can-handle-url-p ((client drakma-client) url)
  "If SSL support is loaded, we can handle every URL. Otherwise can only handle
  :http protocol."
  (or (not (featurep :drakma-no-ssl))
      (eql (uri-scheme url) :http)))

(defmethod %http-client-manages-streams-p ((client drakma-client))
  nil)

(defmethod %http-request ((client drakma-client) url
                          &key additional-headers
                            want-stream)
  (http-request url :additional-headers additional-headers
                    :want-stream want-stream
                    :force-binary t
                    :verify #+:drakma-no-ssl nil #-:drakma-no-ssl :required
                    :user-agent (format nil "CLPM/~A Drakma/~A"
                                        (clpm-version)
                                        drakma:*drakma-version*)))
