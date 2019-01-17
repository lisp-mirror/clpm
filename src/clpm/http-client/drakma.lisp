;;;; Support for fetching files over HTTP using a drakma
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/http-client/drakma
    (:use #:cl
          #:alexandria
          #:clpm/http-client/defs
          #:clpm/utils
          #:drakma)
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
  t)

(defmethod %fetch-url-to-stream ((client drakma-client) url out-stream
                                 &key headers)
  (multiple-value-bind (http-stream status-code)
      (http-request url :want-stream t
                        :verify :required
                        :decode-content t
                        :additional-headers headers)
    (assert (= 200 status-code))
    (copy-stream http-stream out-stream :element-type 'character :buffer-size 8192)
    (close http-stream)))
