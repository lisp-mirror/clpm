(uiop:define-package #:clpm/http-client/defs
    (:use #:cl)
  (:export #:fetch-error
           #:fetch-url-to-pathname
           #:fetch-url-to-stream
           #:simple-fetch-error))

(in-package #:clpm/http-client/defs)

(define-condition fetch-error (error)
  ())

(define-condition simple-fetch-error (fetch-error simple-error)
  ())

(defgeneric fetch-url-to-pathname (client url pathname
                                   &key headers))

(defgeneric fetch-url-to-stream (client url stream &key headers))
