(uiop:define-package #:clpm/http-client/all
    (:use #:cl
          #:clpm/http-client/curl
          #:clpm/http-client/defs)
  (:reexport #:clpm/http-client/defs))

(in-package #:clpm/http-client/all)
