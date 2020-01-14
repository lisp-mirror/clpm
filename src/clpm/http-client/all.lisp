;;;; Support for fetching resources over HTTP.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/http-client/all
    (:use #:cl
          #+:clpm-curl #:clpm/http-client/curl
          #:clpm/http-client/defs
          #+:clpm-drakma #:clpm/http-client/drakma)
  (:reexport #:clpm/http-client/defs))

(in-package #:clpm/http-client/all)
