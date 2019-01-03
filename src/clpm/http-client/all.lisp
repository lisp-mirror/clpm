;;;; Support for fetching resources over HTTP.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; COPYING for license information.

(uiop:define-package #:clpm/http-client/all
    (:use #:cl
          #:clpm/http-client/curl
          #:clpm/http-client/defs)
  (:reexport #:clpm/http-client/defs))

(in-package #:clpm/http-client/all)
