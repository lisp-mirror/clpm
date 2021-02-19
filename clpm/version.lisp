;;;; CLPM version definitions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/version
    (:use #:cl)
  (:import-from #:clpm-asdf
                #:clpm-version)
  (:export #:clpm-version))

(in-package #:clpm/version)
