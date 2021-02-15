;;;; Package
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-asdf
    (:use #:cl)
  (:local-nicknames (#:ops #:asdf-release-ops))
  (:export #:build-clpm-client-tarball-op
           #:clpm-system))

(in-package #:clpm-asdf)
