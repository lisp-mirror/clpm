;;;; Interacting with bundles
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/bundle
    (:use #:cl)
  (:import-from #:uiop
                #:getenv)
  (:export #:clpmfile-pathname
           #:inside-bundle-exec-p))

(in-package #:clpm-client/bundle)

(defun inside-bundle-exec-p ()
  "Returns T iff we were spawned by a ~clpm bundle exec~ command."
  (not (null (getenv "CLPM_BUNDLE_CLPMFILE"))))

(defun clpmfile-pathname ()
  "If spawned by a ~clpm bundle exec~ command, return the pathname to the
clpmfile."
  (pathname (getenv "CLPM_BUNDLE_CLPMFILE")))
