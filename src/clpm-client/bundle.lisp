;;;; Interacting with bundles
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/bundle
    (:use #:cl
          #:clpm-client/clpm)
  (:import-from #:uiop
                #:getenv
                #:pathname-directory-pathname
                #:with-current-directory)
  (:export #:bundle-pathnames
           #:bundle-pathnames-for-asd-config
           #:clpmfile-pathname
           #:inside-bundle-exec-p))

(in-package #:clpm-client/bundle)

(defun inside-bundle-exec-p ()
  "Returns T iff we were spawned by a ~clpm bundle exec~ command."
  (not (null (getenv "CLPM_BUNDLE_CLPMFILE"))))

(defun clpmfile-pathname ()
  "If spawned by a ~clpm bundle exec~ command, return the pathname to the
clpmfile."
  (pathname (getenv "CLPM_BUNDLE_CLPMFILE")))

(defun clpmfile-directory-pathname ()
  "The directory pathname containing the clpmfile."
  (pathname-directory-pathname (clpmfile-pathname)))

(defun bundle-pathnames ()
  "Return a list of pathnames to ASD files that are part of this bundle."
  (with-current-directory ((clpmfile-directory-pathname))
    (uiop:split-string
     (run-clpm '("bundle" "pathnames")
               :output '(:string :stripped t))
     :separator '(#\Newline))))

(defun bundle-pathnames-for-asd-config ()
  (format nil "~{~A~^:~}" (bundle-pathnames)))
