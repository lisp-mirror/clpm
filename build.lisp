;;;; CLPM Build Helper

(require :asdf)

(defpackage #:clpm-build
  (:use #:cl)
  (:import-from #:uiop)
  (:export #:build))

(in-package #:clpm-build)

(defvar *setup-file-pathname* *load-truename*
  "The pathname to this file.")

(defvar *root-pathname* (uiop:pathname-directory-pathname *setup-file-pathname*)
  "The pathname to the root directory of the CLPM release being built.")

(defvar *build-root-pathname* (merge-pathnames "build/"
                                               *root-pathname*)
  "The pathname to the root of the build directory. Defaults to build/ inside
*ROOT-PATHNAME*")

(defun build (&key
                (root-pathname *root-pathname*)
                (build-root-pathname *build-root-pathname*))
  (let* ((*root-pathname* (uiop:ensure-directory-pathname root-pathname))
         (*build-root-pathname* (uiop:ensure-directory-pathname build-root-pathname))
         (build-cache (merge-pathnames "cl-cache/" *build-root-pathname*)))
    (format uiop:*stderr*
            "I will build CLPM from sources located at ~A~%The build files will be located at ~A~%~%~%"
            *root-pathname*
            *build-root-pathname*)
    (asdf:clear-configuration)
    (asdf:initialize-source-registry `(:source-registry
                                       :ignore-inherited-configuration
                                       (:tree ,*root-pathname*)))
    (asdf:initialize-output-translations `(:output-translations
                                           :ignore-inherited-configuration
                                           (:root (,build-cache :implementation :**/ :*.*.*))))
    (asdf:make :clpm)))

(build)
