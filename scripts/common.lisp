;;;; Common setup for all clpm scripts that configures ASDF appropriately
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(defpackage #:clpm-scripts
  (:use #:cl))

(in-package #:clpm-scripts)

(require :asdf)

(defvar *setup-file-pathname* *load-truename*
  "The pathname to this file.")

(defvar *root-pathname* (uiop:pathname-parent-directory-pathname
                         (uiop:pathname-directory-pathname *setup-file-pathname*))
  "The pathname to the root directory of the CLPM release being built.")

(defvar *build-root-pathname* (merge-pathnames "build/"
                                               *root-pathname*)
  "The pathname to the root of the build directory. Defaults to build/ inside
*ROOT-PATHNAME*")

(defun setup-asdf ()
  (asdf:initialize-source-registry `(:source-registry
                                     :ignore-inherited-configuration
                                     (:tree ,*root-pathname*))))
