;;;; Defs for VCS Repositories
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.


(uiop:define-package #:clpm/repos/defs
    (:use #:cl)
  (:export #:ensure-ref-present-locally!
           #:ref-present-p
           #:repo-archive-stream
           #:repo-lib-base-pathname
           #:resolve-ref-to-commit))

(in-package #:clpm/repos/defs)

(defgeneric ensure-ref-present-locally! (repo &key commit branch tag))

(defgeneric ref-present-p (repo &key commit branch tag))

(defgeneric repo-archive-stream (repo &key branch tag commit))

(defgeneric repo-lib-base-pathname (repo))

(defgeneric resolve-ref-to-commit (repo &key branch tag commit))
