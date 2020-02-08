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

(defgeneric ensure-ref-present-locally! (repo ref))

(defgeneric ref-present-p (repo ref))

(defgeneric repo-archive-stream (repo ref))

(defgeneric repo-lib-base-pathname (repo))

(defgeneric resolve-ref-to-commit (repo ref))
