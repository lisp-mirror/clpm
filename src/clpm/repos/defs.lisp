;;;; Defs for VCS Repositories
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.


(uiop:define-package #:clpm/repos/defs
    (:use #:cl)
  (:export #:*fetch-repo-automatically*
           #:ensure-ref-present-locally!
           #:ref-present-p
           #:repo-archive-stream
           #:repo-lib-base-pathname
           #:repo-to-form
           #:resolve-ref-to-commit))

(in-package #:clpm/repos/defs)

(defvar *fetch-repo-automatically* t)

(defgeneric ensure-ref-present-locally! (repo ref))

(defgeneric ref-present-p (repo ref))

(defgeneric repo-archive-stream (repo ref))

(defgeneric repo-lib-base-pathname (repo))

(defgeneric repo-to-form (repo))

(defgeneric resolve-ref-to-commit (repo ref))
