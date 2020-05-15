;;;; Defs for VCS Repositories
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.


(uiop:define-package #:clpm/repos/defs
    (:use #:cl)
  (:export #:*fetch-repo-automatically*
           #:clone-repo!
           #:ensure-ref-present-locally!
           #:ref-present-p
           #:repo-archive-stream
           #:repo-current-branch
           #:repo-current-commit
           #:repo-lib-base-pathname
           #:repo-to-form
           #:resolve-ref-to-commit))

(in-package #:clpm/repos/defs)

(defvar *fetch-repo-automatically* t)

(defgeneric clone-repo! (repo &key pathname not-bare-p))

(defgeneric ensure-ref-present-locally! (repo ref))

(defgeneric ref-present-p (repo ref))

(defgeneric repo-archive-stream (repo ref))

(defgeneric repo-current-branch (repo))

(defgeneric repo-current-commit (repo))

(defgeneric repo-lib-base-pathname (repo))

(defgeneric repo-to-form (repo))

(defgeneric resolve-ref-to-commit (repo ref))
