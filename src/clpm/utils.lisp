(uiop:define-package #:clpm/utils
    (:use #:cl)
  (:import-from #:osicat)
  (:export #:run-program-augment-env-args))

(in-package #:clpm/utils)

(defun run-program-augment-env-args (new-env-alist)
  "Given an alist of environment variables, return a list of arguments suitable
for uiop:{launch/run}-program to set the augmented environment for the child
process."
  (let ((env (append new-env-alist
                     (osicat:environment))))
    #-sbcl (error "not implemented")
    (list :environment (mapcar (lambda (cons)
                                 (concatenate 'string (car cons) "=" (cdr cons)))
                               env))))
