;;;; Miscellaneous utilities
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/utils
    (:use #:cl
          #:puri)
  (:export #:run-program-augment-env-args
           #:uri-to-string))

(in-package #:clpm/utils)

(defun run-program-augment-env-args (new-env-alist)
  "Given an alist of environment variables, return a list of arguments suitable
for uiop:{launch/run}-program to set the augmented environment for the child
process."
  #-sbcl (error "not implemented")
  (let ((env (append (mapcar (lambda (c)
                               (concatenate 'string (car c) "=" (cdr c)))
                             new-env-alist)
                     (sb-ext:posix-environ))))
    (list :environment env)))

(defun uri-to-string (uri)
  "Convert a puri URI to a string."
  (with-output-to-string (s)
    (render-uri uri s)))
