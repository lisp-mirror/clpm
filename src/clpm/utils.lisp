;;;; Miscellaneous utilities
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/utils
    (:use #:cl
          #:puri
          #:split-sequence)
  (:export #:run-program-augment-env-args
           #:uri-to-string))

(in-package #:clpm/utils)

#+sbcl
(defun run-program-augment-env-args (new-env-alist)
  "Given an alist of environment variables, return a list of arguments suitable
for uiop:{launch/run}-program to set the augmented environment for the child
process."
  (let* ((inherited-env
           (remove-if (lambda (x)
                        (let ((name (first (split-sequence #\= x))))
                          (member name new-env-alist :test #'equal :key #'car)))
                      (sb-ext:posix-environ)))
         (env (append (mapcar (lambda (c)
                                (concatenate 'string (car c) "=" (cdr c)))
                              new-env-alist)
                      inherited-env)))
    (list :environment env)))

#-sbcl
(defun run-program-augment-env-args (new-env-alist)
  (error "not implemented"))

(defun uri-to-string (uri)
  "Convert a puri URI to a string."
  (with-output-to-string (s)
    (render-uri uri s)))
