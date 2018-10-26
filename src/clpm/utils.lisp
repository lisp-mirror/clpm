(uiop:define-package #:clpm/utils
    (:use #:cl)
  (:export #:run-program-augment-env-args))

(in-package #:clpm/utils)

(defun run-program-augment-env-args (new-env-alist)
  "Given an alist of environment variables, return a list of arguments suitable
for uiop:{launch/run}-program to set the augmented environment for the child
process."
  (let ((env (append (mapcar (lambda (c)
                               (concatenate 'string (car c) "=" (cdr c)))
                             new-env-alist)
                     (sb-ext:posix-environ))))
    #-sbcl (error "not implemented")
    (list :environment env)))
