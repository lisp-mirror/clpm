;;;; Implementation of execvpe.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/execvpe
    (:use #:cl
          #:alexandria
          #:cffi
          #:clpm/utils)
  (:export #:execvpe))

(in-package #:clpm/execvpe)

#-os-windows
(progn
  (defcfun ("execvpe"
            %execvpe)
      :int
    (file :string)
    (args :pointer)
    (envp :pointer))

  (defun execvpe (file args env &optional augment-env-p)
    "Call the execvpe C function. ~file~ must be a string. ~args~ is a list of
strings to pass as the arguments to the executable. ~env~ is an alist of
variable name (string), variable value (string) pairs. If ~augment-env-p~ is T,
the environment variables specified by ~env~ are appended with the current
environment variables."
    (let* ((env (mapcar (lambda (c)
                          (concatenate 'string (car c) "=" (cdr c)))
                        env))
           (new-env (append env (when augment-env-p (sb-ext:posix-environ)))))
      (with-foreign-objects ((foreign-args :pointer (+ 2 (length args)))
                             (foreign-env :pointer (+ 1 (length new-env))))

        ;; pack the args into foreign memory
        (setf (mem-aref foreign-args :pointer 0) (foreign-string-alloc file))
        (loop
          :for i :upfrom 1
          :for arg :in args
          :do
             (setf (mem-aref foreign-args :pointer i)
                   (foreign-string-alloc arg)))
        (setf (mem-aref foreign-args :pointer (1+ (length args)))
              (null-pointer))

        ;; pack the environment variables into foreign memory
        (loop
          :for i :upfrom 0
          :for assignment :in new-env
          :do
             (setf (mem-aref foreign-env :pointer i)
                   (foreign-string-alloc assignment)))
        (setf (mem-aref foreign-env :pointer (length new-env))
              (null-pointer))
        (let ((code (%execvpe file foreign-args foreign-env)))
          (error "Failed to exec with code ~S" code))
        ))))

#+os-windows
(defun execvpe (file args env &optional augment-env-p)
  "On Windows, _execvpe does not replace the process, so just use
~uiop:run-program~ instead."
  (assert augment-env-p)
  (multiple-value-bind (out err code)
      (apply #'uiop:run-program
             `(,file ,@args)
             :ignore-error-status t
             :input :interactive
             :output :interactive
             :error-output :interactive
             (run-program-augment-env-args env))
    (declare (ignore out err))
    (uiop:quit code)))
