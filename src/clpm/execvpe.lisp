(uiop:define-package #:clpm/execvpe
    (:use #:cl
          #:alexandria
          #:cffi)
  (:export #:execvpe))

(in-package #:clpm/execvpe)

(defcfun ("execvpe" %execvpe) :int
  (file :string)
  (args :pointer)
  (envp :pointer))

(defun execvpe (file args env &optional augment-env-p)
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
      (%execvpe file foreign-args foreign-env))))
