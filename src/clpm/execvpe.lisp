(uiop:define-package #:clpm/execvpe
    (:use #:cl
          #:alexandria
          #:cffi)
  (:import-from #:osicat
                #:environment)
  (:export #:execvpe))

(in-package #:clpm/execvpe)

(defcfun ("execvpe" %execvpe) :int
  (file :string)
  (args :pointer)
  (envp :pointer))

(defun execvpe (file args env &optional augment-env-p)
  (let ((env (append env (when augment-env-p (environment)))))
    (with-foreign-objects ((foreign-args :pointer (+ 2 (length args)))
                           (foreign-env :pointer (+ 1 (length env))))

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
        :for (var-name . var-value) :in env
        :do
           (setf (mem-aref foreign-env :pointer i)
                 (foreign-string-alloc (concatenate 'string var-name "=" var-value))))
      (setf (mem-aref foreign-env :pointer (length env))
            (null-pointer))
      (%execvpe file foreign-args foreign-env))))
