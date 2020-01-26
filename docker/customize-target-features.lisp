;;;; File to customize SBCL's features while building it in Docker

(lambda (list)
  (flet ((enable (x)
           (pushnew x list))
         (disable (x)
           (setf list (remove x list))))
    (enable :sb-core-compression))
  list)
