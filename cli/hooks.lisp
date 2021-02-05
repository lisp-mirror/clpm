(uiop:define-package #:clpm-cli/hooks
    (:use #:cl))

(in-package #:clpm-cli/hooks)

(uiop:register-image-restore-hook
 (lambda ()
   ;; Fix PURI's URL encoding to always use uppercase letters:
   (setf puri::*escaped-encoding*
         (vector #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F))))
