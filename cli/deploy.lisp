;;;; Massaging the deploy library.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/deploy
    (:use #:cl
          #:clpm/client)
  (:import-from #:deploy)
  (:import-from #:puri)
  #+clpm-winhttp
  (:import-from #:winhttp
                #:winhttp))

(in-package #:clpm-cli/deploy)

(deploy:define-resource-directory deploy-src "clpm/")
(deploy:define-resource-directory deploy-cli "cli/")
(deploy:define-resource-directory deploy-features "features/")
(deploy:define-resource-directory deploy-client "client/")

;; Don't deploy the WinHttp dll
#+clpm-winhttp
(deploy:define-library winhttp
  :dont-deploy t)

(uiop:register-image-restore-hook
 (lambda ()
   ;; Fix PURI's URL encoding to always use uppercase letters:
   (setf puri::*escaped-encoding*
         (vector #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F))))

(uiop:register-image-restore-hook
 (lambda ()
   ;; Silence Deploy
   (setf deploy:*status-output* nil)
   ;; Compute the data directory for Deploy based on the core's pathname (which,
   ;; since we're using deploy, is the same as the runtime pathname).
   (let* ((core-pathname sb-ext:*core-pathname*)
          (clpm-home (merge-pathnames (make-pathname :directory '(:relative :up "lib" "clpm"))
                                      core-pathname)))
     (setf deploy:*data-location* (uiop:truenamize (uiop:ensure-directory-pathname
                                                    (uiop:ensure-absolute-pathname clpm-home))))
     (setf *clpm-client-asd-pathname* (merge-pathnames "client/clpm-client.asd"
                                                       deploy:*data-location*))))
 nil)
