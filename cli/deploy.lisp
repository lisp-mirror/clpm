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
                #:winhttp)
  (:export #:*default-clpm-home*))

(in-package #:clpm-cli/deploy)

(defvar *default-clpm-home* nil)

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
   ;; Compute the data directory for Deploy based on CLPM_HOME
   (let ((clpm-home (or (uiop:getenv "CLPM_HOME")
                        *default-clpm-home*)))
     (unless clpm-home
       (setf clpm-home (merge-pathnames (make-pathname :directory '(:relative :up "lib" "clpm"))
                                        (deploy:runtime-directory))))
     (setf deploy:*data-location* (uiop:truenamize (uiop:ensure-directory-pathname
                                                    (uiop:ensure-absolute-pathname clpm-home))))
     ;; Fixup the logical pathnames
     (when (uiop:featurep :clpm-logical-pathnames)
       (setf (logical-pathname-translations "clpm")
             `(("clpm:clpm;**;*.*.*" ,(merge-pathnames "clpm/**/*.*" deploy:*data-location*))
               ("clpm:cli;**;*.*.*" ,(merge-pathnames "cli/**/*.*" deploy:*data-location*))
               ("clpm:features;**;*.*.*" ,(merge-pathnames "features/**/*.*" deploy:*data-location*)))))
     (setf *clpm-client-asd-pathname* (merge-pathnames "src/clpm-client/clpm-client.asd"
                                                       deploy:*data-location*))
     (unless (uiop:probe-file* clpm-home)
       (format *error-output*
               "Unable to find CLPM_HOME. Please set CLPM_HOME environment variable.~%")
       (uiop:quit 1))))
 nil)
