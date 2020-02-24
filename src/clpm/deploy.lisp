;;;; Massaging the deploy library.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/deploy
    (:use #:cl
          #:clpm/client
          #:clpm/deps)
  (:import-from #:deploy)
  #+clpm-winhttp
  (:import-from #:winhttp
                #:winhttp)
  (:export #:*default-clpm-home*))

(in-package #:clpm/deploy)

(defvar *default-clpm-home* nil)

(deploy:define-resource-directory deploy-src "src/")

;; Don't deploy the WinHttp dll
#+clpm-winhttp
(deploy:define-library winhttp
  :dont-deploy t)

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
             `(("clpm:src;**;*.*.*" ,(merge-pathnames "src/**/*.*" deploy:*data-location*)))))
     (setf *clpm-groveler-asd-pathname* (merge-pathnames "src/clpm-groveler/clpm-groveler.asd"
                                                         deploy:*data-location*))
     (setf *clpm-client-asd-pathname* (merge-pathnames "src/clpm-client/clpm-client.asd"
                                                       deploy:*data-location*))
     (unless (uiop:probe-file* clpm-home)
       (format *error-output*
               "Unable to find CLPM_HOME. Please set CLPM_HOME environment variable.~%")
       (uiop:quit 1))))
 nil)
