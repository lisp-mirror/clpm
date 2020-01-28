;;;; Massaging the deploy library.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/deploy
    (:use #:cl)
  (:import-from #:deploy))

(in-package #:clpm/deploy)

(uiop:register-image-restore-hook
 (lambda ()
   ;; Silence Deploy
   (setf deploy:*status-output* nil)
   ;; Compute the data directory for Deploy based on CLPM_HOME
   (let ((clpm-home (uiop:getenv "CLPM_HOME")))
     (unless clpm-home
       (setf clpm-home (merge-pathnames (make-pathname :directory '(:relative :up "lib" "clpm"))
                                        (deploy:runtime-directory))))
     (setf deploy:*data-location* (uiop:truenamize (uiop:ensure-directory-pathname
                                                    (uiop:ensure-absolute-pathname clpm-home))))
     (unless (uiop:probe-file* clpm-home)
       (format *error-output*
               "Unable to find CLPM_HOME. Please set CLPM_HOME environment variable.~%")
       (uiop:quit 1))))
 nil)
