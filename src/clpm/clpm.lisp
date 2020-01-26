;;;; Common Lisp Package Manager - CLPM.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/clpm
    (:use #:cl
          ;;#:clpm/cli/bundle
          ;;#:clpm/cli/client
          ;;#:clpm/cli/config
          #:clpm/cli/entry
          ;;#:clpm/cli/install
          #:clpm/cli/license-info
          #:clpm/cli/sync
          #:clpm/interface)
  (:import-from #:deploy))

(in-package #:clpm/clpm)

(uiop:register-clear-configuration-hook
 (lambda ()
   (setf deploy:*status-output* nil)))

(uiop:register-image-restore-hook
 (lambda ()
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
