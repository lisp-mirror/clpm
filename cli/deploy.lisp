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

(defun copy-directory-tree-ignoring-fasls-and-.git (source target)
  (cond
    ((uiop:directory-pathname-p source)
     (let* ((directory-name (car (last (pathname-directory source))))
            (new-target (merge-pathnames (make-pathname :directory (list :relative directory-name))
                                         target)))
       (unless (equal directory-name ".git")
         (dolist (new-source (uiop:directory* (merge-pathnames uiop:*wild-file* source)))
           (copy-directory-tree-ignoring-fasls-and-.git new-source new-target)))))
    ((not (equal (pathname-type source) "fasl"))
     (ensure-directories-exist target)
     (uiop:copy-file source (merge-pathnames (make-pathname :name (pathname-name source)
                                                            :type (pathname-type source))
                                             target)))))

(deploy:define-hook (:deploy deploy-source-code) (system directory)
  (when (uiop:featurep :clpm-deploy-source-code)
    (dolist (dir '("cli/" "features/" "clpm/" "ext/" "licenses/"
                   "clpm-cli.asd" "clpm-features.asd" "clpm.asd"))
      (copy-directory-tree-ignoring-fasls-and-.git
       (merge-pathnames dir (asdf:system-source-directory system))
       (merge-pathnames "src/" directory)))))

(deploy:define-hook (:deploy deploy-client) (system directory)
  (copy-directory-tree-ignoring-fasls-and-.git
   (merge-pathnames "client/" (asdf:system-source-directory system))
   directory))

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
                                      (uiop:pathname-directory-pathname core-pathname))))
     (setf deploy:*data-location* (uiop:truenamize (uiop:ensure-directory-pathname
                                                    (uiop:ensure-absolute-pathname clpm-home))))
     (setf *clpm-client-asd-pathname* (merge-pathnames "client/clpm-client.asd"
                                                       deploy:*data-location*))))
 nil)
