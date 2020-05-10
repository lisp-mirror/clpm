;;;; clpm bundle source-registry
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/source-registry
    (:use #:cl
          #:clpm/bundle
          #:clpm/cli/bundle/common
          #:clpm/cli/common-args
          #:clpm/cli/interface-defs
          #:clpm/clpmfile
          #:clpm/log)
  (:import-from #:adopt))

(in-package #:clpm/cli/bundle/source-registry)

(setup-logger)

(defparameter *option-with-client*
  (adopt:make-option
   :bundle-exec-with-client
   :long "with-client"
   :help "Include the CLPM client in the source registry"
   :reduce (constantly t)))

(defparameter *bundle-source-registry-ui*
  (adopt:make-interface
   :name "clpm bundle source-registry"
   :summary "Common Lisp Package Manager Bundle Source-registry"
   :usage "bundle source-registry [options]"
   :help "Print the source registry for a bundle."
   :contents (list *group-common*
                   *group-bundle*
                   *option-with-client*)))

(define-cli-command (("bundle" "source-registry") *bundle-source-registry-ui*) (args options)
  (declare (ignore args))
  (let* ((clpmfile-pathname (bundle-clpmfile-pathname))
         (*default-pathname-defaults* (uiop:pathname-directory-pathname clpmfile-pathname))
         (include-client-p (gethash :bundle-exec-with-client options))
         (cl-source-registry-form (bundle-source-registry clpmfile-pathname :include-client-p include-client-p)))
    (format t "~S~%" cl-source-registry-form)
    t))
