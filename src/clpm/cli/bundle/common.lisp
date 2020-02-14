;;;; Common bundle CLI functions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/common
    (:use #:cl
          #:alexandria
          #:clpm/cli/common-args
          #:clpm/cli/subcommands
          #:clpm/config
          #:clpm/utils)
  (:import-from #:adopt)
  (:import-from #:cl-ppcre)
  (:export #:*group-bundle*
           #:*option-bundle-local*))

(in-package #:clpm/cli/bundle/common)

(defparameter *option-file*
  (adopt:make-option
   :bundle-file
   :short #\f
   :parameter "FILE"
   :initial-value "clpmfile"
   :help "The path to the clpmfile"
   :reduce #'adopt:last))

(defparameter *option-bundle-local*
  (adopt:make-option
   :bundle-local
   :long "local"
   :help "Do not sync remote sources, use only the data located in the local cache"
   :reduce (constantly t)))

(defparameter *group-bundle*
  (adopt:make-group
   :bundle
   :title "Bundle options"
   :help "Common options for bundle commands"
   :options (list *option-file*)))

(defparameter *default-ui*
  (adopt:make-interface
   :name "clpm bundle"
   :summary "Common Lisp Package Manager Bundle"
   :usage "bundle [options] subcommand"
   :help "Bundle commands"
   :contents (list *group-common*
                   *group-bundle*)))

(define-cli-command-folder
    (("bundle") *default-ui*)
    (args options)
    (declare (ignore args))
    (let* ((clpmfile-path (merge-pathnames (gethash :bundle-file options)
                                           (uiop:getcwd)))
           (local-config (merge-pathnames ".clpm/bundle.conf"
                                          (uiop:pathname-directory-pathname clpmfile-path))))
      (when (probe-file local-config)
        (config-add-file-source! local-config))))
