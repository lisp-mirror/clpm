;;;; clpm bundle install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/update
    (:use #:cl
          #:clpm/bundle
          #:clpm/cli/bundle/common
          #:clpm/cli/common-args
          #:clpm/cli/subcommands
          #:clpm/log)
  (:import-from #:adopt))

(in-package #:clpm/cli/bundle/update)

(setup-logger)

(defparameter *bundle-update-ui*
  (adopt:make-interface
   :name "clpm bundle update"
   :summary "Common Lisp Package Manager Bundle Update"
   :usage "bundle update [options] <PROJECTS*>"
   :help "Update a bundle"
   :contents (list *group-common*
                   *group-bundle*)))

(define-cli-command (("bundle" "update") *bundle-update-ui*) (args options)
  (let ((clpmfile-pathname (merge-pathnames (gethash :bundle-file options)
                                            (uiop:getcwd))))
    (bundle-update clpmfile-pathname)
    t))
