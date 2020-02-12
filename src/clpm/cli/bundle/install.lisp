;;;; clpm bundle install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/install
    (:use #:cl
          #:clpm/bundle
          #:clpm/cli/bundle/common
          #:clpm/cli/common-args
          #:clpm/cli/subcommands
          #:clpm/log)
  (:import-from #:adopt))

(in-package #:clpm/cli/bundle/install)

(setup-logger)

(defparameter *bundle-install-ui*
  (adopt:make-interface
   :name "clpm bundle install"
   :summary "Common Lisp Package Manager Bundle Install"
   :usage "bundle install [options]"
   :help "Install a bundle"
   :contents (list *group-common*
                   *group-bundle*)))

(define-cli-command (("bundle" "install") *bundle-install-ui*) (args options)
  (let ((clpmfile-pathname (merge-pathnames (gethash :bundle-file options)
                                            (uiop:getcwd))))
    (bundle-install clpmfile-pathname)
    t))
