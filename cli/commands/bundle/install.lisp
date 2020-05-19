;;;; clpm bundle install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/bundle/install
    (:use #:cl
          #:clpm-cli/commands/bundle/common
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt)
  (:import-from #:clpm))

(in-package #:clpm-cli/commands/bundle/install)

(define-string *help-string*
  "Install a bundle described in a clpmfile.

If no clpmfile.lock file exists, one is created by first syncing all sources in
the clpmfile and using the requirements to resolve a set of project releases
that satisfy them all. All of the releases are then installed locally and the
lock file is written.

If a lock file does exist, all sources are synced and the requirements
re-resolved, but preference is given to installing the releases that already
exist in the lock file.")

(defparameter *bundle-install-ui*
  (adopt:make-interface
   :name "clpm bundle install"
   :summary "Common Lisp Package Manager Bundle Install"
   :usage "bundle install [options]"
   :help *help-string*
   :contents (list *group-common*
                   *group-bundle*
                   *option-local*
                   *option-yes*
                   *option-no-resolve*)))

(define-cli-command (("bundle" "install") *bundle-install-ui*) (args options)
  (declare (ignore args))
  (clpm:bundle-install :validate (make-diff-validate-fun :yesp (gethash :yes options))
                       :no-resolve (gethash :bundle-no-resolve options))
  t)
