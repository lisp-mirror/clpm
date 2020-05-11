;;;; clpm bundle install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/update
    (:use #:cl
          #:clpm/bundle
          #:clpm/cli/bundle/common
          #:clpm/cli/common-args
          #:clpm/cli/defs
          #:clpm/cli/interface-defs
          #:clpm/context
          #:clpm/log)
  (:import-from #:adopt))

(in-package #:clpm/cli/bundle/update)

(setup-logger)

(define-string *help-string*
  "Update a bundle described in a clpmfile. Can either update the whole bundle
or a subset.

If no clpmfile.lock file exists, one is created by first syncing all sources in
the clpmfile and using the requirements to resolve a set of project releases
that satisfy them all. All of the releases are then installed locally and the
lock file is written.

If a lock file does exist, all sources are synced and the requirements
re-resolved from scratch. If a list of systems to update is provided, then the
preference for unrelated projects is to keep them at the same version as
currently in the lock file. If no systems to update are provided then the
preference is to update everything to the latest version possible.")

(defparameter *bundle-update-ui*
  (adopt:make-interface
   :name "clpm bundle update"
   :summary "Common Lisp Package Manager Bundle Update"
   :usage "bundle update [options] <SYSTEM>*"
   :help *help-string*
   :contents (list *group-common*
                   *group-bundle*
                   *option-local*
                   *option-yes*)))

(define-cli-command (("bundle" "update") *bundle-update-ui*) (args options)
  (let* ((clpmfile-pathname (bundle-clpmfile-pathname))
         (yesp (gethash :yes options)))
    (bundle-update clpmfile-pathname :update-systems args
                                     :validate (make-diff-validate-fun :yesp yesp))
    t))
