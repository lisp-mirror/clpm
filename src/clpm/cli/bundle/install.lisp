;;;; clpm bundle install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/install
    (:use #:cl
          #:clpm/bundle
          #:clpm/cli/bundle/common
          #:clpm/cli/common-args
          #:clpm/cli/defs
          #:clpm/cli/subcommands
          #:clpm/context
          #:clpm/log)
  (:import-from #:adopt))

(in-package #:clpm/cli/bundle/install)

(setup-logger)

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
                   *option-bundle-local*)))

(defun make-validate-fun (yes-p output)
  (lambda (diff)
    (unless (equal output "sexp")
      ;; We can't print this in a sexp format at the moment.
      (print-context-diff diff *standard-output*))
    (or yes-p (y-or-n-p "Proceed?"))))

(define-cli-command (("bundle" "install") *bundle-install-ui*) (args options)
  (let ((clpmfile-pathname (merge-pathnames (gethash :bundle-file options)
                                            (uiop:getcwd))))
    (bundle-install clpmfile-pathname :local (gethash :bundle-local options)
                                      :validate (make-validate-fun (gethash :yes options)
                                                                   (gethash :output options)))
    t))
