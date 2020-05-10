;;;; clpm bundle install
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/install
    (:use #:cl
          #:alexandria
          #:clpm/bundle
          #:clpm/cli/bundle/common
          #:clpm/cli/common-args
          #:clpm/cli/defs
          #:clpm/cli/interface-defs
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
                   *option-local*
                   *option-yes*
                   *option-output*
                   *option-no-resolve*)))

(defun sexp-interaction-y-or-n-p ()
  (uiop:with-safe-io-syntax ()
    (prin1 :proceedp *standard-output*)
    (terpri *standard-output*)
    (finish-output *standard-output*)
    (read *standard-input*)))

(defun make-validate-fun (yes-p output)
  (lambda (diff)
    (block nil
      (unless (context-diff-has-diff-p diff)
        (return t))
      (switch (output :test #'equal)
        ("sexp"
         (uiop:with-safe-io-syntax ()
           (prin1 (context-diff-to-plist diff) *standard-output*)
           (terpri *standard-output*)
           (or yes-p (sexp-interaction-y-or-n-p))))
        (t
         (print-context-diff diff *standard-output*)
         (or yes-p (y-or-n-p "Proceed?")))))))

(define-cli-command (("bundle" "install") *bundle-install-ui*) (args options)
  (declare (ignore args))
  (let* ((clpmfile-pathname (bundle-clpmfile-pathname))
         (*default-pathname-defaults* (uiop:pathname-directory-pathname clpmfile-pathname)))
    (bundle-install clpmfile-pathname :validate (make-validate-fun (gethash :yes options)
                                                                   (gethash :output options))
                    :no-resolve (gethash :bundle-no-resolve options))))
