;;;; Interacting with bundles
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-client/bundle
    (:use #:cl
          #:clpm-client/clpm
          #:clpm-client/env)
  (:import-from #:uiop
                #:getenv
                #:pathname-directory-pathname
                #:with-current-directory)
  (:export #:clpm-bundle-command
           #:clpm-bundle-install
           #:clpm-bundle-source-registry
           #:clpmfile-pathname
           #:clpm-inside-bundle-exec-p))

(in-package #:clpm-client/bundle)

(defun clpm-inside-bundle-exec-p ()
  "Returns T iff we were spawned by a `clpm bundle exec` command."
  (not (null (getenv "CLPM_BUNDLE_CLPMFILE"))))

(defun clpmfile-pathname ()
  "If spawned by a `clpm bundle exec` command, return the pathname to the
clpmfile."
  (pathname (getenv "CLPM_BUNDLE_CLPMFILE")))

(defun clpmfile-directory-pathname ()
  "The directory pathname containing the clpmfile."
  (pathname-directory-pathname (clpmfile-pathname)))

(defun clpm-bundle-command ()
  "Return a command (list of string) suitable for invoking the same CLPM
executable that spawned this bundle exec environment."
  (let ((bin (uiop:getenv "CLPM_BUNDLE_BIN")))
    (if bin
        (list bin)
        (let ((live-script (uiop:getenv "CLPM_BUNDLE_BIN_LIVE_SCRIPT"))
              (lisp-implementation (uiop:getenv "CLPM_BUNDLE_BIN_LISP_IMPLEMENTATION")))
          (cond
            ((equalp "sbcl" lisp-implementation)
             (list "sbcl" "--load" live-script "--"))
            (t
             (error "Unknown lisp implementation")))))))

(defun clpm-bundle-source-registry ()
  "Return a source-registry form for this bundle."
  (uiop:with-safe-io-syntax ()
    (read-from-string
     (run-clpm `("bundle" "source-registry"
                          "-f" ,(uiop:native-namestring (clpmfile-pathname))
                          "--with-client")
               :output '(:string :stripped t)))))

(defun clpm-bundle-install (&key (validate (constantly t)))
  "Run `clpm bundle install`. VALIDATE will be called with a diff and its return
value is passed to the bindle install command. Returns T iff the command exited
successfully."
  (let* ((proc (launch-clpm `("bundle" "install"
                                       "-f" ,(uiop:native-namestring (clpmfile-pathname))
                                       "--output" "sexp")
                            :output :stream
                            :input :stream
                            :error-output :stream))
         (diff (uiop:with-safe-io-syntax ()
                 (read (uiop:process-info-output proc) nil))))
    (when diff
      (let ((result (funcall validate diff)))
        (uiop:with-safe-io-syntax ()
          (prin1 result (uiop:process-info-input proc))
          (terpri (uiop:process-info-input proc))
          (finish-output (uiop:process-info-input proc)))))
    (when *clpm-dribble*
      (uiop:copy-stream-to-stream (uiop:process-info-error-output proc)
                                  *clpm-dribble*
                                  :linewise t
                                  :prefix *clpm-dribble-prefix*))
    (close (uiop:process-info-error-output proc))
    (close (uiop:process-info-input proc))
    (close (uiop:process-info-output proc))
    (zerop (uiop:wait-process proc))))
