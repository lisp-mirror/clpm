;;;; When loaded, this starts CLPM's main method
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:cl-user)

(load (merge-pathnames "common.lisp"
                       *load-truename*))

(in-package #:clpm-scripts)

(setup-asdf "live")

;; Special handling for Windows builds.
(when (uiop:os-windows-p)
  ;; CL-PLUS-SSL requires sb-bsd-socket be loaded (it uses an internal package
  ;; loaded by that contrib), but it doesn't declare a dependency on it.
  ;; (require :sb-bsd-sockets)
  ;; By default, openssl is ~useless on Windows. Don't build it until we can
  ;; figure out how to make it use the OS' certificate store.
  (pushnew :drakma-no-ssl *features*))

;; Load CLPM
(flet ((load-clpm ()
         (let* ((compilation-visible-p (equal (uiop:getenv "CLPM_LIVE_COMPILATION_VISIBLE") "true"))
                (output-stream (if compilation-visible-p
                                   *error-output*
                                   (make-string-output-stream)))
                (stderr *error-output*)
                (*error-output* output-stream)
                (*standard-output* output-stream))
           (handler-bind
               ((error (lambda (e)
                         (unless compilation-visible-p
                           (write-string (get-output-stream-string output-stream) stderr)
                           (terpri stderr))
                         (format stderr "~&Error while compiling CLPM: ~S~%" e)
                         (uiop:print-condition-backtrace e :stream stderr))))
             (asdf:load-system :clpm)))))
  (load-clpm))

;; Record the pathname to this script.
(setf clpm/utils:*live-script-location* (uiop:native-namestring *load-truename*))

(when (equal (uiop:getenv "CLPM_LIVE_PRIVATE_REPL") "true")
  (sb-impl::toplevel-repl nil))

(let ((clpm-sys (asdf:find-system :clpm)))
  (uiop:call-function (asdf::component-entry-point clpm-sys)))
