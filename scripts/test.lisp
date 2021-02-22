;;;; Script to test a CLPM executable
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:cl-user)

(pushnew :hunchentoot-no-ssl *features*)

(load (merge-pathnames "common.lisp" *load-truename*))

(in-package #:clpm-scripts)

(defparameter *option-clpm*
  (adopt:make-option
   :clpm
   :help "Specify the clpm executable to test"
   :long "clpm"
   :initial-value "clpm"
   :parameter "CLPM-EXEC"
   :reduce #'adopt:last))

(defparameter *ui*
  (adopt:make-interface
   :name "scripts/build.lisp"
   :summary "Build script for CLPM"
   :help "Build script for CLPM"
   :usage "[options]"
   :contents (list *option-help*
                   *option-clpm*)))

(defvar *args*)
(defvar *opts*)

(multiple-value-setq (*args* *opts*) (adopt:parse-options *ui*))

(when (gethash :help *opts*)
  (adopt:print-help-and-exit *ui*))

(asdf:load-system :clpm-test)

(let ((clpm (gethash :clpm *opts*)))
  (when (pathname-directory clpm)
    (setf clpm (uiop:ensure-absolute-pathname (merge-pathnames clpm (uiop:getcwd)))))
  (unless (clpm-test::run-tests-with-server :clpm clpm)
    (uiop:quit 1)))
