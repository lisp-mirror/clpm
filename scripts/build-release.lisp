;;;; Script to build CLPM releases
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:cl-user)

(load (merge-pathnames "common.lisp" *load-truename*))

(in-package #:clpm-scripts)

(setup-asdf)

(defparameter *option-static*
  (adopt:make-option
   :static
   :help "Build a static executable"
   :long "static"
   :reduce (constantly t)))

(defparameter *ui*
  (adopt:make-interface
   :name "scripts/build.lisp"
   :summary "Build script for CLPM"
   :help "Build script for CLPM"
   :usage "[options]"
   :contents (list *option-help*
                   *option-static*)))

(defvar *args*)
(defvar *opts*)

(multiple-value-setq (*args* *opts*) (adopt:parse-options *ui*))

(when (gethash :help *opts*)
  (adopt:print-help-and-exit *ui*))

;; TODO: This is a bit hacky, but speeds up the build significantly when
;; starting from scratch (like in CI). The root problem is that
;; asdf-release-ops will occasionally cause the same code to be compiled twice:
;; once in the child process and once in the parent. This is because we use
;; asdf:monolithic-lib-op in the parent. However, moving that op to the child
;; didn't quite work as it would error out due to package variance in
;; dexador...

(asdf:load-system :clpm)
(asdf:load-system :clpm-cli)

(defparameter *op* (if (gethash :static *opts*)
                       'asdf-release-ops:static-release-archive-op
                       'asdf-release-ops:dynamic-release-archive-op))

(asdf:operate *op* :clpm)

(format uiop:*stdout*
        "A ~:[dynamic~;static~] release has been built at ~A~%"
        (gethash :static *opts*)
        (asdf:output-file *op* :clpm))
