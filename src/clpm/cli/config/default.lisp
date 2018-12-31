(uiop:define-package #:clpm/cli/config/default
    (:use #:cl
          #:clpm/cli/config/common
          #:clpm/cli/entry
          #:clpm/config
          #:clpm/log
          #:clpm/source)
  (:import-from #:uiop
                #:*stdout*)
  (:import-from #:net.didierverna.clon
                #:defsynopsis
                #:make-context
                #:getopt
                #:remainder
                #:help))

(in-package #:clpm/cli/config/default)

(setup-logger)

(defparameter *default-sources.conf*
  ";;; -*- mode: common-lisp; -*-
;;; Default sources.conf file generated by `clpm config default` contains only
;;; the primary Quicklisp distribution.
(:clpm-sources
 (\"https://beta.quicklisp.org/dist/quicklisp.txt\"
  :type :quicklisp
  :name \"quicklisp\"
  :force-https t))")

(defparameter *synopsis*
  (defsynopsis (:make-default nil)
    (text
     :contents "Write a default sources.conf file. Does not overwrite an existing file by default.")
    (flag
     :long-name "force"
     :description "Overwrite the sources.conf file even if it exists.")
    *common-arguments*))

(define-config-entry default (*synopsis*)
  ;; Unpack the command line arguments.
  (let* ((force-p (getopt :long-name "force"))
         (sources.conf (clpm-config-pathname '("sources.conf") :direction :output)))
    (log:info "Writing a default sources.conf file to ~A" sources.conf)
    (with-open-file (s sources.conf :direction :output
                                    :if-exists (if force-p
                                                   :supersede
                                                   :error))
      (write-string *default-sources.conf* s)
      (terpri s)))
  t)
