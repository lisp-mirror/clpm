;;;; clpm license-info
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/license-info
    (:use #:cl
          #:clpm/cli/entry
          #:clpm-licenses)
  (:import-from #:uiop
                #:*stdout*)
  (:import-from #:net.didierverna.clon
                #:defsynopsis
                #:make-context
                #:getopt
                #:remainder
                #:help))

(in-package #:clpm/cli/license-info)

(defparameter *synopsis*
  (defsynopsis (:make-default nil)
    (text :contents "Print the license info of CLPM and all dependencies.")
    *common-arguments*))

(defparameter *license-separator*
  "
================================================================================
")

(defun print-licenses (stream)
  (format stream "CLPM is licensed under the following terms:~%~%~A~%" *clpm-license*)
  (fresh-line stream)

  (loop
    :for project-name :being :the :hash-keys :in *licenses* :using (hash-value license)
    :for notice := (gethash project-name *notices*)
    :do
       (fresh-line stream)
       (write-string *license-separator* stream)
    :when notice
      :do
         (format stream "~A~%~%~A~%" notice license)
    :else
      :do
         (format stream
                 "CLPM contains code from the ~A project, which is licensed under the following terms:~%~%~A~%"
                 project-name license))
  (fresh-line stream))

(define-cli-entry license-info (*synopsis*)
  (print-licenses *stdout*)
  t)
