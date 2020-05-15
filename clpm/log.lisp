;;;; Logging support
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/log
    (:use #:cl)
  (:import-from #:log4cl)
  (:export #:setup-logger))

(in-package #:clpm/log)

;;; Logging framework for CLPM

(defmacro setup-logger ()
  "This macro *must* be called in any package where the logging macros are
used."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (log:package-options :category-separator "/")))
