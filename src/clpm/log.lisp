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

(defun configure-logger ()
  (log:config (log:category '(clpm))
              :tricky-console :stream *error-output* :immediate-flush :own :notime
              :warn))

(uiop:register-image-restore-hook 'configure-logger nil)
