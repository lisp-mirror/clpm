;;;; CLPM CLI defs
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/defs
    (:use #:cl)
  (:import-from #:cl-ppcre)
  (:export #:define-string))

(in-package #:clpm/cli/defs)

(defmacro define-string (name string)
  `(defparameter ,name ,(format nil
                                (cl-ppcre:regex-replace-all
                                 '(:sequence (:register :non-whitespace-char-class) #\Newline (:greedy-repetition 0 nil #\Space) (:register :non-whitespace-char-class)) string "\\1 ~
 \\2"))))
