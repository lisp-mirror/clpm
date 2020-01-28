;;;; CLI common arguments
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/common-args
    (:use #:cl
          #:clpm/version)
  (:import-from #:adopt)
  (:export #:*group-common*
           #:*option-help*
           #:*option-verbose*))

(in-package #:clpm/cli/common-args)

(defparameter *option-help*
  (adopt:make-option :help
                     :long "help"
                     :help "Display help and exit"
                     :reduce (constantly t)))

(defparameter *option-verbose*
  (adopt:make-option :verbose
                     :long "verbose"
                     :short #\V
                     :help "Increase verbosity of output. Can be specified multiple times."
                     :initial-value 0
                     :reduce #'1+))

(defparameter *group-common*
  (adopt:make-group :common
                    :title "Common"
                    :help "Options common to all CLPM operations"
                    :options (list *option-help*
                                   *option-verbose*)))
