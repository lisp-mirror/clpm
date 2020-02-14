;;;; CLI common arguments
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/common-args
    (:use #:cl
          #:clpm/version)
  (:import-from #:adopt)
  (:export #:*group-common*
           #:*option-context*
           #:*option-help*
           #:*option-output*
           #:*option-yes*
           #:*option-verbose*))

(in-package #:clpm/cli/common-args)

(defparameter *option-help*
  (adopt:make-option :help
                     :long "help"
                     :help "Display help and exit"
                     :reduce (constantly t)))

(defparameter *option-yes*
  (adopt:make-option
   :yes
   :short #\y
   :long "yes"
   :help "Answer yes to all questions"
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

(defparameter *option-context*
  (adopt:make-option :context
                     :long "context"
                     :help "Set the context in which to operate"
                     :parameter "CONTEXT"
                     :reduce #'adopt:last))

(defparameter *option-output*
  (adopt:make-option :output
                     :long "output"
                     :help "Set how results are output. Currently meant for use only by the clpm-client library"
                     :parameter "OUTPUT"
                     :reduce #'adopt:last))
