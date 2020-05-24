;;;; clpm clpi release - EXPERIMENTAL
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/clpi/release
    (:use #:cl
          #:clpm-cli/common-args
          #:clpm-cli/commands/clpi/common
          #:clpm-cli/interface-defs
          #:clpm)
  (:import-from #:adopt)
  (:import-from #:jsown)
  (:import-from #:uiop
                #:*stdout*))

(in-package #:clpm-cli/commands/clpi/release)

(defparameter *option-asds*
  (adopt:make-option
   :asds
   :long "asd"
   :help "Grovel this ASD. Can be specified multiple-times."
   :parameter "ASD-PATHNAME"
   :reduce (adopt:flip #'cons)))

(defparameter *option-deps-from-lockfile*
  (adopt:make-option
   :deps-from-lockfile
   :long "deps-from-lockfile"
   :help "Load any dpendencies needed for grovelling from the lockfile in the current directory."
   :reduce (constantly t)))

(define-string *help-string*
  "EXPERIMENTAL

Prototype interface to produce a JSON object containing data about a new release
of a project. This *will* change over time. Ideally, this will also integrate
with an HTTP client to submit the data to a CLPI server.")

(defparameter *clpi-release-ui*
  (adopt:make-interface
   :name "clpm clpi release"
   :summary "Print a json description of the release for a CLPI server"
   :usage "clpi systems [options]"
   :help *help-string*
   :contents (list *group-common*
                   *option-asds*
                   *option-deps-from-lockfile*)))

(define-cli-command (("clpi" "release") *clpi-release-ui*) (arguments options)
  (destructuring-bind (project-name project-version release-url) arguments
    (let ((asds (gethash :asds options))
          (deps-from-lockfile-p (gethash :deps-from-lockfile options)))
      (write-string (jsown:to-json (clpm:make-release-jsown project-name project-version release-url asds
                                                            :clpmfile (when deps-from-lockfile-p
                                                                        (merge-pathnames "clpmfile")))))
      (terpri *standard-output*)))
  t)
