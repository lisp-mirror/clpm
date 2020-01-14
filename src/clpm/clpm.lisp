;;;; Common Lisp Package Manager - CLPM.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/clpm
    (:use #:cl
          ;;#:clpm/cli/bundle
          ;;#:clpm/cli/client
          ;;#:clpm/cli/config
          #:clpm/cli/entry
          ;;#:clpm/cli/install
          #:clpm/cli/license-info
          #:clpm/cli/sync
          #:clpm/interface)
  (:import-from #:deploy))

(in-package #:clpm/clpm)

(uiop:register-clear-configuration-hook
 (lambda ()
   (setf deploy:*status-output* nil)))
