;;;; Syncing sources
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/sync
    (:use #:cl
          #:clpm/log
          #:clpm/session
          #:clpm/source)
  (:export #:sync))

(in-package #:clpm/sync)

(setup-logger)

(defun sync (&key sources)
  (with-clpm-session ()
    (let ((sources (if sources (mapcar #'get-source sources) (sources))))
      (log:info "Syncing ~{~A~^, ~}" (mapcar #'source-name sources))
      (mapc #'sync-source sources))))
