;;;; Source definitions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/source
    (:use #:cl
          #:clpm/cache
          #:clpm/config
          #:clpm/sources/config
          #:clpm/sources/defs
          #:clpm/sources/fs
          #:clpm/sources/vcs)
  (:reexport #:clpm/sources/config)
  (:reexport #:clpm/sources/defs)
  (:reexport #:clpm/sources/fs)
  (:reexport #:clpm/sources/vcs)
  (:export #:sources))

(in-package #:clpm/source)

(defvar *sources*)

(defun sources ()
  (unless (boundp '*sources*)
    (setf *sources* (load-sources)))
  *sources*)

(defun clear-sources ()
  (makunbound '*sources*))

(uiop:register-clear-configuration-hook 'clear-sources)
