;;;; Support for sandboxing with firejail
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/sandbox/firejail
    (:use #:cl
          #:clpm/cache
          #:clpm/sandbox/defs))

(in-package #:clpm/sandbox/firejail)

(defclass firejail-sandbox ()
  ((path
    :initarg :path
    :initform "firejail"
    :reader firejail-path
    :documentation
    "Path to firejail executable."))
  (:documentation
   "A sandbox client that uses firejail."))

(register-sandbox-client :firejail 'firejail-sandbox)

(defmethod sandbox-client-available-p ((client firejail-sandbox))
  "Returns T iff the firejail program exists at the path specified by the client
and its version can be successfully queried."
  (ignore-errors
   (zerop
    (nth-value 2
               (uiop:run-program (list (firejail-path client)
                                       "--version")
                                 :ignore-error-status t)))))

(defmethod %sandbox-augment-command ((client firejail-sandbox) command
                                    &key read-write-pathnames)
  `(,(firejail-path client)
    "--x11=none"
    "--net=none"
    "--private-tmp"
    "--read-only=/home"
    ,(format nil "--read-only=~A" (uiop:getenv "HOME"))
    ,@(loop
        :for place :in read-write-pathnames
        :for namestring := (namestring place)
        :collect (format nil "--read-write=~A" namestring))
    "--quiet"
    "--caps.drop=all"
    "--no3d"
    "--nodbus"
    "--nodvd"
    "--nonewprivs"
    "--noroot"
    "--nosound"
    "--novideo"
    "--private-dev"
    "--seccomp"
    "--shell=none"
    "--"
    ,@command))
