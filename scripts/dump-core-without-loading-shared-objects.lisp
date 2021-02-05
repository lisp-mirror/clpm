;;;; When loaded, this dumps a core for CLPM that does not load shared objects.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:cl-user)

(dolist (lib sb-alien::*shared-objects*)
  (setf (sb-alien::shared-object-dont-save lib) t))

(pushnew :cl+ssl-foreign-libs-already-loaded *features*)

(asdf:operate 'clpm-cli/clpm-program-op:clpm-core-op :clpm-cli)
