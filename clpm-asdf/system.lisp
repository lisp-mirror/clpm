;;;; System
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-asdf)

(defclass clpm-system (ops:release-system-inferred-system)
  ())

(defmethod ops:program-image-features (o (s clpm-system))
  (augment-features (clpm-features)))
