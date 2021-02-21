;;;; System
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-asdf)

(defclass clpm-system (ops:release-system-inferred-system)
  ())

(defmethod ops:program-image-features (o (s clpm-system))
  (augment-features (clpm-features)))

(defmethod ops:program-static-image-features (o (s clpm-system))
  (list :cl+ssl-foreign-libs-already-loaded))

(defmethod ops::release-system-version-designator ((s clpm-system))
  (clpm-version))
