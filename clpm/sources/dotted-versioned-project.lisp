;;;; Dotted versioned projects
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/sources/dotted-versioned-project
    (:use #:cl
          #:clpm/sources/defs
          #:clpm/version-strings)
  (:export #:dotted-versioned-release
           #:dotted-versioned-system-release))

(in-package #:clpm/sources/dotted-versioned-project)

(defclass dotted-versioned-release (clpm-release)
  ())

(defmethod release-satisfies-version-spec-p ((release dotted-versioned-release)
                                             version-spec)
  (version-spec-satisfied-p/dotted version-spec (release-version release)))

(defmethod release-> ((release-1 dotted-versioned-release)
                      (release-2 dotted-versioned-release))
  (assert (eql (release-project release-1) (release-project release-2)))
  (uiop:version< (release-version release-2)
                 (release-version release-1)))


(defclass dotted-versioned-system-release (clpm-system-release)
  ())

(defmethod system-release-satisfies-version-spec-p ((system-release dotted-versioned-system-release)
                                                    version-spec)
  (let ((system-version (system-release-system-version system-release)))
    (cond
      ((null version-spec)
       t)
      (system-version
       (version-spec-satisfied-p/dotted version-spec system-version))
      (t
       nil))))

(defmethod system-release-> ((system-release-1 dotted-versioned-system-release)
                             (system-release-2 dotted-versioned-system-release))
  (uiop:version< (system-release-system-version system-release-2)
                 (system-release-system-version system-release-1)))
