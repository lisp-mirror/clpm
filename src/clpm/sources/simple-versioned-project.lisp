(uiop:define-package #:clpm/sources/simple-versioned-project
    (:use #:cl
          #:clpm/sources/defs
          #:clpm/version-strings)
  (:export #:simple-versioned-release))

(in-package #:clpm/sources/simple-versioned-project)

(defclass simple-versioned-release (clpm-release)
  ())

(defmethod release-satisfies-version-spec-p ((release simple-versioned-release)
                                             version-spec)
  (version-spec-satisfied-p/simple-string version-spec (release/version release)))

(defmethod release-> ((release-1 simple-versioned-release)
                      (release-2 simple-versioned-release))
  (assert (eql (release/project release-1) (release/project release-2)))
  (string> (release/version release-1)
           (release/version release-2)))
