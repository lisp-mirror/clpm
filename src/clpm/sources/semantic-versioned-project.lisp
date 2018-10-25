(uiop:define-package #:clpm/sources/semantic-versioned-project
    (:use #:cl
          #:clpm/sources/defs
          #:clpm/version-strings)
  (:export #:semantic-versioned-release
           #:semantic-versioned-system-release))

(in-package #:clpm/sources/semantic-versioned-project)

(defclass semantic-versioned-release (clpm-release)
  ())

(defmethod release-satisfies-version-spec-p ((release semantic-versioned-release)
                                             version-spec)
  (version-spec-satisfied-p/semantic version-spec (release/version release)))

(defmethod release-> ((release-1 semantic-versioned-release)
                      (release-2 semantic-versioned-release))
  (assert (eql (release/project release-1) (release/project release-2)))
  (semantic-version< (release/version release-2)
                     (release/version release-1)))


(defclass semantic-versioned-system-release (clpm-system-release)
  ())

(defmethod system-release-satisfies-version-spec-p ((system-release semantic-versioned-system-release)
                                                    version-spec)
  (let ((system-version (system-release/system-version system-release)))
    (cond
      ((null version-spec)
       t)
      (system-version
       (version-spec-satisfied-p/semantic version-spec system-version))
      (t
       nil))))

(defmethod system-release-> ((system-release-1 semantic-versioned-system-release)
                             (system-release-2 semantic-versioned-system-release))
  (semantic-version< (system-release/system-version system-release-2)
                     (system-release/system-version system-release-1)))
