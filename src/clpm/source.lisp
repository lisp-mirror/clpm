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
  (:reexport #:clpm/sources/vcs))

(in-package #:clpm/source)
