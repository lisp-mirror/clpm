;;;; ASDF extensions for CLPM
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-asdf/clpm-asdf
    (:nicknames #:clpm-asdf)
  (:use #:cl
        #:clpm-asdf/concatenate-package-inferred-system-source-op
        #:clpm-asdf/concatenate-source-deliver-asd-op
        #:clpm-asdf/concatenate-source-deliver-asd-tarball-op)
  (:reexport #:clpm-asdf/concatenate-package-inferred-system-source-op
             #:clpm-asdf/concatenate-source-deliver-asd-op
             #:clpm-asdf/concatenate-source-deliver-asd-tarball-op))

(in-package #:clpm-asdf/clpm-asdf)
