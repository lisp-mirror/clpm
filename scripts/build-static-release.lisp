;;;; When loaded, this builds the CLPM releases.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:cl-user)

(load (merge-pathnames "common.lisp"
                       *load-truename*))

(in-package #:clpm-scripts)

(setup-asdf)

(asdf:load-system :asdf-release-ops)

(asdf:operate 'asdf-release-ops:static-release-archive-op :clpm)
