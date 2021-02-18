;;;; When loaded, this builds the CLPM releases.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:cl-user)

(load (merge-pathnames "common.lisp"
                       *load-truename*))

(in-package #:clpm-scripts)

(setup-asdf)

;; TODO: This is a bit hacky, but speeds up the build significantly when
;; starting from scratch (like in CI). The root problem is that
;; asdf-release-ops will occasionally build the same code twice: once in the
;; child process and once in the parent. This is because we use
;; asdf:monolithic-lib-op in the parent. However, moving that op to the child
;; didn't quite work as it would error out due to package variance in
;; dexador...

(asdf:load-system :clpm)
(asdf:load-system :clpm-cli)

(asdf:operate 'asdf-release-ops:static-release-archive-op :clpm)
