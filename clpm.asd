;;;; CLPM System Definition
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

#-:asdf3.1
(error "Requires ASDF >=3.1")

;; Not necessary, but easier to have when using SLIME.
(in-package :asdf-user)

(defsystem #:clpm
  :version (:read-file-form "src/clpm/version.lisp" :at (2 2))
  :description "A Common Lisp package Manager"
  :license "BSD-2-Clause"
  :pathname "src/clpm/"
  :class :package-inferred-system
  :defsystem-depends-on (#:cffi-toolchain)
  :entry-point "clpm/cli/entry:main"
  :build-operation :load-op
  :in-order-to ((build-op (build-op :clpm-exec/dynamic-libs)
                          (build-op :clpm-exec/static-libs)))
  :depends-on (#:clpm/clpm))

(defmethod output-files ((o cffi-toolchain:static-program-op) (c (eql (find-system "clpm"))))
  (values (list (merge-pathnames "build/bin/clpm"
                                 (pathname-directory-pathname (system-source-file "clpm"))))
          t))

(defmethod perform ((o image-op) (c (eql (find-system "clpm"))))
  "Turn on compression for clpm images."
  (dump-image (output-file o c)
              :executable (typep o 'program-op)
              #+:sb-core-compression :compression #+:sb-core-compression t))
