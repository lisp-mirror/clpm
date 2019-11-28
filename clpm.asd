;;;; CLPM System Definition
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

#-:asdf3.2
(error "Requires ASDF >=3.2")

;; Not necessary, but easier to have when using SLIME.
(in-package :asdf-user)

(defsystem #:clpm
  :version (:read-file-form "src/clpm/version.lisp" :at (2 2))
  :description "A Common Lisp Package Manager"
  :license "BSD-2-Clause"
  :pathname "src/clpm/"
  :class :package-inferred-system
  :defsystem-depends-on (#:cffi-toolchain)
  :entry-point "clpm/cli/entry:main"
  :build-operation :program-op
  :depends-on (#:clpm/clpm)
  :in-order-to ((prepare-op (build-op :clpm-client))))

(defmethod asdf:operation-done-p ((o program-op) (c (eql (find-system "clpm"))))
  nil)

(defmethod perform ((o image-op) (c (eql (find-system "clpm"))))
  "Turn on compression for clpm images."
  (dump-image (output-file o c)
              :executable (typep o 'program-op)
              #+:sb-core-compression :compression #+:sb-core-compression t))

(defmethod asdf:output-files ((o program-op) (c (eql (find-system "clpm"))))
  (values (list "../../build/bin/clpm") t))
