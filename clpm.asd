;;;; CLPM System Definition
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

#-:asdf3.2
(error "Requires ASDF >=3.2")

;; Not necessary, but easier to have when using SLIME.
(in-package :asdf-user)

(load-system :clpm-asdf)

(defsystem #:clpm
  :version (:read-file-form "clpm/version.lisp" :at (2 2))
  :description "A Common Lisp Package Manager"
  :license "BSD-2-Clause"
  :pathname "clpm/"
  :entry-point "clpm-cli/entry:main"
  :class "clpm-asdf:clpm-system"
  :build-operation "asdf-release-ops:dynamic-program-op"
  :defsystem-depends-on (#:clpm-asdf)
  :depends-on (#:clpm/clpm
               (:feature :clpm-curl #:clpm-multi-http-client-impl/curl)
               (:feature :clpm-dexador #:clpm-multi-http-client-impl/dexador)
               (:feature :clpm-drakma #:clpm-multi-http-client-impl/drakma)
               (:feature :clpm-firejail #:clpm/sandbox/firejail)
               (:feature :clpm-openssl #:clpm/http-client/cl-plus-ssl))

  :release-license-file "../LICENSE"
  :release-readme-file "../README.org"
  :release-staging-directory "../build/release-staging/"
  :release-directory "../releases/"
  :in-order-to ((program-op (load-op :clpm-cli))
                (asdf-release-ops:perform-program-image-op (load-op :clpm-cli))))
