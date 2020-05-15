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
  :pathname #+clpm-logical-pathnames #p"clpm:src;clpm;" #-clpm-logical-pathnames "src/clpm/"
  :class :package-inferred-system
  :defsystem-depends-on (#:clpm-build)
  :depends-on (#:clpm/clpm
               (:feature :clpm-curl #:clpm-multi-http-client-impl/curl)
               (:feature :clpm-dexador #:clpm-multi-http-client-impl/dexador)
               (:feature :clpm-drakma #:clpm-multi-http-client-impl/drakma)
               (:feature :clpm-firejail #:clpm/sandbox/firejail)
               (:feature :clpm-openssl #:clpm/http-client/cl-plus-ssl)))
