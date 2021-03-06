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
  :version (:read-file-form "version.lisp-expr")
  :description "A Common Lisp Project Manager"
  :license "BSD-2-Clause"
  :pathname "clpm/"
  :entry-point "clpm-cli/entry:main"
  :class "clpm-asdf:clpm-system"
  :build-operation "asdf-release-ops:dynamic-program-op"
  :build-pathname #-os-windows "../build/bin/clpm" #+os-windows "../build/bin/clpm.exe"
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

  :release-structure
  ((:clpm-install-script "install.sh" :if-feature (:not :os-windows))
   (:module "bin"
    :components
    ((:program-file "clpm")))
   (:module "share"
    :components
    ( ;; (:man-directory "man")
     (:module "clpm"
      :components
      ((:clpm-client-directory "client")))
     (:module "doc"
      :components
      ((:module "clpm"
        :append-version t
        :components ((:license-file "LICENSE")
                     (:readme-file "README")
                     (:dependencies-license-file "BUNDLED-LICENSES"))))))))
  :in-order-to ((program-op (load-op :clpm-cli))
                (asdf-release-ops:perform-program-image-op (load-op :clpm-cli))
                (test-op (asdf-release-ops:dynamic-program-op :clpm)
                         (load-op :clpm-test)))

  :perform (test-op (o c)
                    (funcall (uiop:find-symbol* :run-tests-with-server :clpm-test)
                             :clpm (output-file 'asdf-release-ops:dynamic-program-op c))))

(defsystem #:clpm/client-helper
  :description "Helper to make sure clpm-client is built before clpm/client is
  loaded. This is nasty and I really want to remove it soon."
  :in-order-to ((load-op (concatenate-source-op :clpm-client)
                         (clpm-asdf:build-clpm-client-tarball-op :clpm))))
