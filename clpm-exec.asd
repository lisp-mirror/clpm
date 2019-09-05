;;;; CLPM Executable System Definitions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

#-:asdf3.2
(error "Requires ASDF >=3.2")

;; Not necessary, but easier to have when using SLIME.
(in-package :asdf-user)

(defsystem #:clpm-exec
  :license "BSD-2-Clause")

(defsystem #:clpm-exec/dynamic-libs
  :license "BSD-2-Clause"
  :defsystem-depends-on (#:cffi-toolchain)
  :entry-point "clpm/cli/entry:main"
  :build-operation :static-program-op
  :build-pathname "build/bin/dynamic/clpm"
  :depends-on (#:clpm))

(defmethod asdf:operation-done-p ((o image-op) (c (eql (find-system "clpm-exec/dynamic-libs"))))
  nil)

(defmethod perform ((o image-op) (c (eql (find-system "clpm-exec/dynamic-libs"))))
  "Turn on compression for clpm images."
  (dump-image (output-file o c)
              :executable (typep o 'program-op)
              #+:sb-core-compression :compression #+:sb-core-compression t))

(defmethod cffi-toolchain:static-image-new-features
    (o (s (eql (find-system "clpm-exec/dynamic-libs"))))
  "Prevent cl-ssl from loading the foreign libraries at build time."
  `(:cl+ssl-foreign-libs-already-loaded
    ,@(when (uiop:os-windows-p)
        `(:drakma-no-ssl))))

(defmethod cffi-toolchain:static-image-remove-features-on-dump
    (o (s (eql (find-system "clpm-exec/dynamic-libs"))))
  "The libraries are not truly loaded, we just tricked cl+ssl, so remove that
feature on dump."
  (list :cl+ssl-foreign-libs-already-loaded))

(defsystem #:clpm-exec/static-libs
  :license "BSD-2-Clause"
  :defsystem-depends-on (#:cffi-toolchain)
  :entry-point "clpm/cli/entry:main"
  :build-operation :static-program-op
  :build-pathname "build/bin/static/clpm"
  :depends-on (#:clpm))

(defmethod operation-done-p ((o image-op) (c (eql (find-system "clpm-exec/static-libs"))))
  nil)

(defmethod perform ((o image-op) (c (eql (find-system "clpm-exec/static-libs"))))
  "Turn on compression for clpm images."
  ;; TODO: Actually implement a search for the right directory... This should
  ;; work on most GNU/Linux systems for now.
  (uiop:register-image-restore-hook (lambda () (setf (uiop:getenv "SSL_CERT_DIR") "/etc/ssl/certs")) nil)
  (dump-image (output-file o c)
              :executable (typep o 'program-op)
              #+:sb-core-compression :compression #+:sb-core-compression t))

(defmethod perform ((o cffi-toolchain:static-runtime-op) (s (eql (find-system "clpm-exec/static-libs"))))
  (cffi-toolchain:link-lisp-executable
   (output-file o s)
   (loop
     :for lib :in `(,(first (input-files o s))
                    ,@(unless (uiop:os-windows-p)
                        '("-l:libcrypto.a"
                          "-l:libssl.a")))
     :appending (cffi-toolchain::link-all-library lib))))

(defmethod cffi-toolchain:static-image-new-features (o (s (eql (find-system "clpm-exec/static-libs"))))
  `(:cl+ssl-foreign-libs-already-loaded
    ,@(when (uiop:os-windows-p)
        '(:drakma-no-ssl))))
