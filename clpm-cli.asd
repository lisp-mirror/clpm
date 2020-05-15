;;;; CLPM-CLI System Definition
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

#-:asdf3.2
(error "Requires ASDF >=3.2")

(in-package :asdf-user)

(defsystem #:clpm-cli
  :version (:read-file-form "src/clpm/version.lisp" :at (2 2))
  :description "Command Line Interface for CLPM"
  :license "BSD-2-Clause"
  :pathname #+clpm-logical-pathnames #p"clpm:cli;" #-clpm-logical-pathnames "cli/"
  :class :package-inferred-system
  :defsystem-depends-on (#:clpm-features #:deploy)
  :entry-point "clpm-cli/entry:main"
  :build-operation "deploy-op"
  :build-pathname "clpm"
  :depends-on (#:clpm-cli/clpm-cli))

(defmethod asdf:output-files ((o deploy:deploy-op) (c (eql (find-system "clpm-cli"))))
  (let ((file (print (merge-pathnames (asdf/system:component-build-pathname c)
                                      (merge-pathnames (uiop:ensure-directory-pathname "build/bin")
                                                       (asdf:system-source-directory c))))))
    (values (list file
                  (merge-pathnames (uiop:ensure-directory-pathname "lib/clpm")
                                   (uiop:pathname-parent-directory-pathname file)))
            T)))

(defmethod asdf:perform ((o deploy:deploy-op) (c (eql (find-system "clpm-cli"))))
  (let* ((bin-pathname (first (asdf:output-files o c)))
         (man-dir-pathname (merge-pathnames (uiop:make-pathname*
                                             :directory '(:relative :up "man" "man1"))
                                            (uiop:pathname-directory-pathname bin-pathname))))
    (uiop:call-function (uiop:find-symbol* :output-manual :clpm-cli/man)
                        man-dir-pathname))
  (call-next-method))
