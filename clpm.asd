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
  :defsystem-depends-on (#:clpm-build #:deploy)
  :entry-point "clpm/cli/entry:main"
  :build-operation "deploy-op"
  :build-pathname #+win32 "clpm.exe" #-win32 "clpm"
  :depends-on (#:clpm/clpm
               (:feature :clpm-curl #:clpm/http-client/curl)
               (:feature :clpm-dexador #:clpm/http-client/dexador)
               (:feature :clpm-drakma #:clpm/http-client/drakma)
               (:feature :clpm-firejail #:clpm/sandbox/firejail)))

(defmethod asdf:output-files ((o deploy:deploy-op) (c (eql (find-system "clpm"))))
  (let ((file (print (merge-pathnames (asdf/system:component-build-pathname c)
                                      (merge-pathnames (uiop:ensure-directory-pathname "build/bin")
                                                       (asdf:system-source-directory c))))))
    (values (list file
                  (merge-pathnames (uiop:ensure-directory-pathname "lib/clpm")
                                   (uiop:pathname-parent-directory-pathname file)))
            T)))

(defmethod asdf:perform ((o deploy:deploy-op) (c (eql (find-system "clpm"))))
  (let* ((bin-pathname (first (asdf:output-files o c)))
         (man-dir-pathname (merge-pathnames (uiop:make-pathname*
                                             :directory '(:relative :up "man" "man1"))
                                            (uiop:pathname-directory-pathname bin-pathname))))
    (uiop:call-function (uiop:find-symbol* :output-manual :clpm/man)
                        man-dir-pathname))
  (call-next-method))
