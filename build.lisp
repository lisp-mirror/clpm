;;;; CLPM Build Helper

(require :asdf)

(defpackage #:clpm-build
  (:use #:cl)
  (:import-from #:uiop
                #:*stderr*
                #:*stdout*
                #:directory-exists-p
                #:ensure-directory-pathname
                #:find-symbol*
                #:getenv
                #:native-namestring
                #:pathname-directory-pathname
                #:run-program
                #:with-current-directory
                #:with-safe-io-syntax)
  (:export #:build))

(in-package #:clpm-build)

(defvar *setup-file-pathname* *load-truename*
  "The pathname to this file.")

(defvar *root-pathname* (pathname-directory-pathname *setup-file-pathname*)
  "The pathname to the root directory of the CLPM release being built.")

(defvar *build-root-pathname* (merge-pathnames "build/"
                                               *root-pathname*)
  "The pathname to the root of the build directory. Defaults to build/ inside
*ROOT-PATHNAME*")

(defun run-commands-in-child-lisp (args &key env (lisp "sbcl"))
  (loop
    :for (name . val) :in env
    :do (setf (getenv name) val))
  (flet ((vomit (stream)
           (with-safe-io-syntax ()
             (loop
               :for arg :in args
               :do
                  (print arg stream)
                  (print arg *stdout*)))))
    (run-program `(,lisp "--no-userinit" "--no-sysinit" "--disable-debugger")
                 :output :interactive
                 :error-output :interactive
                 :input #'vomit)))

(defun git-exists-p ()
  (ignore-errors
   (run-program '("git" "--version"))
   t))

(defun git-version ()
  (when (and (directory-exists-p (merge-pathnames ".git/"
                                                  *root-pathname*))
             (git-exists-p))
    (with-current-directory (*root-pathname*)
      (let ((commit (run-program '("git" "rev-parse" "--short" "HEAD")
                                 :output '(:string :stripped t)))
            (dirty-p (not (zerop (nth-value 2
                                            (run-program '("git" "diff-index" "--quiet" "HEAD")
                                                         :ignore-error-status t))))))
        (format nil "~A~:[-dirty~;~]" commit dirty-p)))))

(defun build-clpm ()
  (let ((clpm-pathname
          #-:os-windows
          (merge-pathnames "bin/clpm" *build-root-pathname*)
          #+:os-windows
          (merge-pathnames "bin/clpm.exe" *build-root-pathname*))
        (clpm-client-pathname (merge-pathnames "bin/clpm-client.lisp" *build-root-pathname*))
        (clpm-deps-root (merge-pathnames "src/clpm-deps/" *root-pathname*))
        (cache-pathname (merge-pathnames "cl-cache/" *build-root-pathname*)))
    (format *stderr* "Building `clpm` executable at ~A~%" clpm-pathname)
    (run-commands-in-child-lisp
     `((require :asdf)
       (require :sb-posix)
       (require :sb-bsd-sockets)
       (format t "CL_SOURCE_REGISTRY: ~S~%" (getenv "CL_SOURCE_REGISTRY"))
       (format t "ASDF_OUTPUT_TRANSLATIONS: ~S~%" (getenv "ASDF_OUTPUT_TRANSLATIONS"))
       (when (uiop:featurep :os-windows)
         (pushnew :drakma-no-ssl *features*))
       (declaim (optimize debug))
       (asdf:register-immutable-system :sb-posix)
       (asdf:register-immutable-system :sb-bsd-sockets)
       (asdf:load-system :clpm)
       ;; (defmethod asdf:output-files ((,(gensym) asdf:monolithic-concatenate-source-op)
       ;;                               (,(gensym) (eql (asdf:find-system :clpm-client))))
       ;;   (values (list ,clpm-client-pathname) t))
       (defmethod asdf:output-files ((,(gensym) asdf:program-op) (,(gensym) (eql (asdf:find-system :clpm))))
         (values (list ,clpm-pathname) t))
       (when (probe-file ,clpm-pathname)
         (delete-file ,clpm-pathname))
       ;; (asdf:operate 'asdf:monolithic-concatenate-source-op :clpm-client)
       ;; (setf (symbol-value (uiop:find-symbol* :*client-contents* :clpm/client))
       ;;       (uiop:read-file-string ,clpm-client-pathname))

       (defmethod asdf:perform ((cl-user::o asdf:image-op) (cl-user::c asdf:system))
         (uiop:dump-image (asdf:output-file cl-user::o cl-user::c) :executable (typep cl-user::o 'asdf:program-op) #+:sb-core-compression ,@(list :compression t)))
       ;; (uiop:register-image-restore-hook
       ;;  (lambda ()
       ;;    (eval `(trace ,(uiop:find-symbol* :filter-requirements :clpm/resolve))))
       ;;  nil)
       (asdf:operate 'asdf:program-op :clpm))
     :env `(("CL_SOURCE_REGISTRY"
             . ,(format nil "~A/"
                        (namestring *root-pathname*)))
            ("ASDF_OUTPUT_TRANSLATIONS"
             . ,(format nil "(:output-translations :ignore-inherited-configuration (:root (\"~A\" :implementation :**/ :*.*.*)))"
                        (namestring cache-pathname)))))))

(defun build (&key
                (root-pathname *root-pathname*)
                (build-root-pathname *build-root-pathname*))
  (let ((*root-pathname* (ensure-directory-pathname root-pathname))
        (*build-root-pathname* (ensure-directory-pathname build-root-pathname)))
    (format *stderr*
            "I will build CLPM from sources located at ~A~%The build file will be located at ~A~%~%~%"
            *root-pathname*
            *build-root-pathname*)

    (build-clpm)))
