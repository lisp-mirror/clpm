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

(defun env-var-name (s)
  (let ((pos (position #\= s)))
    (subseq s 0 pos)))

(defun run-program-augment-env-args (new-env-alist)
  "Given an alist of environment variables, return a list of arguments suitable
for ~uiop:{launch/run}-program~ to set the augmented environment for the child
process."
  (let* ((inherited-env
           (remove-if (lambda (x)
                        (let ((name (env-var-name x)))
                          (member name new-env-alist :test #'equal :key #'car)))
                      (sb-ext:posix-environ)))
         (env (append (mapcar (lambda (c)
                                (concatenate 'string (car c) "=" (cdr c)))
                              new-env-alist)
                      inherited-env)))
    (list :environment env)))

(defun run-commands-in-child-lisp (args &key env)
  (flet ((vomit (stream)
           (with-safe-io-syntax ()
             (loop
               :with out := (make-broadcast-stream stream *stdout*)
               :for arg :in args
               :do
                  (print arg out)))))
    (apply #'run-program
           (list "/home/etimmons/common-lisp/clpm/scripts/clpm-live")
           :output :interactive
           :error-output :interactive
           :input #'vomit
           (run-program-augment-env-args (list* '("CLPM_LIVE_PRIVATE_REPL" . "true") env)))))

(defun build-clpm ()
  (let ((clpm-pathname
          #-:os-windows
          (merge-pathnames "bin/clpm" *build-root-pathname*)
          #+:os-windows
          (merge-pathnames "bin/clpm.exe" *build-root-pathname*))
        (cache-pathname (merge-pathnames "cl-cache/" *build-root-pathname*)))
    (format *stderr* "Building `clpm` executable at ~A~%" clpm-pathname)
    (run-commands-in-child-lisp
     `((defmethod asdf:output-files ((,(gensym) asdf:program-op) (,(gensym) (eql (asdf:find-system :clpm))))
         (values (list ,clpm-pathname) t))
       (when (probe-file ,clpm-pathname)
         (delete-file ,clpm-pathname))

       (defmethod asdf:perform ((cl-user::o asdf:image-op) (cl-user::c asdf:system))
         (uiop:dump-image (asdf:output-file cl-user::o cl-user::c) :executable (typep cl-user::o 'asdf:program-op) #+:sb-core-compression ,@(list :compression t)))

       (asdf:operate 'asdf:program-op :clpm))
     :env `(("ASDF_OUTPUT_TRANSLATIONS"
             . ,(format nil "(:output-translations :ignore-inherited-configuration (:root (\"~A\" :implementation :**/ :*.*.*)))"
                        (namestring cache-pathname)))))))

(defun build (&key
                (root-pathname *root-pathname*)
                (build-root-pathname *build-root-pathname*))
  (let ((*root-pathname* (ensure-directory-pathname root-pathname))
        (*build-root-pathname* (ensure-directory-pathname build-root-pathname)))
    (format *stderr*
            "I will build CLPM from sources located at ~A~%The build files will be located at ~A~%~%~%"
            *root-pathname*
            *build-root-pathname*)

    (build-clpm)))
