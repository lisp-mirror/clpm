;;;; CLPM dependency groveler
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-groveler/main
    (:nicknames #:clpm-groveler)
  (:use #:cl)
  (:import-from #:uiop
                #:pathname-equal)
  (:export #:determine-systems-from-file
           #:safe-load-asd
           #:start-rel
           #:system-direct-deps))

(in-package #:clpm-groveler)

(defvar *directly-loaded-systems* (make-hash-table :test 'equal))
(defvar *file-being-loaded* nil)

(defmethod asdf:operate :around ((op asdf:load-op) c &key &allow-other-keys)
  (if (or (null *file-being-loaded*)
          (equalp "asdf" (asdf:component-name c)))
      (call-next-method)
      (progn
        (pushnew (asdf:component-name c) (gethash *file-being-loaded* *directly-loaded-systems*)
                 :test 'equalp)
        (let ((*file-being-loaded* nil))
          (call-next-method)))))


(defvar *cache* (make-hash-table :test 'equalp))

;; (defun chase-package-inferred-deps (system-name parent-system-name)
;;   (if (gethash system-name *cache*)
;;       nil
;;       (setf (gethash system-name *cache*)
;;             (cond
;;               ((listp system-name)
;;                (list system-name))
;;               ((string= parent-system-name
;;                         (asdf:primary-system-name system-name))
;;                (mapcan (lambda (d)
;;                          (chase-package-inferred-deps d parent-system-name))
;;                        (asdf:system-depends-on (asdf:find-system system-name))))
;;               (t
;;                (list system-name))))))

(defgeneric asdf-dependency-system-name (dep))

(defmethod asdf-dependency-system-name ((dep string))
  dep)

(defmethod asdf-dependency-system-name ((dep list))
  (case (first dep)
    (:version
     (second dep))
    (t
     nil)))

(defun clean-up-deps-list (dependencies)
  (remove-duplicates
   (remove-if (lambda (x)
                (gethash x *cache*))
              dependencies)
   :test #'equalp))

(defun chase-package-inferred-deps (dependencies system)
  (let ((dep-with-same-primary-system (find-if (lambda (x)
                                                 (equal (asdf:primary-system-name system)
                                                        (asdf:primary-system-name
                                                         (asdf-dependency-system-name x))))
                                               dependencies)))
    (if dep-with-same-primary-system
        ;; There is a dependency with the same primary system name. Replace it
        ;; with its dependencies.
        (let ((new-dependencies
                (append (asdf:system-depends-on
                         (asdf:find-system (asdf-dependency-system-name dep-with-same-primary-system)))
                        (remove dep-with-same-primary-system dependencies))))
          (setf (gethash dep-with-same-primary-system *cache*) t)
          (chase-package-inferred-deps (clean-up-deps-list new-dependencies) system))
        dependencies)))

(defun system-direct-deps (system)
  (let* ((defsystem-deps (asdf:system-defsystem-depends-on system))
         (source-file (asdf:system-source-file system))
         (*cache* (make-hash-table :test 'equalp))
         (depends-on (asdf:system-depends-on system)))

    `(,(asdf:component-name system)
      :depends-on ,depends-on
      :version ,(asdf:component-version system)
      :defsystem-depends-on ,defsystem-deps
      :source-file ,source-file
      :loaded-systems ,(gethash source-file *directly-loaded-systems*))))

(defun call-with-error-handling (thunk)
  (handler-case
      (values (funcall thunk) nil nil)
    (asdf:missing-component (c)
      (values nil nil (string-downcase (string (asdf/find-component:missing-requires c)))))
    (error (c)
      (values nil (with-output-to-string (s)
                    (uiop:print-condition-backtrace c :stream s))
              nil))))

(defmacro with-error-handling (nil &body body)
  `(call-with-error-handling (lambda () ,@body)))

(defun safe-load-asd (pathname)
  "Attempt to load the asd file located at PATHNAME. Returns three values, the
first is T iff the file was loaded successfully. The second value is a backtrace
if an unknown error occurred, the third is a missing system."
  (with-error-handling ()
    (let ((*standard-output* (make-broadcast-stream))
          (*error-output* (make-broadcast-stream))
          (*terminal-io* (make-broadcast-stream))
          (*file-being-loaded* pathname))
      (asdf:load-asd pathname)
      t)))

(defun lisp-file-wilden (pathname)
  (uiop:merge-pathnames*
   (uiop:merge-pathnames* (uiop:make-pathname* :name uiop:*wild*
                                               :type "lisp")
                          uiop:*wild-inferiors*)
   pathname))

(defun chase-primary-system-package-inferred-systems (system-name)
  ;; Get the primary system, look at at its pathname and find any lisp files
  ;; that could be sub systems.
  (let* ((system (asdf:find-system system-name))
         (root-pathname (asdf:component-pathname system))
         (all-lisp-files (uiop:directory* (lisp-file-wilden root-pathname))))
    (remove nil
            (mapcar (lambda (pn)
                      (let* ((enough (enough-namestring pn root-pathname))
                             (system-name (uiop:strcat system-name "/"
                                                       ;; Trim the .lisp from the end.
                                                       (subseq enough 0 (- (length enough) 5)))))
                        ;; Make sure we can load a package inferred system from
                        ;; this file. (We've all done it: accidentally included
                        ;; an empty file or one with a mangled defpackage...)
                        (ignore-errors
                         (asdf:find-system system-name t)
                         system-name)))
                    all-lisp-files))))

(defun determine-systems-from-file (asd-pathname)
  (with-error-handling ()
    (let* ((systems (remove-if-not (lambda (system-name)
                                     (let* ((system (asdf:find-system system-name))
                                            (system-source-file (asdf:system-source-file system)))
                                       (pathname-equal asd-pathname system-source-file)))
                                   (asdf:registered-systems)))
           ;; If any of the systems are primary systems that are also package
           ;; inferred systems, we need to chase down all the secondary systems
           ;; coming from them...
           (primary-package-inferred-system-names
             (remove-if-not (lambda (system-name)
                              (and (equal system-name (asdf:primary-system-name system-name))
                                   (typep (asdf:find-system system-name) 'asdf:package-inferred-system)))
                            systems))
           (secondary-package-inferred-system-names
             (mapcan #'chase-primary-system-package-inferred-systems primary-package-inferred-system-names)))
      (remove-duplicates (append systems secondary-package-inferred-system-names) :test #'equal))))

(defun start-rel ()
  "Some lisp's like to always print a prompt at their REPL. To avoid needing to
handle skipping this prompt when READ'ing the groveler's output, we run our own
REL (READ EVAL LOOP) instead. All prints must be explcit."
  (loop
    (let ((input (read)))
      (eval input)
      (finish-output))))
