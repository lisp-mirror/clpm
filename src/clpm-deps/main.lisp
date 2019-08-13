;;;; CLPM dependency groveler
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-deps/main
    (:nicknames #:clpm-deps)
  (:use #:cl)
  (:import-from #:uiop
                #:pathname-equal)
  (:export #:determine-systems-from-file
           #:safe-load-asd
           #:start-rel
           #:system-direct-deps))

(in-package #:clpm-deps/main)

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
         (direct-depends-on (asdf:system-depends-on system))
         (depends-on (if (typep system 'asdf:package-inferred-system)
                         (chase-package-inferred-deps direct-depends-on system)
                         direct-depends-on)))

    `(,(asdf:component-name system)
      :depends-on ,depends-on
      :version ,(asdf:component-version system)
      :defsystem-depends-on ,defsystem-deps
      :source-file ,source-file
      :loaded-systems ,(gethash source-file *directly-loaded-systems*))))

(defun safe-load-asd (pathname)
  "Attempt to load the asd file located at PATHNAME. Returns three values, the
first is T iff the file was loaded successfully. The second value is a backtrace
if an unknown error occurred, the third is a missing system."
  (handler-case
      (let ((*standard-output* (make-broadcast-stream))
            ;;(*error-output* (make-broadcast-stream))
            (*terminal-io* (make-broadcast-stream))
            (*file-being-loaded* pathname))
        (asdf:load-asd pathname)
        (values t nil nil))
    (asdf:missing-component (c)
      (values nil nil (string-downcase (string (asdf/find-component:missing-requires c)))))
    (error (c)
      (values nil (with-output-to-string (s)
                    (uiop:print-condition-backtrace c :stream s))
              nil))))

(defun determine-systems-from-file (asd-pathname)
  (remove-if-not (lambda (system-name)
                   (let* ((system (asdf:find-system system-name))
                          (system-source-file (asdf:system-source-file system)))
                     (pathname-equal asd-pathname system-source-file)))
                 (asdf:registered-systems)))

(defun start-rel ()
  "Some lisp's like to always print a prompt at their REPL. To avoid needing to
handle skipping this prompt when READ'ing the groveler's output, we run our own
REL (READ EVAL LOOP) instead. All prints must be explcit."
  (loop
    (let ((input (read)))
      (eval input)
      (finish-output))))
