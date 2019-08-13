;;;; Interface for caching the clpm-deps system, used to grovel system info
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/deps
    (:use #:cl
          #:alexandria
          #:clpm/cache
          #:clpm/log)
  (:export #:deps-asd-pathname
           #:deps-version-string
           #:ensure-deps-system-in-cache!))

(in-package #:clpm/deps)

(setup-logger)

(defvar *deps-system* nil
  "A string containing the contents of the groveler's clpm-deps.asd. Populated
at image dump.")
(defvar *deps-source* nil
  "A string containing the contents of the groveler's main.lisp. Populated at
image dump.")
(defvar *deps-version* nil
  "A string containing the contents of the groveler's version.sexp. Populated at
image dump.")

(defun read-deps-system-string ()
  "Read clpm-deps.asd from file as a string."
  (uiop:read-file-string (asdf:system-relative-pathname :clpm "src/clpm-deps/clpm-deps.asd")))
(defun read-deps-source-string ()
  "Read main.lisp from file as a string."
  (uiop:read-file-string (asdf:system-relative-pathname :clpm "src/clpm-deps/main.lisp")))
(defun read-deps-version-string ()
  "Read version.sexp from file as a string."
  (uiop:read-file-form (asdf:system-relative-pathname :clpm "src/clpm-deps/version.sexp")))

(defun cache-files! ()
  "Cache the groveler files in the variables."
  (setf *deps-system* (read-deps-system-string))
  (setf *deps-source* (read-deps-source-string))
  (setf *deps-version* (read-deps-version-string)))
(uiop:register-image-dump-hook 'cache-files!)

(defun deps-system-string ()
  "Return clpm-deps.asd as a string. Prefers cached version instead of reading
from file."
  (or *deps-system*
      (read-deps-system-string)))
(defun deps-source-string ()
  "Return main.lisp as a string. Prefers cached version instead of reading from
file."
  (or *deps-source*
      (read-deps-source-string)))
(defun deps-version-string ()
  "Return version.sexp as a string. Prefers cached version instead of reading
from file."
  (or *deps-version*
      (read-deps-version-string)))

(defparameter *force-deps-cache-writing-p* t
  "If T, ~ensure-deps-system-in-cache!~ defaults to writing files to the cache
even if they exist.")

(defun unforce-cache-writing ()
  "Set ~*force-deps-cache-writing-p*~ to NIL."
  (setf *force-deps-cache-writing-p* nil))
(uiop:register-image-dump-hook 'unforce-cache-writing)

(defun deps-asd-pathname ()
  "Return the pathname to the deps .asd file."
  (clpm-cache-pathname
   (list "deps-groveler-system"
         (deps-version-string)
         "clpm-deps.asd")))

(defun ensure-deps-system-in-cache! (&optional (forcep *force-deps-cache-writing-p*))
  "Ensure the groveler source code is present in the cache. If ~forcep~ is
non-NIL, overwrite."
  (let* ((deps-version (deps-version-string))
         (deps-system-pathname (clpm-cache-pathname
                                (list
                                 "deps-groveler-system"
                                 deps-version
                                 "clpm-deps.asd")))
         (deps-source-pathname (clpm-cache-pathname
                                (list
                                 "deps-groveler-system"
                                 deps-version
                                 "main.lisp")))
         (deps-version-pathname (clpm-cache-pathname
                                 (list
                                  "deps-groveler-system"
                                  deps-version
                                  "version.sexp"))))
    (ensure-directories-exist deps-system-pathname)
    (unless (and (not forcep)
                 (probe-file deps-system-pathname))
      (write-string-into-file (deps-system-string) deps-system-pathname :if-exists :supersede))
    (unless (and (not forcep)
                 (probe-file deps-source-pathname))
      (write-string-into-file (deps-source-string) deps-source-pathname :if-exists :supersede))
    (unless (and (not forcep)
                 (probe-file deps-version-pathname))
      (with-open-file (s deps-version-pathname :direction :output :if-exists :supersede)
        (print deps-version s)))))
