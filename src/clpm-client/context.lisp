;;;; CLPM Contexts
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)

(defvar *context* nil
  "The default context in which to operate. Can either be a string (naming a
global CLPM context), the symbol :BUNDLE (which refers to the currently active
bundle, typically determined by the CLPM_BUNDLE_CLPMFILE environment variable),
or a pathname to a clpmfile.")

(defun context ()
  "Returns the current context. Returns *CONTEXT* if it is non-NIL. Otherwise
returns \"default\" or :BUNDLE."
  (or *context*
      (if (inside-bundle-exec-p)
          :bundle
          "default")))

(defun context-bundle-p (context)
  "A context names a bundle if it is the symbol :bundle or a pathname."
  (or (eql context :bundle)
      (pathnamep context)))

(defun context-asd-pathnames (&optional (context (context)))
  "Given a context, return a list of pathnames to .asd files installed in that
context."
  (assert (not (context-bundle-p context)))
  (with-clpm-proc (proc)
    (clpm-proc-print
     proc
     `(context-asd-pathnames ,context))
    (clpm-proc-read proc)))

(defun context-asd-directories (&optional (context (context)))
  "Return the directories containing the .asd files installed in CONTEXT."
  (asd-pathnames-to-directories (context-asd-pathnames context)))

(defun asd-pathnames-to-directories (pathnames)
  "Given a list of pathnames to .ASD files, return a list of pathnames to the
directories containing the files."
  (remove-duplicates (mapcar 'uiop:pathname-directory-pathname pathnames)
                     :test 'uiop:pathname-equal))

(defun context-find-system-asd-pathname (system-name &optional (context (context)))
  "Find the pathname to a system in the given context."
  (assert (not (context-bundle-p context)))
  (with-clpm-proc (proc)
    (clpm-proc-print
     proc
     `(context-find-system-asd-pathname ,context ,system-name))
    (clpm-proc-read proc)))

(defun context-source-registry (&optional (context (context)))
  "Return a source-registry form for the CONTEXT."
  (with-clpm-proc (proc)
    (clpm-proc-print
     proc
     (if (context-bundle-p context)
         `(with-bundle-default-pathname-defaults (,@(when (pathnamep context) (list context)))
            (with-bundle-local-config (,@(when (pathnamep context) (list context)))
              (bundle-source-registry ,(if (pathnamep context) context '(bundle-clpmfile-pathname)))))
         `(context-to-asdf-source-registry-form ,context)))
    (clpm-proc-read proc)))
