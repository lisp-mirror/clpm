;;;; Context Queries
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/context-queries
    (:use #:cl
          #:clpm/context
          #:clpm/session
          #:clpm/source)
  (:export #:asd-pathnames
           #:editable-primary-system-names
           #:find-system-asd-pathname
           #:installed-system-names
           #:output-translations
           #:source-registry
           #:visible-primary-system-names))

(in-package #:clpm/context-queries)

(defun asd-pathnames (&key context)
  (with-clpm-session ()
    (with-context (context)
      (context-asd-pathnames context))))

(defun editable-primary-system-names (&key context)
  (with-clpm-session ()
    (with-sources-using-installed-only ()
      (with-context (context)
        (context-editable-primary-system-names context)))))

(defun find-system-asd-pathname (system-name &key context)
  (with-clpm-session ()
    (with-context (context)
      (context-find-system-asd-pathname context system-name))))

(defun installed-system-names (&key context)
  (with-clpm-session ()
    (with-context (context)
      (mapcar 'system-name (context-installed-systems context)))))

(defun output-translations (&key context)
  (with-clpm-session ()
    (with-context (context)
      (context-output-translations context))))

(defun source-registry (&key context with-client-p
                          ignore-inherited-source-registry
                          splice-inherited)
  (with-clpm-session ()
    (with-sources-using-installed-only ()
      (with-context (context)
        (context-to-asdf-source-registry-form
         context
         :with-client with-client-p
         :ignore-inherited ignore-inherited-source-registry
         :splice-inherited splice-inherited)))))

(defun visible-primary-system-names (&key context)
  (with-clpm-session ()
    (with-context (context)
      (context-visible-primary-system-names context))))
