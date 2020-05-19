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
           #:find-system-asd-pathname
           #:installed-system-names
           #:output-translations
           #:source-registry
           #:visible-primary-system-names))

(in-package #:clpm/context-queries)

(defun asd-pathnames (&key context)
  (with-clpm-session ()
    (context-asd-pathnames (get-context context))))

(defun find-system-asd-pathname (system-name &key context)
  (with-clpm-session ()
    (context-find-system-asd-pathname (get-context context) system-name)))

(defun installed-system-names (&key context)
  (mapcar 'system-name (context-installed-systems (get-context context))))

(defun output-translations (&key context)
  (with-clpm-session ()
    (context-output-translations (get-context context))))

(defun source-registry (&key context with-client-p
                          ignore-inherited-source-registry
                          splice-inherited)
  (with-clpm-session ()
    (context-to-asdf-source-registry-form
     (get-context context)
     :with-client with-client-p
     :ignore-inherited ignore-inherited-source-registry
     :splice-inherited splice-inherited)))

(defun visible-primary-system-names (&key context)
  (context-visible-primary-system-names (get-context context)))
