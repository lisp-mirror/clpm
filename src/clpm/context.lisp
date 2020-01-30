;;;; Context
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/context
    (:use #:cl
          #:alexandria
          #:clpm/requirement
          #:clpm/source)
  (:export #:context-name
           #:context-releases
           #:context-requirements
           #:context-reverse-dependencies
           #:context-sources
           #:context-system-releases
           #:copy-context
           #:load-context-from-fs
           #:serialize-context-to-stream))

(in-package #:clpm/context)

(defclass context ()
  ((name
    :initform nil
    :initarg :name
    :reader context-name)
   (requirements
    :initform nil
    :initarg :requirements
    :accessor context-requirements)
   (sources
    :initform nil
    :initarg :sources
    :accessor context-sources)
   (releases
    :initform nil
    :initarg :releases
    :accessor context-releases)
   (reverse-dependencies
    :initform nil
    :initarg :reverse-dependencies
    :accessor context-reverse-dependencies)
   (system-releases
    :initform nil
    :initarg :system-releases
    :accessor context-system-releases)))

(defun copy-context (context)
  (make-instance 'context
                 :name (context-name context)
                 :releases (copy-list (context-releases context))
                 :requirements (copy-list (context-requirements context))
                 :reverse-dependencies (copy-alist (context-reverse-dependencies context))
                 :sources (copy-list (context-sources context))
                 :system-releases (copy-list (context-system-releases context))))

(defun load-context-from-fs (name)
  (make-instance 'context
                 :name name
                 :sources (load-sources)))


;; * Serializing

;; ** Context

(defun write-section-header (section-name stream)
  (dotimes (i 80)
    (write-char #\; stream))
  (terpri stream)
  (write-string ";; BEGIN " stream)
  (write-string (string-upcase section-name) stream)
  (terpri stream)
  (dotimes (i 80)
    (write-char #\; stream))
  (terpri stream)
  (terpri stream))

(defun serialize-context-to-stream (context stream)
  (format t "~S~%~%~%" (context-system-releases context))
  (uiop:with-safe-io-syntax ()
    (let ((*print-case* :downcase)
          (*print-length* nil)
          (*print-level* nil)
          (*print-pretty* t)
          (*print-right-margin* nil))
      (pprint-logical-block (stream nil)
        (prin1 '(:api-version "0.3") stream)
        (pprint-newline :mandatory stream)
        (pprint-newline :mandatory stream)

        ;; Requirements
        (write-section-header "requirements" stream)
        (prin1 :requirements stream)
        (pprint-newline :mandatory stream)
        (pprint-logical-block (stream (context-requirements context))
          (pprint-exit-if-list-exhausted)
          (loop
            (serialize-context-requirement (pprint-pop) stream)
            (pprint-exit-if-list-exhausted)
            (pprint-newline :mandatory stream)))
        (pprint-newline :mandatory stream)
        (pprint-newline :mandatory stream)

        ;; Releases
        (write-section-header "releases" stream)
        (prin1 :releases stream)
        (pprint-newline :mandatory stream)
        (pprint-logical-block (stream (context-releases context))
          (pprint-exit-if-list-exhausted)
          (loop
            (let ((release (pprint-pop)))
              (serialize-context-release release
                                         (remove-if-not (lambda (x)
                                                          (eql x release))
                                                        (context-system-releases context)
                                                        :key #'system-release-release)
                                         stream)
              (pprint-exit-if-list-exhausted)
              (pprint-newline :mandatory stream))))
        (pprint-newline :mandatory stream)
        (pprint-newline :mandatory stream)

        ;; Reverse Dependencies
        (write-section-header "reverse-dependencies" stream)
        (prin1 :reverse-dependencies stream)
        (pprint-newline :mandatory stream)
        (pprint-logical-block (stream (context-reverse-dependencies context))
          (pprint-exit-if-list-exhausted)
          (loop
            (let ((release-and-reverse-deps (pprint-pop)))
              (serialize-context-reverse-dependencies
               release-and-reverse-deps
               stream))
            (pprint-exit-if-list-exhausted)
            (pprint-newline :mandatory stream)
            (pprint-newline :mandatory stream)))
        (pprint-newline :mandatory stream)))))

;; ** Releases

(defmethod extract-why ((why clpm-system-release))
  (let* ((system (system-release-system why)))
    (list :system (system-name system))))

(defmethod extract-why ((why (eql t)))
  t)

(defun serialize-reverse-dep (req stream)
  (let ((why (requirement/why req)))
    (pprint-logical-block (stream nil :prefix "(" :suffix ")")
      (prin1 (extract-why why) stream)
      (pprint-newline :mandatory stream)
      (serialize-context-requirement req stream))))

(defun serialize-context-release (release system-releases stream)
  (let ((project (release-project release)))
    (pprint-logical-block (stream nil :prefix "(" :suffix ")")
      ;; Name
      (prin1 (project-name project) stream)
      ;; Version
      (pprint-newline :mandatory stream)
      (prin1 :version stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (release-version release) stream)
      ;; Source
      (pprint-newline :mandatory stream)
      (prin1 :source stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (source-name (release-source release)) stream)

      ;; Systems
      (pprint-newline :mandatory stream)
      (prin1 :systems stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (pprint-logical-block (stream system-releases :prefix "(" :suffix ")")
        (pprint-exit-if-list-exhausted)
        (loop
          (prin1 (system-name (system-release-system (pprint-pop))) stream)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space stream)
          (pprint-newline :fill stream))))))

(defun serialize-context-reverse-dependencies (release-and-reverse-deps stream)
  (destructuring-bind (release . reverse-deps) release-and-reverse-deps
    (let ((project (release-project release)))
      (pprint-logical-block (stream nil :prefix "(" :suffix ")")
        ;; Name
        (prin1 (project-name project) stream)
        ;; Reverse deps
        (pprint-newline :mandatory stream)
        (pprint-logical-block (stream reverse-deps)
          (pprint-exit-if-list-exhausted)
          (loop
            (serialize-reverse-dep (pprint-pop) stream)
            (pprint-exit-if-list-exhausted)
            (pprint-newline :mandatory stream)))))))

;; ** Requirements

(defgeneric serialize-context-requirement (req stream))

(defgeneric requirement-type-keyword (req))

(defmethod requirement-type-keyword ((req system-requirement))
  :system)

(defmethod serialize-context-requirement ((req versioned-requirement) stream)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (prin1 (requirement-type-keyword req) stream)
    ;; Name
    (write-char #\Space stream)
    (pprint-newline :fill stream)
    (prin1 :name stream)
    (write-char #\Space stream)
    (pprint-newline :miser stream)
    (prin1 (requirement/name req) stream)
    ;; Version
    (when (and (requirement/version-spec req)
               (not (equal (requirement/version-spec req) '((>= . "0")))))
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :version-spec stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (requirement/version-spec req) stream))))

(defmethod print-object ((context context) stream)
  (pprint-logical-block (stream nil)
    (print-unreadable-object (context stream :type t :identity t)
      (pprint-newline :mandatory stream)
      ;; Sources
      (prin1 :sources stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (write (context-sources context) :stream stream)
      ;; Requirements
      (write-char #\Space stream)
      (pprint-newline :mandatory stream)
      (prin1 :requirements stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (write (context-requirements context) :stream stream)
      ;; Releases
      (write-char #\Space stream)
      (pprint-newline :mandatory stream)
      (prin1 :releases stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (write (context-releases context) :stream stream)
      ;; Put ID on a new line
      (pprint-newline :mandatory stream))))
