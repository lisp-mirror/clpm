;;;; Context
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/context
    (:use #:cl
          #:alexandria
          #:clpm/data
          #:clpm/requirement
          #:clpm/source)
  (:export #:context-diff
           #:context-name
           #:context-releases
           #:context-requirements
           #:context-reverse-dependencies
           #:context-sources
           #:context-system-releases
           #:context-to-asdf-source-registry
           #:copy-context
           #:load-global-context
           #:save-global-context
           #:serialize-context-to-stream))

(in-package #:clpm/context)

(defclass context ()
  ((name
    :initform nil
    :initarg :name
    :accessor context-name)
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

(defun global-context-pathname (name)
  (clpm-data-pathname (list "contexts" name)))


;; * ASDF Integration

(defun context-to-asdf-source-registry (context)
  (let* ((system-releases (flatten (mapcar #'release-system-releases (context-releases context))))
         (system-files (mapcar #'system-release-system-file system-releases))
         (system-file-pathnames (mapcar #'system-file-absolute-asd-pathname system-files))
         (system-file-directories (remove-duplicates (mapcar #'uiop:pathname-directory-pathname system-file-pathnames)
                                                     :test #'uiop:pathname-equal)))
    `(:source-registry
      :ignore-inherited-configuration
      ,@(mapcar (lambda (x) (list :directory x)) system-file-directories))))


;; * Diffing

(defun context-diff (old-context new-context)
  (let* ((old-releases (context-releases old-context))
         (new-releases (context-releases new-context))
         (added-releases (set-difference new-releases old-releases))
         (removed-releases (set-difference old-releases new-releases))
         (changed-releases nil))
    ;; Compute up/downgrades.
    (dolist (r removed-releases)
      (when-let ((new-r (member (project-name r) added-releases :key #'project-name :test #'equal)))
        (removef removed-releases r)
        (removef added-releases new-r)
        (push (cons r new-r) changed-releases)))
    (values added-releases removed-releases changed-releases)))


;; * Deserializing

(defun load-global-context (name &optional (error t))
  (let ((pn (global-context-pathname name)))
    (with-open-file (s pn
                       :if-does-not-exist (if error :error nil))
      (if s
          (let ((out (load-context-from-stream s)))
            (setf (context-name out) name)
            out)
          (make-instance 'context
                         :name name
                         :sources (load-sources))))))

(defgeneric check-section-valid (prev-section current-section)
  (:method (prev-section current-section)
    (error "Invalid section transition ~S -> ~S" prev-section current-section))
  (:method ((prev-section (eql nil)) (current-section (eql :requirements)))
    t)
  (:method ((prev-section (eql :requirements)) (current-section (eql :releases)))
    t)
  (:method ((prev-section (eql :releases)) (current-section (eql :reverse-dependencies)))
    t))

(defgeneric process-form (context section form))

(defmethod process-form (context (section (eql :requirements)) form)
  (destructuring-bind (type &key name version-spec) form
    (assert (eql type :system))
    (assert (null version-spec))
    (push (make-instance 'system-requirement
                         :name name
                         :why t)
          (context-requirements context))))

(defmethod process-form (context (section (eql :releases)) form))

(defmethod process-form (context (section (eql :reverse-dependencies)) form))

(defun load-context-from-stream (stream)
  (uiop:with-safe-io-syntax ()
    ;; The first form in the stream must be an API declaration.
    (let ((f (read stream nil)))
      (unless (equal f '(:api-version "0.3"))
        (error "Unknown context API version")))
    (let ((out (make-instance 'context :sources (load-sources))))
      ;; The next forms are either tags or lists. The tags denote sections.
      (loop
        :with section := nil
        :for form := (read stream nil :eof)
        :until (eql form :eof)
        :when (symbolp form)
          :do (check-section-valid section form)
              (setf section form)
        :else
          :do (process-form out section form))
      out)))


;; * Serializing

(defun save-global-context (context)
  (assert (context-name context))
  (assert (context-reverse-dependencies context))
  (let ((pn (global-context-pathname (context-name context))))
    (ensure-directories-exist pn)
    (with-open-file (s pn
                       :direction :output
                       :if-exists :supersede)
      (serialize-context-to-stream context s))))

;; ** Context

(defun write-header (text stream)
  (assert (not (position #\Newline text)))
  (dotimes (i 80)
    (write-char #\; stream))
  (terpri stream)
  (write-string ";; " stream)
  (write-string text stream)
  (terpri stream)
  (dotimes (i 80)
    (write-char #\; stream))
  (terpri stream)
  (terpri stream))

(defun write-section-header (section-name stream)
  (write-header (concatenate 'string "BEGIN " (string-upcase section-name)) stream))

(defun serialize-context-to-stream (context stream)
  (uiop:with-safe-io-syntax ()
    (let ((*print-case* :downcase)
          (*print-length* nil)
          (*print-level* nil)
          (*print-pretty* t)
          (*print-right-margin* nil))
      (write-header "This is autogenerated by CLPM. Do not edit by hand." stream)
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