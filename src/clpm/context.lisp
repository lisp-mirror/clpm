;;;; Context
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/context
    (:use #:cl
          #:alexandria
          #:cl-ansi-text
          #:clpm/config
          #:clpm/data
          #:clpm/log
          #:clpm/requirement
          #:clpm/source)
  (:export #:context-add-requirement!
           #:context-diff
           #:context-diff-has-diff-p
           #:context-name
           #:context-releases
           #:context-requirements
           #:context-reverse-dependencies
           #:context-sources
           #:context-system-releases
           #:context-to-asdf-source-registry-form
           #:context-to-asdf-source-registry.d-forms
           #:context-write-asdf-files
           #:copy-context
           #:get-context
           #:load-global-context
           #:print-context-diff
           #:save-global-context
           #:serialize-context-to-stream))

(in-package #:clpm/context)

(setup-logger)

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

(defun get-context (context-designator)
  (cond
    ((typep context-designator 'context)
     context-designator)
    ((stringp context-designator)
     (load-global-context context-designator nil))
    ((null context-designator)
     (load-global-context "default" nil))
    (t
     (error "Unable to translate ~S to a context object" context-designator))))


;; * Adding requirements

(defun context-find-requirement (context type name)
  (find-if (lambda (req)
             (and (typep req type)
                  (equal name (requirement/name req))))
           (context-requirements context)))

(defun context-edit-req! (old-req new-req)
  (let ((changed-p nil))
    (unless (eql (requirement/source old-req) (requirement/source new-req))
      (setf changed-p t
            (requirement/source old-req) (requirement/source new-req)))
    (unless (subsetp (requirement/version-spec new-req) (requirement/version-spec old-req)
                     :test #'equal)
      (setf changed-p t
            (requirement/version-spec old-req) (union (requirement/version-spec old-req)
                                                      (requirement/version-spec new-req)
                                                      :test #'equal)))

    changed-p))

(defun context-add-requirement! (context req)
  (let ((existing-req (context-find-requirement context (type-of req) (requirement/name req))))
    (if existing-req
        (context-edit-req! existing-req req)
        (push req (context-requirements context)))))


;; * ASDF Integration

(defun context-to-asdf-source-registry.d-forms (context)
  (let* ((system-releases (flatten (mapcar #'release-system-releases (context-releases context))))
         (system-files (mapcar #'system-release-system-file system-releases))
         (system-file-pathnames (mapcar #'system-file-absolute-asd-pathname system-files))
         (system-file-directories (remove-duplicates (mapcar #'uiop:pathname-directory-pathname system-file-pathnames)
                                                     :test #'uiop:pathname-equal)))
    (mapcar (lambda (x) (list :directory x)) system-file-directories)))

(defun context-to-asdf-source-registry-form (context)
  `(:source-registry
    :ignore-inherited-configuration
    ,@(context-to-asdf-source-registry.d-forms context)))

(defun context-write-asdf-files (context)
  (assert (context-name context))
  (let* ((name (context-name context))
         (source-registry-files (config-value :contexts name :source-registry-files))
         (source-registry.d-files (config-value :contexts name :source-registry.d-files))
         (registry-form (context-to-asdf-source-registry-form context))
         (registry.d-forms (context-to-asdf-source-registry.d-forms context)))
    (uiop:with-safe-io-syntax ()
      (let ((*print-case* :downcase))
        (dolist (f source-registry-files)
          (log:debug "Writing context source-registry file to ~A" f)
          (ensure-directories-exist f)
          (with-open-file (s f :direction :output :if-exists :supersede)
            (write-string ";; -*- mode: common-lisp; -*-" s)
            (terpri s)
            (write-string ";; Autogenerated by clpm. Do not edit directly." s)
            (terpri s)
            (prin1 registry-form s)
            (terpri s)))
        (dolist (f source-registry.d-files)
          (log:debug "Writing context source-registry.d file to ~A" f)
          (ensure-directories-exist f)
          (with-open-file (s f :direction :output :if-exists :supersede)
            (write-string ";; -*- mode: common-lisp; -*-" s)
            (terpri s)
            (write-string ";; Autogenerated by clpm. Do not edit directly." s)
            (terpri s)
            (dolist (form registry.d-forms)
              (prin1 form s)
              (terpri s))))))))


;; * Diffing

(defclass context-diff ()
  ((added-releases
    :initarg :added-releases
    :initform nil
    :accessor context-diff-added-releases)
   (removed-releases
    :initarg :removed-releases
    :initform nil
    :accessor context-diff-removed-releases)))

(defun context-diff (old-context new-context)
  (let* ((old-releases (context-releases old-context))
         (new-releases (context-releases new-context))
         (added-releases (set-difference new-releases old-releases))
         (removed-releases (set-difference old-releases new-releases)))
    (make-instance 'context-diff
                   :added-releases added-releases
                   :removed-releases removed-releases)))

(defun context-diff-has-diff-p (diff)
  (or (context-diff-added-releases diff)
      (context-diff-removed-releases diff)))

(defun print-diff-release (r stream)
  (format stream "~A~50,1,4<~A~;~A~^~;~A~;~A~^~>"
          (make-color-string :green)
          (project-name (release-project r))
          (make-color-string :blue)
          (release-version r)
          +reset-color-string+))

(defun print-diff-release-change (pair stream)
  (destructuring-bind (to . from) pair
    (format stream "~A~65,1,4<~A~;~A~^~;~A -> ~A~;~A~^~>"
            (make-color-string :green)
            (project-name (release-project from))
            (make-color-string :blue)
            (release-version from)
            (release-version to)
            +reset-color-string+)))

(defun print-context-diff (diff stream)
  ;; Compute the maximum project length.
  (let ((removed-releases (context-diff-removed-releases diff))
        (added-releases (context-diff-added-releases diff))
        (changed-releases nil))
    (dolist (r added-releases)
      (when-let ((matching-release (find (release-project r) removed-releases
                                         :key #'release-project)))
        (removef removed-releases matching-release)
        (removef added-releases r)
        (push (cons r matching-release) changed-releases)))

    (when changed-releases
      (format stream "~AChanged releases:~A~%"
              (make-color-string :magenta :effect :bright)
              +reset-color-string+)
      (dolist (pair changed-releases)
        (print-diff-release-change pair stream)
        (terpri stream)))
    (when added-releases
      (format stream "~AAdded releases:~A~%"
              (make-color-string :magenta :effect :bright)
              +reset-color-string+)
      (dolist (r added-releases)
        (print-diff-release r stream)
        (terpri stream)))
    (when removed-releases
      (format stream "~ARemoved releases:~A~%"
              (make-color-string :magenta :effect :bright)
              +reset-color-string+)
      (dolist (r removed-releases)
        (print-diff-release r stream)
        (terpri stream)))
    (finish-output stream)))


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
                         :sources (sources))))))

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
  (destructuring-bind (type &key name version source) form
    (push (make-instance (ecase type
                           (:system 'system-requirement)
                           (:project 'project-requirement))
                         :name name
                         :source (get-source source)
                         :version-spec version
                         :why t)
          (context-requirements context))))

(defmethod process-form (context (section (eql :releases)) form)
  (destructuring-bind (name &key version source systems) form
    (declare (ignore systems))
    (let ((source (get-source source)))
      (push (source-project-release source name version)
            (context-releases context)))))

(defmethod process-form (context (section (eql :reverse-dependencies)) form))

(defun load-context-from-stream (stream)
  (uiop:with-safe-io-syntax ()
    ;; The first form in the stream must be an API declaration.
    (let ((f (read stream nil)))
      (unless (equal f '(:api-version "0.3"))
        (error "Unknown context API version")))
    (let ((out (make-instance 'context :sources (sources))))
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
            (let* ((req (pprint-pop)))
              ;; This is a requirement where we didn't know the real reason for
              ;; its existence at the time (because we were trying to load an
              ;; ASD file into the groveler). We can ignore it because we'll
              ;; have the true reason elsewhere in the list.
              (unless (or (eql t req)
                          (eql (requirement/why req) :grovel))
                (serialize-reverse-dep req stream))
              (pprint-exit-if-list-exhausted)
              (unless (or (eql t req)
                          (eql (requirement/why req) :grovel))
                (pprint-newline :mandatory stream)))))))))

;; ** Requirements

(defgeneric serialize-context-requirement (req stream))

(defgeneric requirement-type-keyword (req))

(defmethod requirement-type-keyword ((req project-requirement))
  :project)

(defmethod requirement-type-keyword ((req system-requirement))
  :system)

(defmethod requirement-type-keyword ((req vcs-project-requirement))
  :project)

(defmethod serialize-context-requirement ((req vcs-project-requirement) stream)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (prin1 (requirement-type-keyword req) stream)
    ;; Name
    (write-char #\Space stream)
    (pprint-newline :fill stream)
    (prin1 :name stream)
    (write-char #\Space stream)
    (pprint-newline :miser stream)
    (prin1 (requirement/name req) stream)
    ;; Branch
    (when (requirement/branch req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :branch stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (requirement/branch req) stream))
    ;; Commit
    (when (requirement/commit req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :commit stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (requirement/commit req) stream))
    ;; Tag
    (when (requirement/tag req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :tag stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (requirement/tag req) stream))
    ;; Source
    (when (requirement/source req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :source stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (source-name (requirement/source req)) stream))
    ;; no deps
    (when (requirement/no-deps-p req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :no-deps-p stream)
      (write-char #\Space stream)
      (prin1 t stream))))

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
    (when (requirement/version-spec req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :version stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (requirement/version-spec req) stream))
    ;; Source
    (when (requirement/source req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :source stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (source-name (requirement/source req)) stream))
    ;; no deps
    (when (requirement/no-deps-p req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :no-deps-p stream)
      (write-char #\Space stream)
      (prin1 t stream))))

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
