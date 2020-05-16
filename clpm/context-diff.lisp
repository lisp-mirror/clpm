;;;; Context Diffs
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/context-diff
    (:use #:cl
          #:alexandria
          #:anaphora
          #:cl-ansi-text
          #:clpm/context
          #:clpm/source)
  (:export #:context-diff-empty-p
           #:context-diff-to-plist
           #:make-context-diff
           #:print-context-diff-to-stream))

(in-package #:clpm/context-diff)

(defclass context-diff ()
  ((release-diffs
    :initarg :release-diffs
    :initform nil
    :reader context-diff-release-diffs))
  (:documentation
   "A representation of changes that will be made to a context."))

(defclass release-diff ()
  ((project-name
    :initarg :project-name
    :reader release-diff-project-name)
   (old-version
    :initform nil
    :initarg :old-version
    :reader release-diff-old-version)
   (new-version
    :initform nil
    :initarg :new-version
    :reader release-diff-new-version)
   (old-source
    :initform nil
    :initarg :old-source
    :reader release-diff-old-source)
   (new-source
    :initform nil
    :initarg :new-source
    :reader release-diff-new-source))
  (:documentation
   "A representation of a change to a single project."))

(defun make-context-diff (old-context new-context)
  (let* ((old-releases (context-releases old-context))
         (new-releases (context-releases new-context))
         (all-projects (union (mapcar (compose #'project-name #'release-project) old-releases)
                              (mapcar (compose #'project-name #'release-project) new-releases)
                              :test 'equal)))
    (make-instance 'context-diff
                   :release-diffs
                   (loop
                     :for project-name :in all-projects
                     :for old-release := (find project-name old-releases
                                               :key (compose #'project-name #'release-project)
                                               :test 'equal)
                     :for new-release := (find project-name new-releases
                                               :key (compose #'project-name #'release-project)
                                               :test 'equal)
                     :if (not (eql old-release new-release))
                       :collect (apply #'make-instance
                                       'release-diff
                                       :project-name project-name
                                       (append
                                        (when old-release
                                          (list :old-source (source-name (release-source old-release))
                                                :old-version (release-version old-release)))
                                        (when new-release
                                          (list :new-source (source-name (release-source new-release))
                                                :new-version (release-version new-release)))))))))


;; Serializing

(defun context-diff-to-plist (diff)
  (list :releases (mapcar #'release-diff-to-plist (context-diff-release-diffs diff))))

(defun release-diff-to-plist (diff)
  `(:project-name ,(release-diff-project-name diff)
                  ,@(awhen (release-diff-old-version diff)
                      (list :old-version it))
                  ,@(awhen (release-diff-new-version diff)
                      (list :new-version it))
                  ,@(awhen (release-diff-old-source diff)
                      (list :old-source it))
                  ,@(awhen (release-diff-new-source diff)
                      (list :new-source it))))

(defun context-diff-empty-p (diff)
  "Returns non-NIL if DIFF contains no changes."
  (null (context-diff-release-diffs diff)))

(defun context-diff-column-widths (diff)
  "Returns a list of 5 integers detailing the column widths needed to print a
diff. The first column is the project name, then old version, new version, old
source, and finally new source."
  (flet ((printed-length (x)
           (if (stringp x)
               (length x)
               (length (format nil "~A" x)))))
    (list (max (length "Project")
               (reduce #'max (context-diff-release-diffs diff)
                       :key (lambda (release-diff)
                              (printed-length (release-diff-project-name release-diff)))
                       :initial-value 0))
          (max (length "Old Version")
               (reduce #'max (context-diff-release-diffs diff)
                       :key (lambda (release-diff)
                              (printed-length (release-diff-old-version release-diff)))
                       :initial-value 0))
          (max (length "New Version")
               (reduce #'max (context-diff-release-diffs diff)
                       :key (lambda (release-diff)
                              (printed-length (release-diff-new-version release-diff)))
                       :initial-value 0))
          (max (length "Old Source")
               (reduce #'max (context-diff-release-diffs diff)
                       :key (lambda (release-diff)
                              (printed-length (release-diff-old-source release-diff)))
                       :initial-value 0))
          (max (length "New Source")
               (reduce #'max (context-diff-release-diffs diff)
                       :key (lambda (release-diff)
                              (printed-length (release-diff-new-source release-diff)))
                       :initial-value 0)))))

(defun print-context-diff-to-stream (diff stream &key use-color-p)
  "Print a human readable description of DIFF to STREAM."
  (flet ((num (x)
           (format nil "~D" x))
         (color (x)
           (when use-color-p
             (if (eql x :reset)
                 +reset-color-string+
                 (make-color-string x)))))
    (let* ((column-lengths (context-diff-column-widths diff))
           (header-format-string
             (uiop:strcat
              ;; Project name
              "~" (num (first column-lengths)) "<~A~>  "
              ;; Old Version
              "~" (num (second column-lengths)) "<~@[~A~]~>"
              ;; Arrow
              "  ~:*~:[  ~;~:[  ~;->~]~:*~]  "
              ;; New Version
              "~" (num (third column-lengths)) "<~@[~A~]~>  "
              ;; Old Source
              "~" (num (fourth column-lengths)) "<~@[~A~]~>"
              ;; Arrow
              "  ~:*~:[  ~;~:[  ~;->~]~:*~]  "
              ;; New Version
              "~" (num (fifth column-lengths)) "<~@[~A~]~>~%"))
           (row-format-string
             (uiop:strcat
              ;; Project name
              (color :green) "~" (num (first column-lengths)) "<~A~>  "
              ;; Old Version
              (color :red) "~" (num (second column-lengths)) "<~@[~A~]~>"
              ;; Arrow
              (color :reset) "  ~:*~:[  ~;~:[  ~;->~]~:*~]  "
              ;; New Version
              (color :blue) "~" (num (third column-lengths)) "<~@[~A~]~>  "
              ;; Old Source
              (color :red) "~" (num (fourth column-lengths)) "<~@[~A~]~>"
              ;; Arrow
              (color :reset) "  ~:*~:[  ~;~:[  ~;->~]~:*~]  "
              ;; New Version
              (color :blue) "~" (num (fifth column-lengths)) "<~@[~A~]~>~%"
              ;; Reset color
              (color :reset))))
      (format stream header-format-string
              "Project"
              "Old Version" "New Version"
              "Old Source" "New Source")
      (dolist (release-diff (sort (copy-list (context-diff-release-diffs diff))
                                  #'string< :key #'release-diff-project-name))
        (format stream row-format-string
                (release-diff-project-name release-diff)
                (release-diff-old-version release-diff) (release-diff-new-version release-diff)
                (release-diff-old-source release-diff) (release-diff-new-source release-diff))))))
