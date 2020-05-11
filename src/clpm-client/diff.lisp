;;;; Functions dealing with context diffs from CLPM
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)

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

(defun context-diff-empty-p (diff)
  "Returns non-NIL if DIFF contains no changes."
  (null (context-diff-release-diffs diff)))

(defun make-added-release-diff-from-description (desc)
  (destructuring-bind (project-name &key version source)
      desc
    (make-instance 'release-diff
                   :project-name project-name
                   :new-version version
                   :new-source source)))

(defun make-removed-release-diff-from-description (desc)
  (destructuring-bind (project-name &key version source)
      desc
    (make-instance 'release-diff
                   :project-name project-name
                   :old-version version
                   :old-source source)))

(defun make-changed-release-diff-from-description (desc)
  (destructuring-bind (project-name &key old new)
      desc
    (destructuring-bind (&key ((:version old-version)) ((:source old-source)))
        old
      (destructuring-bind (&key ((:version new-version)) ((:source new-source)))
          new
        (make-instance 'release-diff
                       :project-name project-name
                       :old-version old-version
                       :old-source old-source
                       :new-version new-version
                       :new-source new-source)))))

(defun make-context-diff-from-description (desc)
  (destructuring-bind (&key added-releases removed-releases changed-releases)
      desc
    (make-instance 'context-diff
                   :release-diffs
                   (sort (append
                          (mapcar #'make-added-release-diff-from-description added-releases)
                          (mapcar #'make-removed-release-diff-from-description removed-releases)
                          (mapcar #'make-changed-release-diff-from-description changed-releases))
                         #'string<
                         :key #'release-diff-project-name))))

(defun context-diff-column-widths (diff)
  "Returns a list of 5 integers detailing the column widths needed to print a
diff. The first column is the project name, then old version, new version, old
source, and finally new source."
  (list (max (length "Project")
             (reduce #'max (context-diff-release-diffs diff)
                     :key (lambda (release-diff)
                            (length (release-diff-project-name release-diff)))
                     :initial-value 0))
        (max (length "Old Version")
             (reduce #'max (context-diff-release-diffs diff)
                     :key (lambda (release-diff)
                            (length (release-diff-old-version release-diff)))
                     :initial-value 0))
        (max (length "New Version")
             (reduce #'max (context-diff-release-diffs diff)
                     :key (lambda (release-diff)
                            (length (release-diff-new-version release-diff)))
                     :initial-value 0))
        (max (length "Old Source")
             (reduce #'max (context-diff-release-diffs diff)
                     :key (lambda (release-diff)
                            (length (release-diff-old-source release-diff)))
                     :initial-value 0))
        (max (length "New Source")
             (reduce #'max (context-diff-release-diffs diff)
                     :key (lambda (release-diff)
                            (length (release-diff-new-source release-diff)))
                     :initial-value 0))))

(defun print-context-diff-to-stream (diff stream)
  "Print a human readable description of DIFF to STREAM."
  (let* ((column-lengths (context-diff-column-widths diff))
         (header-format-string
           (apply #'format nil "~~~D<~~A~~>   ~
~~~D<~~@[~~A~~]~~>      ~~~D<~~@[~~A~~]~~>   ~
~~~D<~~@[~~A~~]~~>      ~~~D<~~@[~~A~~]~~>~~%"
                  column-lengths))
         (row-format-string
           (apply #'format
                  nil "~~~D<~~A~~>   ~
~~~D<~~@[~~A~~]~~>  ~~:*~~:[  ~~;~~:[  ~~;->~~]~~:*~~]  ~~~D<~~@[~~A~~]~~>   ~
~~~D<~~@[~~A~~]~~>  ~~:*~~:[  ~~;~~:[  ~~;->~~]~~:*~~]  ~~~D<~~@[~~A~~]~~>~~%"
                  column-lengths)))
    (format stream header-format-string
            "Project"
            "Old Version" "New Version"
            "Old Source" "New Source")
    (dolist (release-diff (context-diff-release-diffs diff))
      (format stream row-format-string
              (release-diff-project-name release-diff)
              (release-diff-old-version release-diff) (release-diff-new-version release-diff)
              (release-diff-old-source release-diff) (release-diff-new-source release-diff)))))

(defun make-diff-validator-fun ()
  `(lambda (diff)
     (print (context-diff-to-plist diff))
     (terpri)
     (finish-output)
     (read)))
