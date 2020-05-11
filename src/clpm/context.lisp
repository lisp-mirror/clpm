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
  (:export #:context
           #:context-add-requirement!
           #:context-asd-pathnames
           #:context-diff
           #:context-diff-has-diff-p
           #:context-diff-to-plist
           #:context-find-system-asd-pathname
           #:context-installed-only-p
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
           #:load-anonymous-context-from-pathname
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
   (installed-only-p
    :initarg :installed-only-p
    :initform nil
    :reader context-installed-only-p)
   (pathname
    :initarg :pathname
    :accessor context-pathname)
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
     (load-global-context (config-value :context) nil))
    (t
     (error "Unable to translate ~S to a context object" context-designator))))


;; * Adding requirements

(defun context-find-requirement (context type name)
  (find-if (lambda (req)
             (and (ecase type
                    (:project (or (typep req 'project-requirement)
                                  (typep req 'vcs-project-requirement)))
                    (:system (typep req 'system-requirement))
                    (:asd-system (typep req 'fs-system-requirement))
                    (:asd-file (typep req 'fs-system-file-requirement)))
                  (equal name (requirement-name req))))
           (context-requirements context)))

(defun context-add-requirement! (context req)
  "If the requirement is new, add it. Otherwise modify an existing requirement
in place with the same name. Return the new requirement if it was modified."
  (let ((existing-req (context-find-requirement context (requirement-type-keyword req)
                                                (requirement-name req))))
    (if existing-req
        (progn
          (log:debug "Replacing requirement ~A with ~A" existing-req req)
          (setf (context-requirements context)
                (substitute req existing-req (context-requirements context)))
          req)
        (progn
          (log:debug "Adding requirement ~A" req)
          (push req (context-requirements context))
          nil))))


;; * ASDF Integration

(defun context-asd-pathnames (context)
  (let* ((context (get-context context))
         (releases (context-releases context))
         (system-files (flatten (mapcar #'release-system-files releases))))
    (mapcar #'system-file-absolute-asd-pathname system-files)))

(defun context-to-asdf-source-registry.d-forms (context)
  (let* ((context (get-context context))
         (system-file-pathnames (context-asd-pathnames context))
         (system-file-directories (remove-duplicates (mapcar #'uiop:pathname-directory-pathname system-file-pathnames)
                                                     :test #'uiop:pathname-equal)))
    (mapcar (lambda (x) (list :directory x)) system-file-directories)))

(defun context-to-asdf-source-registry-form (context &optional extra-forms)
  `(:source-registry
    :ignore-inherited-configuration
    ,@(context-to-asdf-source-registry.d-forms context)
    ,@extra-forms))

(defun context-find-system-asd-pathname (context system-name)
  (when-let* ((context (get-context context))
              (system-release (find system-name (context-system-releases context)
                                    :key (compose #'system-name #'system-release-system)
                                    :test #'equal)))
    (system-release-absolute-asd-pathname system-release)))

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
                   :added-releases (remove 'fs-release added-releases :key #'type-of)
                   :removed-releases (remove 'fs-release removed-releases :key #'type-of))))

(defun context-diff-has-diff-p (diff)
  (or (context-diff-added-releases diff)
      (context-diff-removed-releases diff)))

(defun context-diff-to-plist (diff &key stringify-commits-p)
  (let ((removed-releases (context-diff-removed-releases diff))
        (added-releases (context-diff-added-releases diff))
        (changed-releases nil))
    (dolist (r added-releases)
      (when-let ((matching-release (find (release-project r) removed-releases
                                         :key #'release-project)))
        (removef removed-releases matching-release)
        (removef added-releases r)
        (push (cons matching-release r) changed-releases)))

    (labels ((release-to-plist (r)
               (let ((version (release-version r)))
                 (list :version (if (and (listp version) stringify-commits-p)
                                    (format nil "commit ~A" (second version))
                                    version)
                       :source (source-name (release-source r)))))
             (release-to-form (r)
               (list* (project-name (release-project r)) (release-to-plist r)))
             (changed-release-to-form (cell)
               (destructuring-bind (old . new) cell
                 (list (project-name (release-project old))
                       :old (release-to-plist old) :new (release-to-plist new)))))
      (list
       :added-releases (sort (mapcar #'release-to-form added-releases) 'string< :key #'car)
       :removed-releases (sort (mapcar #'release-to-form removed-releases) 'string< :key #'car)
       :changed-releases (sort (mapcar #'changed-release-to-form changed-releases) 'string< :key #'car)))))

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

(defun make-diff-format-string (spacing name-length
                                old-version-length new-version-length
                                old-source-length new-source-length
                                &key use-color-p)
  (let ((counter 0)
        (spacing (+ spacing (if use-color-p (length (make-color-string :green)) 0))))
    (flet ((num (x)
             (format nil "~D" x))
           (color (x)
             (when use-color-p (make-color-string x))))
      (uiop:strcat
       ;; Project name column
       (color :green) "~@[~A~]~" (num (incf counter (+ name-length spacing))) "T"
       ;; Old version column
       (color :red) "~@[~A~]~" (num (incf counter (+ old-version-length spacing))) "T"
       ;; New version column
       (color :blue) "~@[~A~]~" (num (incf counter (+ new-version-length spacing))) "T"
       ;; Old source column
       (color :red) "~@[~A~]~" (num (incf counter (+ old-source-length spacing))) "T"
       ;; New source column
       (color :blue) "~@[~A~]~" (num (incf counter (+ new-source-length spacing))) "T"
       ;; Reset color
       (when use-color-p +reset-color-string+)))))

(defun print-context-diff (diff stream)
  ;; Compute the maximum project length.
  (flet ((max-length (list &key (key 'identity))
           (reduce #'max list :key (compose (lambda (x)
                                              (if (stringp x)
                                                  (length x)
                                                  (length (format nil "~A" x))))
                                            key)
                              :initial-value 0)))
    (let* ((plist (context-diff-to-plist diff :stringify-commits-p t))
           (max-name-length (max (length "Project")
                                 (max-length (getf plist :added-releases) :key #'car)
                                 (max-length (getf plist :removed-releases) :key #'car)
                                 (max-length (getf plist :changed-releases) :key #'car)))
           (max-old-version-length (max (length "Old Version")
                                        (max-length (getf plist :removed-releases)
                                                    :key (lambda (x)
                                                           (getf (rest x) :version)))
                                        (max-length (getf plist :changed-releases)
                                                    :key (lambda (x)
                                                           (getf (getf (rest x) :old) :version)))))
           (max-new-version-length (max (length "New Version")
                                        (max-length (getf plist :added-releases)
                                                    :key (lambda (x)
                                                           (getf (rest x) :version)))
                                        (max-length (getf plist :changed-releases)
                                                    :key (lambda (x)
                                                           (getf (getf (rest x) :new) :version)))))
           (max-old-source-length (max (length "Old Source")
                                       (max-length (getf plist :removed-releases)
                                                   :key (lambda (x)
                                                          (getf (rest x) :source)))
                                       (max-length (getf plist :changed-releases)
                                                   :key (lambda (x)
                                                          (getf (getf (rest x) :old) :source)))))
           (max-new-source-length (max (length "New Source")
                                       (max-length (getf plist :added-releases)
                                                   :key (lambda (x)
                                                          (getf (rest x) :source)))
                                       (max-length (getf plist :changed-releases)
                                                   :key (lambda (x)
                                                          (getf (getf (rest x) :new) :source)))))
           (format-string (make-diff-format-string 4 max-name-length
                                                   max-old-version-length max-new-version-length
                                                   max-old-source-length max-new-source-length
                                                   :use-color-p (and (not (featurep :windows))
                                                                     (interactive-stream-p stream))))
           (no-color-format-string (make-diff-format-string 4 max-name-length
                                                            max-old-version-length max-new-version-length
                                                            max-old-source-length max-new-source-length)))
      ;; Print all the added releases:
      (format stream no-color-format-string "Project" "Old Version" "New Version" "Old Source" "New Source")
      (terpri stream)
      (dolist (diff (getf plist :added-releases))
        (format stream format-string (car diff) nil (getf (cdr diff) :version) nil (getf (cdr diff) :source))
        (terpri stream))
      (dolist (diff (getf plist :changed-releases))
        (let ((old (getf (cdr diff) :old))
              (new (getf (cdr diff) :new)))
          (format stream format-string (car diff) (getf old :version) (getf new :version)
                  (getf old :source) (getf new :source)))
        (terpri stream))
      (dolist (diff (getf plist :removed-releases))
        (format stream format-string (car diff) (getf (cdr diff) :version) nil (getf (cdr diff) :source) nil)
        (terpri stream)))))


;; * Deserializing

(defun load-anonymous-context-from-pathname (pn &key installed-only-p)
  (with-open-file (s pn)
    (load-context-from-stream s :pathname pn :installed-only-p installed-only-p)))

(defun context-downselect-sources (name sources)
  (let ((allowed-source-names (config-value :contexts name :sources)))
    (if (eql t allowed-source-names)
        sources
        (remove-if-not (lambda (x) (member (source-name x) allowed-source-names :test 'equal))
                       sources))))

(defun load-global-context (name &optional (error t))
  ;; Currently, global contexts are tricky because they write their sources to
  ;; file, but we want to use the sources defined in the user's config. For now,
  ;; just pass an override into load-context-from-stream.
  (let ((pn (global-context-pathname name)))
    (with-open-file (s pn
                       :if-does-not-exist (if error :error nil))
      (if s
          (let ((out (load-context-from-stream s :sources (context-downselect-sources name (sources)))))
            (setf (context-name out) name)
            out)
          (make-instance 'context
                         :name name
                         :sources (context-downselect-sources name (sources)))))))

(defgeneric check-section-valid (prev-section current-section)
  (:method (prev-section current-section)
    (error "Invalid section transition ~S -> ~S" prev-section current-section))
  (:method ((prev-section (eql nil)) (current-section (eql :sources)))
    t)
  (:method ((prev-section (eql :sources)) (current-section (eql :requirements)))
    t)
  (:method ((prev-section (eql :requirements)) (current-section (eql :releases)))
    t)
  (:method ((prev-section (eql :releases)) (current-section (eql :reverse-dependencies)))
    t))

(defgeneric process-form (context section form))

(defmethod process-form (context (section (eql :requirements)) form)
  (destructuring-bind (type &key name version source branch tag commit ref pathname no-deps-p) form
    (push (cond
            ((eql type :asd-system)
             (make-instance 'fs-system-requirement
                            :name name
                            :pathname pathname
                            :no-deps-p no-deps-p
                            :why t))
            ((eql type :asd-file)
             (make-instance 'fs-system-file-requirement
                            :name pathname
                            :no-deps-p no-deps-p
                            :why t))
            ((or branch tag commit ref)
             (make-instance 'vcs-project-requirement
                            :name name
                            :source (get-source source)
                            :commit commit
                            :branch branch
                            :tag tag
                            :ref ref
                            :no-deps-p no-deps-p
                            :why t))
            (t
             (make-instance (ecase type
                              (:system 'system-requirement)
                              (:project 'project-requirement))
                            :name name
                            :source (get-source source)
                            :version-spec version
                            :no-deps-p no-deps-p
                            :why t)))
          (context-requirements context))))

(defmethod process-form (context (section (eql :releases)) form)
  (destructuring-bind (name &key version source systems) form
    (let* ((source (get-source source))
           (release (source-project-release source name version)))
      (push release
            (context-releases context))
      (dolist (system-name systems)
        (push (release-system-release release system-name)
              (context-system-releases context))))))

(defmethod process-form (context (section (eql :reverse-dependencies)) form))

(defmethod process-form (context (section (eql :sources)) form)
  (let ((source (load-source-from-form form :installed-only-p (context-installed-only-p context))))
    (when (typep source 'fs-source)
      (push (project-release (source-project source :all) :newest)
            (context-releases context)))
    (unless (or (source-can-lazy-sync-p source)
                (config-value :local)
                (context-installed-only-p context))
      (sync-source source))
    (setf (context-sources context) (append (context-sources context) (list source)))))

(defun load-context-from-stream (stream &key pathname (sources nil sources-provided-p)
                                          installed-only-p)
  (uiop:with-safe-io-syntax ()
    ;; The first form in the stream must be an API declaration.
    (let ((f (read stream nil)))
      (unless (equal f '(:api-version "0.3"))
        (error "Unknown context API version")))
    (let ((out (apply #'make-instance
                      'context
                      :sources sources
                      :installed-only-p installed-only-p
                      (when pathname
                        (list :pathname pathname)))))
      ;; The next forms are either tags or lists. The tags denote sections.
      (loop
        :with section := nil
        :for form := (read stream nil :eof)
        :until (eql form :eof)
        :when (symbolp form)
          :do (check-section-valid section form)
              (setf section form)
        :else
          :do (unless (and (eql section :sources) sources-provided-p)
                (with-sources ((context-sources out))
                  (process-form out section form))))
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

        ;; Streams
        (write-section-header "sources" stream)
        (prin1 :sources stream)
        (pprint-newline :mandatory stream)
        (pprint-logical-block (stream (context-sources context))
          (pprint-exit-if-list-exhausted)
          (loop
            (serialize-context-source (pprint-pop) stream)
            (pprint-exit-if-list-exhausted)
            (pprint-newline :mandatory stream)))
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
        (pprint-logical-block (stream (sort (copy-list (context-releases context)) #'string<
                                            :key (compose #'project-name #'release-project)))
          (pprint-exit-if-list-exhausted)
          (loop
            (let ((release (pprint-pop)))
              (unless (typep release 'fs-release)
                (serialize-context-release release
                                           (remove-if-not (lambda (x)
                                                            (eql x release))
                                                          (context-system-releases context)
                                                          :key #'system-release-release)
                                           stream))
              (pprint-exit-if-list-exhausted)
              (unless (typep release 'fs-release)
                (pprint-newline :mandatory stream)))))
        (pprint-newline :mandatory stream)
        (pprint-newline :mandatory stream)

        ;; Reverse Dependencies
        (write-section-header "reverse-dependencies" stream)
        (prin1 :reverse-dependencies stream)
        (pprint-newline :mandatory stream)
        (pprint-logical-block (stream (sort (copy-list (context-reverse-dependencies context)) #'string<
                                            :key (compose #'project-name #'release-project #'car)))
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

;; ** Sources

(defun serialize-context-source (source stream)
  (prin1 (source-to-form source) stream))

;; ** Releases

(defmethod extract-why ((why clpm-system-release))
  (let* ((system (system-release-system why)))
    (list :system :name (system-name system))))

(defmethod extract-why ((why (eql t)))
  t)

(defun serialize-reverse-dep (req stream)
  (let ((why (requirement-why req)))
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
    ;; Filter out the requirements where we didn't know the true reason for the
    ;; requirement at the time (likely because we were trying to load an ASD
    ;; file into the groveler). We can ignore it because we *should* have the
    ;; true reason elsewhere in the list.
    (setf reverse-deps (remove :grovel (remove t reverse-deps)
                               :key #'requirement-why))
    (let ((project (release-project release)))
      (pprint-logical-block (stream nil :prefix "(" :suffix ")")
        ;; Name
        (prin1 (project-name project) stream)
        ;; Reverse deps
        (pprint-newline :mandatory stream)
        (pprint-logical-block (stream (sort (copy-list reverse-deps)
                                            (lambda (x y)
                                              (cond
                                                ((eql x t)
                                                 t)
                                                ((eql y t)
                                                 nil)
                                                (t
                                                 (string< (system-name (system-release-system x))
                                                          (system-name (system-release-system y))))))
                                            :key #'requirement-why))
          (pprint-exit-if-list-exhausted)
          (loop
            (let* ((req (pprint-pop)))
              (serialize-reverse-dep req stream)
              (pprint-exit-if-list-exhausted)
              (pprint-newline :mandatory stream))))))))

;; ** Requirements

(defgeneric serialize-context-requirement (req stream))

(defgeneric requirement-type-keyword (req))

(defmethod requirement-type-keyword ((req project-requirement))
  :project)

(defmethod requirement-type-keyword ((req system-requirement))
  :system)

(defmethod requirement-type-keyword ((req fs-system-requirement))
  :asd-system)

(defmethod requirement-type-keyword ((req fs-system-file-requirement))
  :asd-file)

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
    (prin1 (requirement-name req) stream)
    ;; Branch
    (when (requirement-branch req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :branch stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (requirement-branch req) stream))
    ;; Commit
    (when (requirement-commit req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :commit stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (requirement-commit req) stream))
    ;; Tag
    (when (requirement-tag req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :tag stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (requirement-tag req) stream))
    ;; Ref
    (when (requirement-ref req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :ref stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (requirement-ref req) stream))
    ;; Source
    (when (requirement-source req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :source stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (source-name (requirement-source req)) stream))
    ;; no deps
    (when (requirement-no-deps-p req)
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
    (prin1 (requirement-name req) stream)
    ;; Version
    (when (requirement-version-spec req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :version stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (requirement-version-spec req) stream))
    ;; Source
    (when (requirement-source req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :source stream)
      (write-char #\Space stream)
      (pprint-newline :miser stream)
      (prin1 (source-name (requirement-source req)) stream))
    ;; no deps
    (when (requirement-no-deps-p req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :no-deps-p stream)
      (write-char #\Space stream)
      (prin1 t stream))))

(defmethod serialize-context-requirement ((req fs-system-requirement) stream)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (prin1 (requirement-type-keyword req) stream)
    ;; pathname
    (write-char #\Space stream)
    (pprint-newline :fill stream)
    (prin1 :pathname stream)
    (write-char #\Space stream)
    (prin1 (requirement-pathname req) stream)
    ;; Name
    (write-char #\Space stream)
    (pprint-newline :fill stream)
    (prin1 :name stream)
    (write-char #\Space stream)
    (pprint-newline :miser stream)
    (prin1 (requirement-name req) stream)
    ;; no deps
    (when (requirement-no-deps-p req)
      (write-char #\Space stream)
      (pprint-newline :fill stream)
      (prin1 :no-deps-p stream)
      (write-char #\Space stream)
      (prin1 t stream))))

(defmethod serialize-context-requirement ((req fs-system-file-requirement) stream)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (prin1 (requirement-type-keyword req) stream)
    ;; pathname
    (write-char #\Space stream)
    (pprint-newline :fill stream)
    (prin1 :pathname stream)
    (write-char #\Space stream)
    (prin1 (requirement-name req) stream)
    ;; no deps
    (when (requirement-no-deps-p req)
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
