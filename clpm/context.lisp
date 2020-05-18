;;;; Context
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/context
    (:use #:cl
          #:alexandria
          #:cl-ansi-text
          #:clpm/cache
          #:clpm/client
          #:clpm/config
          #:clpm/data
          #:clpm/log
          #:clpm/requirement
          #:clpm/source)
  (:export #:context
           #:context-add-requirement!
           #:context-asd-pathnames
           #:context-find-system-asd-pathname
           #:context-installed-systems
           #:context-installed-only-p
           #:context-output-translations
           #:context-releases
           #:context-requirements
           #:context-reverse-dependencies
           #:context-sources
           #:context-system-releases
           #:context-to-asdf-source-registry-form
           #:context-to-asdf-source-registry.d-forms
           #:context-visible-primary-system-names
           #:context-write-asdf-files
           #:copy-context
           #:get-context
           #:load-anonymous-context-from-pathname
           #:load-global-context
           #:save-context
           #:serialize-context-to-stream))

(in-package #:clpm/context)

(setup-logger)

(defclass context ()
  ((name
    :initarg :name
    :accessor context-name
    :documentation
    "The name of the context. Either a string (if this is a global context) or a
pathname (if this is an anonymous context).")
   (installed-only-p
    :initarg :installed-only-p
    :initform nil
    :reader context-installed-only-p
    :documentation
    "Hack (hopefully temporary) to indicate that sources should be instantiated
to read only from their local data.")
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
    :accessor context-system-releases))
  (:documentation
   "Represents a snapshot of a context. Includes sources, the releases installed
in the context, the requirements that gave rise to those releases, etc. Contexts
can be named, global contexts, or anonymous."))

(defun context-anonymous-p (context)
  (pathnamep (context-name context)))

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

;; TODO: 0.4
;;
;; See note on CONTEXT-VISIBLE-PRIMARY-SYSTEM-NAMES
(defun context-installed-systems (context)
  (let* ((context (get-context context))
         (system-releases (context-system-releases context)))
    (mapcar #'system-release-system system-releases)))

;; NOTE: There is some slightly wonky behavior here that the client currently
;; depends on when working with bundles, namely the system names from
;; CONTEXT-ASD-PATHNAMES do not show up. But this is ok since they do not show
;; up in CONTEXT-INSTALLED-SYSTEMS either. This prevents the client from
;; unnecessary prompting to install newly created package-inferred-systems in
;; bundles. Want to re-evaluate this choice and/or function name in 0.4.0.
;;
;; Actually, decided to make client not track these things in bundles since
;; BUNDLE-INSTALL doesn't take any system or project names anyways. But leaving
;; as a note to future self.
;;
;; TODO: 0.4
(defun context-visible-primary-system-names (context)
  (let* ((context (get-context context))
         (releases (context-releases context))
         (system-releases (context-system-releases context))
         (system-files (remove-duplicates (mapcar #'system-release-system-file system-releases)))
         (system-file-directories (remove-duplicates (mapcar (compose #'uiop:pathname-directory-pathname
                                                                      #'system-file-absolute-asd-pathname)
                                                             system-files)
                                                     :test #'uiop:pathname-equal))
         (all-system-files (mappend #'release-system-files releases))
         (visible-system-files (remove-if-not (lambda (sf)
                                                (member (uiop:pathname-directory-pathname
                                                         (system-file-absolute-asd-pathname sf))
                                                        system-file-directories
                                                        :test #'uiop:pathname-equal))
                                              all-system-files)))
    (mapcar #'system-file-primary-system-name visible-system-files)))

(defun context-to-asdf-source-registry.d-forms (context)
  (let* ((context (get-context context))
         (system-file-pathnames (context-asd-pathnames context))
         (system-file-directories (remove-duplicates (mapcar #'uiop:pathname-directory-pathname system-file-pathnames)
                                                     :test #'uiop:pathname-equal)))
    (mapcar (lambda (x) (list :directory x)) system-file-directories)))

(defun context-to-asdf-source-registry-form (context &key extra-forms
                                                       ignore-inherited
                                                       splice-inherited
                                                       with-client)
  `(:source-registry
    ,@(context-to-asdf-source-registry.d-forms context)
    ,@(when with-client
        `((:directory ,(uiop:pathname-directory-pathname (client-asd-pathname)))))
    ,@extra-forms
    ,@(cond
        (ignore-inherited
         (list :ignore-inherited-configuration))
        ((and (stringp splice-inherited) (not (equal splice-inherited "")))
         (rest (asdf/source-registry:parse-source-registry-string splice-inherited)))
        ((and splice-inherited (listp splice-inherited))
         (rest splice-inherited))
        (t
         (list :inherit-configuration)))))

(defun context-find-system-asd-pathname (context system-name)
  (when-let* ((context (get-context context))
              (system-release (find system-name (context-system-releases context)
                                    :key (compose #'system-name #'system-release-system)
                                    :test #'equal)))
    (system-release-absolute-asd-pathname system-release)))

(defun context-write-asdf-files (context)
  (unless (context-anonymous-p context)
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
                (terpri s)))))))))

(defun context-output-translations (context)
  (assert (not (context-anonymous-p context)))
  (let* ((context (get-context context))
         (name (context-name context))
         (config-value (config-value :contexts name :output-translation)))
    (when config-value
      `(:output-translations
        :ignore-inherited-configuration
        (t (:root ,@(rest (pathname-directory (clpm-cache-pathname `("contexts" ,name "fasl-cache")
                                                                   :ensure-directory t)))
            :implementation :**/ :*.*.*))))))


;; * Deserializing

(defun load-anonymous-context-from-pathname (pn &key installed-only-p)
  (with-open-file (s pn)
    (load-context-from-stream s :installed-only-p installed-only-p)))

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

(defun load-context-from-stream (stream &key (sources nil sources-provided-p)
                                          installed-only-p)
  (uiop:with-safe-io-syntax ()
    ;; The first form in the stream must be an API declaration.
    (let ((f (read stream nil)))
      (unless (equal f '(:api-version "0.3"))
        (error "Unknown context API version")))
    (let ((out (make-instance 'context
                              :sources sources
                              :installed-only-p installed-only-p)))
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

(defun save-context (context)
  (assert (context-reverse-dependencies context))
  (let ((pn (if (context-anonymous-p context)
                (context-name context)
                (global-context-pathname (context-name context)))))
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
      (format stream "~S~%~%" '(:api-version "0.3"))

      ;; Sources
      (write-section-header "sources" stream)
      (format stream ":sources~%")
      (dolist (source (context-sources context))
        (format stream "~S~%" (source-to-form source)))
      (terpri stream)
      (terpri stream)

      ;; Requirements
      (write-section-header "requirements" stream)
      (format stream ":requirements~%")
      (dolist (req (context-requirements context))
        (format stream "~S~%" (list* (requirement-type-keyword req)
                                     (requirement-to-plist req))))
      (terpri stream)
      (terpri stream)

      ;; Releases
      (write-section-header "releases" stream)
      (format stream ":releases~%")
      (dolist (release (sort (copy-list (context-releases context)) #'string<
                             :key (compose #'project-name #'release-project)))
        (unless (typep release 'fs-release)
          (format stream "~S~%"
                  (context-release-to-form release (remove-if-not (lambda (x)
                                                                    (eql x release))
                                                                  (context-system-releases context)
                                                                  :key #'system-release-release)))))
      (terpri stream)
      (terpri stream)

      ;; Reverse Dependencies
      (write-section-header "reverse-dependencies" stream)
      (format stream ":reverse-dependencies~%")
      (dolist (reverse-dep (sort (copy-list (context-reverse-dependencies context)) #'string<
                                 :key (compose #'project-name #'release-project #'car)))
        (format stream "~S~%~%" (context-reverse-deps-to-form reverse-dep))))))

;; ** Releases

(defmethod extract-why ((why clpm-system-release))
  (let* ((system (system-release-system why)))
    (list :system :name (system-name system))))

(defmethod extract-why ((why (eql t)))
  t)

(defun reverse-dep-to-form (req)
  (let ((why (requirement-why req)))
    `(,(extract-why why)
      ,(list* (requirement-type-keyword req)
              (requirement-to-plist req)))))

(defun context-release-to-form (release system-releases)
  `(,(project-name (release-project release))
    :version ,(release-version release)
    :source ,(source-name (release-source release))
    :systems ,(mapcar (compose #'system-name #'system-release-system) system-releases)))

(defun context-reverse-deps-to-form (release-and-reverse-deps)
  (destructuring-bind (release . reverse-deps) release-and-reverse-deps
    ;; Filter out the requirements where we didn't know the true reason for the
    ;; requirement at the time (likely because we were trying to load an ASD
    ;; file into the groveler). We can ignore it because we *should* have the
    ;; true reason elsewhere in the list.
    (setf reverse-deps (remove :grovel (remove t reverse-deps)
                               :key #'requirement-why))
    (let ((project (release-project release))
          reverse-dep-forms)
      (dolist (reverse-dep (sort (copy-list reverse-deps)
                                 (lambda (x y)
                                   (cond
                                     ((eql x t) t)
                                     ((eql y t) nil)
                                     (t (string< (system-name (system-release-system x))
                                                 (system-name (system-release-system y))))))
                                 :key #'requirement-why))
        (push (reverse-dep-to-form reverse-dep) reverse-dep-forms))
      `(,(project-name project) ,@reverse-dep-forms))))

;; ** Requirements

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
