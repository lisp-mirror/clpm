;;;; Context
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/context
    (:use #:cl
          #:alexandria
          #:anaphora
          #:cl-ansi-text
          #:clpm/cache
          #:clpm/client
          #:clpm/config
          #:clpm/data
          #:clpm/log
          #:clpm/repos
          #:clpm/requirement
          #:clpm/source
          #:clpm/utils
          #:do-urlencode)
  (:export #:context
           #:context-add-requirement!
           #:context-anonymous-p
           #:context-asd-pathnames
           #:context-editable-primary-system-names
           #:context-find-system-asd-pathname
           #:context-fs-sources-ht
           #:context-installed-primary-system-names
           #:context-installed-system-names
           #:context-name
           #:context-output-translations
           #:context-releases
           #:context-requirements
           #:context-reverse-dependencies
           #:context-sources
           #:context-system-releases
           #:context-to-asdf-source-registry-form
           #:context-to-asdf-source-registry.d-forms
           #:context-user-sources
           #:context-vcs-sources
           #:context-vcs-sources-ht
           #:context-visible-primary-system-names
           #:context-write-asdf-files
           #:copy-context
           #:load-anonymous-context-from-pathname
           #:save-context
           #:serialize-context-to-stream
           #:with-context))

(in-package #:clpm/context)

(setup-logger)

(defclass context ()
  ((name
    :initarg :name
    :accessor context-name
    :documentation
    "The name of the context. Either a string (if this is a global context) or a
pathname (if this is an anonymous context).")
   (requirements
    :initform nil
    :initarg :requirements
    :accessor context-requirements)
   (fs-sources-ht
    :initarg :fs-sources-ht
    :initform (make-hash-table :test 'equal)
    :accessor context-fs-sources-ht
    :documentation
    "The implicit filesystem sources rooted for this context.")
   (vcs-sources-ht
    :initarg :vcs-sources-ht
    :initform (make-hash-table :test 'equal)
    :accessor context-vcs-sources-ht
    :documentation
    "The implicit VCS sources rooted for this context.")
   (user-sources
    :initform nil
    :initarg :user-sources
    :accessor context-user-sources)
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
    :accessor context-system-releases
    :documentation
    "An alist mapping release objects to strings naming the systems installed as
    part of the release. SYSTEM-RELEASE objects must *not* be used here, because
    they are not guaranteed to be constructable when the context is read (e.g.,
    in the case of VCS repos where commits have been deleted)."))
  (:documentation
   "Represents a snapshot of a context. Includes sources, the releases installed
in the context, the requirements that gave rise to those releases, etc. Contexts
can be named, global contexts, or anonymous."))

(defmethod context-fs-sources ((context context))
  (hash-table-values (context-fs-sources-ht context)))

(defmethod context-vcs-sources ((context context))
  (hash-table-values (context-vcs-sources-ht context)))

(defgeneric context-name (context))

(defmethod context-name ((context pathname))
  context)

(defmethod context-name ((context string))
  context)

(defmethod context-name ((context (eql nil)))
  (config-value :context))

(defgeneric context-anonymous-p (context))

(defmethod context-anonymous-p ((context context))
  (pathnamep (context-name context)))

(defmethod context-anonymous-p ((context pathname))
  t)

(defmethod context-anonymous-p ((context string))
  nil)

(defmethod context-anonymous-p ((context (eql nil)))
  nil)

(defun copy-context (context)
  (make-instance 'context
                 :name (context-name context)
                 :releases (copy-list (context-releases context))
                 :requirements (copy-list (context-requirements context))
                 :reverse-dependencies (copy-alist (context-reverse-dependencies context))
                 :user-sources (copy-list (context-user-sources context))
                 :fs-sources-ht (copy-hash-table (context-fs-sources-ht context))
                 :vcs-sources-ht (copy-hash-table (context-vcs-sources-ht context))
                 :system-releases (copy-tree (context-system-releases context))))

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
    ((pathnamep context-designator)
     (load-anonymous-context-from-pathname (merge-pathnames (make-pathname :type "lock")
                                                            context-designator)))
    (t
     (error "Unable to translate ~S to a context object" context-designator))))

(defun context-sources (context)
  (append (context-fs-sources context)
          (context-vcs-sources context)
          (context-user-sources context)))

(defun make-vcs-override-fun (root-pathname)
  (let ((root-pathname (uiop:pathname-directory-pathname root-pathname)))
    (lambda (project-name)
      (let ((override (config-value :bundle :local project-name)))
        (when override
          (merge-pathnames override root-pathname))))))

(defun call-with-context (thunk context-designator)
  (if (context-anonymous-p context-designator)
      (let* ((context-name (context-name context-designator))
             (*default-pathname-defaults* (uiop:pathname-directory-pathname context-name))
             (*vcs-project-override-fun* (make-vcs-override-fun *default-pathname-defaults*)))
        (with-config-source (:pathname (merge-pathnames ".clpm/bundle.conf"
                                                        (uiop:pathname-directory-pathname context-name)))
          (funcall thunk (get-context context-designator))))
      (funcall thunk (get-context context-designator))))

(defmacro with-context ((context-var &optional (context-value context-var)) &body body)
  `(call-with-context (lambda (,context-var) ,@body) ,context-value))


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
    (when (typep req 'fs-system-file-requirement)
      (ensure-gethash (requirement-name req) (context-fs-sources-ht context)
                      (make-source 'fs-source :name (requirement-name req))))
    (when (typep req 'fs-system-requirement)
      (ensure-gethash (requirement-pathname req) (context-fs-sources-ht context)
                      (make-source 'fs-source :name (requirement-pathname req))))
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

(defun context-editable-primary-system-names (context)
  (let* ((releases (context-releases context))
         (editable-releases (remove-if-not (lambda (x) (member x (context-fs-sources context)))
                                           releases
                                           :key #'release-source))
         (editable-system-files (mapcan #'release-system-files editable-releases)))
    (sort (remove-duplicates (mapcar #'system-file-primary-system-name editable-system-files)
                        :test #'equal)
          #'string<)))

(defun context-installed-primary-system-names (context)
  (let* ((context (get-context context))
         (system-releases (context-system-releases context)))
    (remove-duplicates
     (mappend (lambda (x) (mapcar #'asdf:primary-system-name (cdr x)))
              system-releases)
     :test #'equal)))

(defun context-installed-system-names (context)
  (let* ((context (get-context context))
         (system-releases (context-system-releases context)))
    (mappend #'cdr system-releases)))

(defun context-visible-primary-system-names (context)
  (let* ((context (get-context context))
         (releases (context-releases context))
         (system-release-alist (context-system-releases context))
         (system-releases (mappend (lambda (x)
                                     (mapcar (lambda (system-name)
                                               (release-system-release (car x) system-name))
                                             (cdr x)))
                                   system-release-alist))
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
              (system-release-cons (find-if (lambda (x)
                                              (member system-name x :test 'equal))
                                            (context-system-releases context)
                                            :key #'cdr))
              (system-release (release-system-release (car system-release-cons) system-name)))
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
  (if (context-anonymous-p context)
      (let ((pathname (context-name context)))
        (case (config-value :bundle :output-translation)
          ((t)
           `(:output-translations
             :ignore-inherited-configuration
             (t (:root ,@(rest (pathname-directory (clpm-cache-pathname '("bundle" "fasl-cache")
                                                                        :ensure-directory t)))
                       ,(urlencode (format nil "~{~A~^/~}" (rest (pathname-directory pathname))))
                       :implementation :**/ :*.*.*))))
          (:local
           `(:output-translations
             :ignore-inherited-configuration
             (t (,(uiop:pathname-directory-pathname pathname) ".clpm" "fasl-cache"
                 :implementation :**/ :*.*.*))))
          (t
           nil)))
      (let* ((context (get-context context))
             (name (context-name context))
             (config-value (config-value :contexts name :output-translation)))
        (when config-value
          `(:output-translations
            :ignore-inherited-configuration
            (t (:root ,@(rest (pathname-directory (clpm-cache-pathname `("contexts" ,name "fasl-cache")
                                                                       :ensure-directory t)))
                :implementation :**/ :*.*.*)))))))


;; * Deserializing

(defun load-anonymous-context-from-pathname (pn)
  (if (probe-file pn)
      (with-open-file (s pn)
        (load-context-from-stream s pn))
      (make-instance 'context
                     :name pn)))

(defun context-downselect-sources (name sources)
  (let ((allowed-source-names (config-value :contexts name :sources)))
    (if (eql t allowed-source-names)
        sources
        (remove-if-not (lambda (x) (member (source-name x) allowed-source-names :test 'equal))
                       sources))))

(defun load-global-context (name &optional (error t))
  ;; Global contexts can have their sources downselected by the user config. We
  ;; accomplish this by setting the sources for the context after it has loaded.
  (let ((pn (global-context-pathname name)))
    (with-open-file (s pn
                       :if-does-not-exist (if error :error nil))
      (if s
          (aprog1 (load-context-from-stream s name)
            (setf (context-user-sources it) (context-downselect-sources name (sources))))
          (make-instance 'context
                         :name name
                         :user-sources (context-downselect-sources name (sources)))))))

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
    (context-add-requirement! context
                              (cond
                                ((eql type :asd-system)
                                 (make-instance 'fs-system-requirement
                                                :name name
                                                :pathname pathname
                                                :no-deps-p no-deps-p
                                                :why t))
                                ((eql type :asd-file)
                                 (make-instance 'fs-system-file-requirement
                                                :name name
                                                :no-deps-p no-deps-p
                                                :why t))
                                ((or branch tag commit ref)
                                 (make-instance 'vcs-project-requirement
                                                :name name
                                                :source (when (eql source :implicit-vcs)
                                                          (get-vcs-source-for-project name))
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
                                                :why t))))))

(defmethod process-form (context (section (eql :releases)) form)
  (destructuring-bind (name &key version source systems) form
    (let* ((source (cond
                     ((eql source :implicit-file)
                      (get-source name))
                     ((eql source :implicit-vcs)
                      (get-vcs-source-for-project name))
                     (t
                      (get-source source))))
           (release (source-project-release source name version)))
      (push release
            (context-releases context))
      (dolist (system-name systems)
        (push system-name
              (assoc-value (context-system-releases context) release))))))

(defmethod process-form (context (section (eql :reverse-dependencies)) form))

(defmethod process-form (context (section (eql :sources)) form)
  (case (first form)
    (:implicit-vcs
     (dolist (project-description (getf (rest form) :projects))
       (let* ((project-name (car project-description))
              (repo-description (cdr project-description))
              (repo (make-repo-from-description repo-description)))
         (ensure-gethash (repo-to-form repo)
                         (context-vcs-sources-ht context)
                         (make-source 'vcs-source :repo repo-description
                                                  :project-name project-name)))))
    (:implicit-file
     (dolist (sf (getf (rest form) :system-files))
       (ensure-gethash sf (context-fs-sources-ht context)
                       (make-source 'fs-source :name sf))))
    (t
     (let ((source (load-source-from-form form)))
       (unless (or (source-can-lazy-sync-p source)
                   (config-value :local))
         (sync-source source))
       (setf (context-user-sources context) (append (context-user-sources context) (list source)))))))

(defun load-context-from-stream (stream name)
  (uiop:with-safe-io-syntax ()
    ;; The first form in the stream must be an API declaration.
    (let ((f (read stream nil)))
      (unless (equal f '(:api-version "0.3"))
        (error "Unknown context API version")))
    (let ((out (make-instance 'context
                              :name name)))
      ;; The next forms are either tags or lists. The tags denote sections.
      (loop
        :with section := nil
        :for form := (read stream nil :eof)
        :until (eql form :eof)
        :when (symbolp form)
          :do (check-section-valid section form)
              (setf section form)
        :else
          :do (with-sources ((context-sources out))
                (process-form out section form)))
      out)))


;; * Serializing

(defun save-context (context)
  (assert (context-reverse-dependencies context))
  (let ((pn (if (context-anonymous-p context)
                (merge-pathnames (make-pathname :type "lock") (context-name context))
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
        (let ((form (source-to-form source)))
          (format stream "~S~%" form)))
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
      (dolist (release (safe-sort (context-releases context) #'string<
                                  :key (compose #'project-name #'release-project)))
        (format stream "~S~%"
                (context-release-to-form release (assoc-value (context-system-releases context)
                                                              release))))
      (terpri stream)
      (terpri stream)

      ;; Reverse Dependencies
      (write-section-header "reverse-dependencies" stream)
      (format stream ":reverse-dependencies~%")
      (dolist (reverse-dep (safe-sort (context-reverse-dependencies context) #'string<
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

(defun context-release-to-form (release system-names)
  `(,(project-name (release-project release))
    :version ,(release-version release)
    ;; TODO: Bundle v0.4 maybe replace with the .asd pathname instead of
    ;; :IMPLICIT-FILE? Same with :IMPLICIT-VCS.
    :source ,(typecase (release-source release)
               (fs-source
                :implicit-file)
               (vcs-source
                :implicit-vcs)
               (t
                (source-name (release-source release))))
    :systems ,(safe-sort system-names #'string<)))

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
      (dolist (reverse-dep (safe-sort reverse-deps
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
