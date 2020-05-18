;;;; Basic installation support
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/install
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/context
          #:clpm/context-diff
          #:clpm/install/defs
          #:clpm/log
          #:clpm/resolve
          #:clpm/requirement
          #:clpm/session
          #:clpm/source
          #:clpm/version-strings)
  (:export #:install
           #:install-release
           #:install-requirements))

(in-package #:clpm/install)

(setup-logger)

(defun parse-specifier (specifier)
  (let (version
        ref
        source
        (parts (uiop:split-string specifier :max 2 :separator '(#\:))))
    ;; Look for a source
    (when (length= 2 parts)
      (when (eql #\: (last-elt (first parts)))
        ;; We have a source!
        (setf source (second parts)
              specifier (subseq (first parts) 0 (1- (length (first parts)))))))

    ;; Look for a VCS ref.
    (setf parts (uiop:split-string specifier :max 2 :separator '(#\@)))
    (when (length= 2 parts)
      ;; We have a ref!
      (setf ref (second parts)
            specifier (first parts)))

    ;; Look for a version constraint.
    (setf parts (uiop:split-string specifier :max 2 :separator '(#\:)))
    (when (length= 2 parts)
      ;; We have a version constraint!
      (setf version (second parts)
            specifier (first parts)))
    (values specifier version ref source)))

(defun make-requirement (specifier type
                         &key ((:version default-version)) ((:ref default-ref))
                           ((:source default-source)) no-deps-p)
  (multiple-value-bind (name spec-version spec-ref spec-source)
      (parse-specifier specifier)
    (let* ((version-spec (parse-version-specifier (or spec-version default-version)))
           (ref (or spec-ref default-ref))
           (source (or spec-source default-source))
           (common-args (list :name name
                              :source source
                              :why t
                              :no-deps-p no-deps-p)))
      (cond
        ((and (eql :project type)
              (or spec-ref (and default-ref (not spec-version))))
         ;; Requesting that we install a project from vcs.
         (apply #'make-instance 'vcs-project-requirement
                :ref ref
                common-args))
        ((eql :project type)
         ;; Install a project, not from VCS.
         (apply #'make-instance 'project-requirement
                :version-spec version-spec
                common-args))
        ((eql type :system)
         ;; Install a system.
         (apply #'make-instance 'system-requirement
                :version-spec version-spec
                common-args))))))

(defun install (&key projects systems asds
                  version ref source
                  no-deps-p
                  (validate (constantly t))
                  context
                  save-context-p)
  "Install a set of projects and systems.

PROJECTS and SYSTEMS must be lists of dependency specifiers. VERSION, REF, and
SOURCE must be strings and are used as the default constraints on PROJECTS and
SYSTEMS if such constraints cannot be extracted from the specifiers themselves.

ASDs must be a list of pathnames to .asd files. All systems in each .asd file
will be installed.

VALIDATE must be a function of one argument (a diff) and returns non-NIL if the
install should proceed."
  (with-clpm-session ()
    (let ((reqs (append (mapcar (rcurry #'make-requirement
                                        :project
                                        :version version :source source :ref ref :no-deps-p no-deps-p)
                                projects)
                        (mapcar (rcurry #'make-requirement
                                        :system
                                        :version version :source source :ref ref :no-deps-p no-deps-p)
                                systems)
                        (mapcar (lambda (x)
                                  (make-instance 'fs-system-file-requirement
                                                 :name x
                                                 :why t
                                                 :no-deps-p no-deps-p))
                                asds))))
      (install-requirements reqs :context context :validate validate :save-context-p save-context-p))))

(defun install-requirements (reqs &key
                                    context
                                    (validate (constantly t))
                                    save-context-p
                                    update-projects)
  (let* ((orig-context (get-context context))
         (new-context (copy-context orig-context)))
    (dolist (r reqs)
      (context-add-requirement! new-context r))
    (setf new-context (resolve-requirements new-context :update-projects update-projects))
    (let ((result (funcall validate (make-context-diff orig-context new-context))))
      (if result
          (progn
            (mapc #'install-release (context-releases new-context))
            (when save-context-p
              (context-write-asdf-files new-context)
              (save-context new-context))
            new-context)
          orig-context))))
