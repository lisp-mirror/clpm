;;;; clpm clpi release - EXPERIMENTAL
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/clpi/release
    (:use #:cl
          #:alexandria
          #:clpm/cli/common-args
          #:clpm/cli/clpi/common
          #:clpm/cli/interface-defs
          #:clpm/context
          #:clpm/groveler
          #:clpm/requirement
          #:clpm/source
          #:clpm/utils
          #:do-urlencode)
  (:import-from #:adopt)
  (:import-from #:jsown)
  (:import-from #:uiop
                #:*stdout*))

(in-package #:clpm/cli/clpi/release)

(defparameter *option-asds*
  (adopt:make-option
   :asds
   :long "asd"
   :help "Grovel this ASD. Can be specified multiple-times."
   :parameter "ASD-PATHNAME"
   :reduce (adopt:flip #'cons)))

(defparameter *option-deps-from-lockfile*
  (adopt:make-option
   :deps-from-lockfile
   :long "deps-from-lockfile"
   :help "Load any dpendencies needed for grovelling from the lockfile in the current directory."
   :reduce (constantly t)))

(define-string *help-string*
  "EXPERIMENTAL

Prototype interface to produce a JSON object containing data about a new release
of a project. This *will* change over time. Ideally, this will also integrate
with an HTTP client to submit the data to a CLPI server.")

(defparameter *clpi-release-ui*
  (adopt:make-interface
   :name "clpm clpi systems"
   :summary "Print systems forms for CLPI"
   :usage "clpi systems [options]"
   :help *help-string*
   :contents (list *group-common*
                   *option-asds*
                   *option-deps-from-lockfile*)))

(defun sort-dependencies (deps)
  (labels ((find-system-name (dep)
             (if (listp dep)
                 (ecase (first dep)
                   (:version
                    (second dep))
                   (:feature
                    (find-system-name (third dep)))
                   (:require
                    (second dep)))
                 dep)))
    (sort (copy-list (remove-duplicates deps :test #'equal))
          #'string< :key #'find-system-name)))

(defun asdf-feature-expression-to-jsown (f)
  (if (listp f)
      (ecase (first f)
        ((:and :or)
         (jsown:new-js
           ("type" (string-downcase (symbol-name (first f))))
           ("features" (mapcar #'asdf-feature-expression-to-jsown (rest f)))))
        (:not
         (jsown:new-js
           ("type" "not")
           ("feature" (asdf-feature-expression-to-jsown (second f))))))
      (string-downcase (symbol-name f))))

(defgeneric asdf-complex-dep-to-jsown (type args))

(defmethod asdf-complex-dep-to-jsown ((type (eql :feature)) args)
  (jsown:new-js
    ("type" "feature")
    ("featureExpression" (asdf-feature-expression-to-jsown (first args)))
    ("dependency" (second args))))

(defmethod asdf-complex-dep-to-jsown ((type (eql :require)) args)
  (jsown:new-js
    ("type" "require")
    ("name" (first args))))

(defmethod asdf-complex-dep-to-jsown ((type (eql :version)) args)
  (jsown:new-js
    ("type" "version")
    ("name" (first args))
    ("version" (second args))))

(defun asdf-dep-to-jsown (dep)
  (if (stringp dep)
      dep
      (asdf-complex-dep-to-jsown (first dep) (rest dep))))

(define-cli-command (("clpi" "release") *clpi-release-ui*) (arguments options)
  (destructuring-bind (project-name project-version release-url) arguments
    (let ((asds (gethash :asds options))
          (*active-groveler* (make-groveler))
          (deps-from-lockfile-p (gethash :deps-from-lockfile options))
          (context)
          system-objs)
      (when deps-from-lockfile-p
        (setf context (load-anonymous-context-from-pathname (merge-pathnames "clpmfile.lock"))))
      (uiop:with-safe-io-syntax ()
        (let ((*print-case* :downcase))
          (dolist (asd asds)
            (let ((absolute-asd-pathname (merge-pathnames asd)))
              ;; Load the asd into the groveler
              (handler-bind ((groveler-dependency-missing
                               (lambda (c)
                                 (when context
                                   (let* ((missing-system-spec (groveler-dependency-missing-system c))
                                          (missing-req (convert-asd-system-spec-to-req missing-system-spec))
                                          (missing-system-name (requirement-name missing-req))
                                          (matching-sr (find missing-system-name
                                                             (context-system-releases context)
                                                             :test #'equal
                                                             :key (compose #'system-name #'system-release-system))))
                                     (when (and matching-sr
                                                (system-release-satisfies-version-spec-p
                                                 matching-sr
                                                 (requirement-version-spec missing-req)))
                                       (groveler-add-asd-and-retry
                                        (system-release-absolute-asd-pathname matching-sr))))))))
                (active-groveler-load-asd absolute-asd-pathname))


              (dolist (system-name (active-groveler-systems-in-file absolute-asd-pathname))
                (destructuring-bind (&key depends-on version defsystem-depends-on loaded-systems
                                       license description
                                     &allow-other-keys)
                    (active-groveler-system-deps system-name)
                  (let ((all-deps (append depends-on defsystem-depends-on loaded-systems))
                        (system-obj (jsown:new-js
                                      ("name" system-name)
                                      ("systemFile" asd))))
                    (when version
                      (jsown:extend-js system-obj
                        ("version" version)))
                    (when description
                      (jsown:extend-js system-obj
                        ("description" description)))
                    (when license
                      (jsown:extend-js system-obj
                        ("license" (uiop:with-safe-io-syntax ()
                                     (write-to-string license)))))
                    (when all-deps
                      (jsown:extend-js system-obj
                        ("dependencies" (mapcar #'asdf-dep-to-jsown all-deps))))
                    (push system-obj
                          system-objs))))))))

      (write-string (jsown:to-json
                     (jsown:new-js
                       ("projectName" project-name)
                       ("version" project-version)
                       ("url" release-url)
                       ("archiveType" "tar.gz")
                       ("systems" system-objs))))
      (terpri *standard-output*)))
  t)
