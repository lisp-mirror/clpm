;;;; CLPI Server Integration - EXPERIMENTAL
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/clpi
    (:use #:cl
          #:alexandria
          #:clpm/context
          #:clpm/groveler
          #:clpm/requirement
          #:clpm/source)
  (:import-from #:jsown)
  (:export #:make-release-jsown))

(in-package #:clpm/clpi)

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

(defun make-release-jsown (project-name project-version release-url asds &key clpmfile)
  (let ((*active-groveler* (make-groveler))
        system-objs)
    (labels ((load-into-groveler (absolute-asd-pathname clpmfile)
               (handler-bind ((groveler-dependency-missing
                                (lambda (c)
                                  (when clpmfile
                                    (let* ((missing-system-spec (groveler-dependency-missing-system c))
                                           (missing-req (convert-asd-system-spec-to-req missing-system-spec))
                                           (missing-system-name (requirement-name missing-req))
                                           (matching-cons (find-if (lambda (x)
                                                                     (member missing-system-name x :test #'equal))
                                                                   (context-system-releases clpmfile)
                                                                   :key #'cdr))
                                           (matching-sr (when matching-cons
                                                          (release-system-release (car matching-cons)
                                                                                  missing-system-name))))
                                      (when (and matching-sr
                                                 (system-release-satisfies-version-spec-p
                                                  matching-sr
                                                  (requirement-version-spec missing-req)))
                                        (groveler-add-asd-and-retry
                                         (system-release-absolute-asd-pathname matching-sr))))))))
                 (active-groveler-load-asd absolute-asd-pathname)))
             (process-asd (asd clpmfile)
               (let ((absolute-asd-pathname (merge-pathnames asd)))
                 ;; Load the asd into the groveler
                 (load-into-groveler absolute-asd-pathname clpmfile)
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
                             system-objs)))))))
      (if clpmfile
          (with-context (clpmfile)
            (dolist (asd asds)
              (process-asd asd clpmfile)))
          (dolist (asd asds)
            (process-asd asd clpmfile)))
      (jsown:new-js
        ("projectName" project-name)
        ("version" project-version)
        ("url" release-url)
        ("archiveType" "tar.gz")
        ("systems" system-objs)))))
