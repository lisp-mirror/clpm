;;;; clpm clpi systems
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/clpi/systems
    (:use #:cl
          #:alexandria
          #:clpm/cli/common-args
          #:clpm/cli/clpi/common
          #:clpm/cli/subcommands
          #:clpm/context
          #:clpm/groveler
          #:clpm/source
          #:clpm/utils
          #:do-urlencode)
  (:import-from #:adopt)
  (:import-from #:uiop
                #:*stdout*))

(in-package #:clpm/cli/clpi/systems)

(defparameter *option-project-name*
  (adopt:make-option
   :project-name
   :long "project-name"
   :help "The name of the project"
   :parameter "PROJECT-NAME"
   :reduce #'adopt:last))

(defparameter *option-project-version*
  (adopt:make-option
   :project-version
   :long "project-version"
   :help "The version of the release"
   :parameter "PROJECT-VERSION"
   :reduce #'adopt:last))

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

(defparameter *clpi-systems-ui*
  (adopt:make-interface
   :name "clpm clpi systems"
   :summary "Print systems forms for CLPI"
   :usage "clpi systems [options]"
   :help "Print systems forms for CLPI"
   :contents (list *group-common*
                   *option-project-name*
                   *option-project-version*
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

(define-cli-command (("clpi" "systems") *clpi-systems-ui*) (arguments options)
  (when arguments
    (warn "This command takes no arguments"))
  (let ((project-name (gethash :project-name options))
        (project-version (gethash :project-version options))
        (asds (gethash :asds options))
        (*active-groveler* (make-groveler))
        (deps-from-lockfile-p (gethash :deps-from-lockfile options))
        (context))
    (when deps-from-lockfile-p
      (setf context (load-anonymous-context-from-pathname (merge-pathnames "clpmfile.lock"))))
    (uiop:with-safe-io-syntax ()
      (let ((*print-case* :downcase))
        (dolist (asd asds)
          (let ((absolute-asd-pathname (merge-pathnames asd))
                (primary-systems (make-hash-table :test 'equal)))
            ;; Load the asd into the groveler
            (handler-bind ((groveler-dependency-missing
                             (lambda (c)
                               (when context
                                 (let* ((missing-system (groveler-dependency-missing/system c))
                                        (matching-sr (find missing-system (context-system-releases context)
                                                           :test #'equal
                                                           :key (compose #'system-name #'system-release-system))))
                                   (when matching-sr
                                     (invoke-restart 'add-asd-and-retry
                                                     (system-release-absolute-asd-pathname matching-sr))))))))
              (active-groveler-load-asd! absolute-asd-pathname))


            (dolist (system-name (active-groveler-systems-in-file absolute-asd-pathname))
              (destructuring-bind (&key depends-on version defsystem-depends-on loaded-systems
                                     license description
                                   &allow-other-keys)
                  (rest (active-groveler-system-deps system-name))
                (let ((primary-name (asdf:primary-system-name system-name))
                      (all-deps (append depends-on defsystem-depends-on loaded-systems)))
                  (if (equal primary-name system-name)
                      (progn
                        (setf (getf (gethash primary-name primary-systems) :system-file) asd)
                        (when version
                          (setf (getf (gethash primary-name primary-systems) :version) version))
                        (when description
                          (setf (getf (gethash primary-name primary-systems) :description) description))
                        (when license
                          (setf (getf (gethash primary-name primary-systems) :license) license))
                        (when all-deps
                          (setf (getf (gethash primary-name primary-systems) :dependencies)
                                (sort-dependencies all-deps))))
                      (progn
                        (push `(,system-name
                                ,@(when version
                                    (list :version version))
                                ,@(when all-deps
                                    (list :dependencies (sort-dependencies all-deps))))
                              (getf (gethash primary-name primary-systems) :secondary-systems)))))))
            (maphash
             (lambda (primary-system-name description)
               (format *stdout* "~A ~S~%"
                       (urlencode primary-system-name)
                       (list* (list project-name project-version)
                              (sort-plist description
                                          '(:system-file :version :description :license
                                            :dependencies :secondary-systems)))))
             primary-systems))))))
  t)
