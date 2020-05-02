;;;; Helpers for releases distributed as tarballs.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/sources/tarball-release
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/archives
          #:clpm/http-client
          #:clpm/install/defs
          #:clpm/log
          #:clpm/sources/defs)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-file
                #:md5)
  (:export #:source-tarball-archive-cache
           #:tarball-release
           #:tarball-release-desired-md5
           #:tarball-release-desired-size
           #:tarball-release-url
           #:tarball-release-cache-pathname))

(in-package #:clpm/sources/tarball-release)

(setup-logger)



(defclass tarball-release ()
  ()
  (:documentation
   "A release that is provided via a tarball that is hosted at a URL."))

(define-condition invalid-tarball (error)
  ((release
    :initarg :release
    :reader invalid-tarball-release)
   (pathname
    :initarg :pathname
    :reader invalid-tarball-pathname)))

(define-condition tarball-missing (invalid-tarball)
  ()
  (:report (lambda (condition stream)
             (format stream "The file ~A is missing." (invalid-tarball-pathname condition)))))

(define-condition tarball-md5-mismatch (invalid-tarball)
  ((actual
    :initarg :actual
    :reader tarball-md5-mismatch-actual)
   (expected
    :initarg :expected
    :reader tarball-md5-mismatch-expected))
  (:report (lambda (condition stream)
             (format stream "~A has md5sum ~A. Expected ~A."
                     (invalid-tarball-pathname condition)
                     (tarball-md5-mismatch-actual condition)
                     (tarball-md5-mismatch-expected condition)))))

(define-condition tarball-size-mismatch (invalid-tarball)
  ((actual
    :initarg :actual
    :reader tarball-size-mismatch-actual)
   (expected
    :initarg :expected
    :reader tarball-size-mismatch-expected))
  (:report (lambda (condition stream)
             (format stream "~A has size ~A. Expected ~A."
                     (invalid-tarball-pathname condition)
                     (tarball-size-mismatch-actual condition)
                     (tarball-size-mismatch-expected condition)))))

(defgeneric tarball-release-desired-md5 (release)
  (:documentation
   "Returns a hex string of the tarball's md5sum."))

(defgeneric tarball-release-desired-size (release)
  (:documentation
   "Returns an integer: the tarball's size in bytes."))

(defgeneric tarball-release-url (release)
  (:documentation
   "Returns a puri URL where the tarball is located.")
  (:method :around (release)
    (puri:parse-uri (call-next-method))))

(defgeneric validate-tarball (release pn)
  (:documentation
   "Raise an error iff the tarball appears to be invalid."))

(defmethod validate-tarball ((release tarball-release) pn)
  (unless (uiop:probe-file* pn)
    (error 'tarball-missing :pathname pn :release release))
  (awhen (tarball-release-desired-size release)
    (with-open-file (s pn :direction :input :element-type '(unsigned-byte 8))
      (log:debug "validating size")
      (let ((actual (file-length s)))
        (unless (= it actual)
          (error 'tarball-size-mismatch :pathname pn :release release
                                        :actual actual :expected it)))))
  (awhen (tarball-release-desired-md5 release)
    (let ((actual (byte-array-to-hex-string (digest-file 'md5 pn))))
      (log:debug "validating md5sum")
      (unless (string-equal actual it)
        (error 'tarball-md5-mismatch :pathname pn :release release
                                     :actual actual :expected it)))))



(defun source-tarball-archive-cache (source)
  "Return a pathname to the directory where ~source~ caches its archives."
  (uiop:resolve-absolute-location
   `(,(source-cache-directory source)
     "distfiles")
   :ensure-directory t))

(defgeneric tarball-release-cache-pathname (release))

(defmethod tarball-release-cache-pathname ((release tarball-release))
  (uiop:resolve-absolute-location
   `(,(source-tarball-archive-cache (release-source release))
     ,(uiop:strcat (project-name (release-project release))
                   "-"
                   (release-version release)
                   ".tar.gz"))))

(defun ensure-tarball-fetched (release)
  "Ensure that the files needed to install ~release~ have been downloaded."
  (let ((version-url (tarball-release-url release))
        (archive-pathname (tarball-release-cache-pathname release)))
    (ensure-file-fetched archive-pathname version-url
                         :hint :immutable)
    archive-pathname))

(defmethod install-release ((release tarball-release))
  (let* ((version-string (release-version release))
         (project (release-project release))
         (project-name (project-name project))
         (install-root (release-lib-pathname release)))
    (log:info "Installing ~A version ~A to ~A" project-name version-string
              install-root)
    (unless (uiop:probe-file* install-root)
      (let ((archive-pathname (ensure-tarball-fetched release)))
        (log:debug "Package distfiles located at ~A" archive-pathname)
        (validate-tarball release archive-pathname)
        (with-open-file (archive-stream archive-pathname
                                        :direction :input
                                        :element-type '(unsigned-byte 8))
          (unarchive 'gzipped-tar-archive
                     archive-stream install-root
                     :strip-components 1))))))
