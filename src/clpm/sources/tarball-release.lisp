;;;; Helpers for releases distributed as tarballs.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/sources/tarball-release
    (:use #:cl
          #:clpm/archives
          #:clpm/http-client
          #:clpm/install
          #:clpm/log
          #:clpm/sources/defs)
  (:import-from #:ironclad
                #:byte-array-to-hex-string
                #:digest-file
                #:md5)
  (:export #:tarball-release
           #:tarball-release/desired-md5
           #:tarball-release/desired-size
           #:tarball-release/url
           #:tarball-release-with-md5
           #:tarball-release-with-size))

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

(defgeneric tarball-release/url (release)
  (:documentation
   "Returns a puri URL where the tarball is located."))

(defgeneric validate-tarball (release pn)
  (:documentation
   "Raise an error iff the tarball appears to be invalid.")
  (:method-combination progn :most-specific-last)
  (:method progn ((release tarball-release) pn)
    (unless (uiop:probe-file* pn)
      (error 'tarball-missing :pathname pn :release release))))



(defclass tarball-release-with-md5 (tarball-release)
  ()
  (:documentation
   "A tarball release where we have an md5sum available."))

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

(defgeneric tarball-release/desired-md5 (release)
  (:documentation
   "Returns a hex string of the tarball's md5sum."))

(defmethod validate-tarball progn ((release tarball-release-with-md5) pn)
  (let ((actual (byte-array-to-hex-string (digest-file 'md5 pn)))
        (expected (tarball-release/desired-md5 release)))
    (log:debug "validating md5sum")
    (unless (string-equal actual expected)
      (error 'tarball-md5-mismatch :pathname pn :release release
                                   :actual actual :expected expected))))



(defclass tarball-release-with-size (tarball-release)
  ()
  (:documentation
   "A tarball release where we have a size available."))

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

(defgeneric tarball-release/desired-size (release)
  (:documentation
   "Returns an integer: the tarball's size in bytes."))

(defmethod validate-tarball progn ((release tarball-release-with-size) pn)
  (with-open-file (s pn :direction :input :element-type '(unsigned-byte 8))
    (log:debug "validating size")
    (let ((actual (file-length s))
          (expected (tarball-release/desired-size release)))
      (unless (= expected actual)
        (error 'tarball-size-mismatch :pathname pn :release release
                                      :actual actual :expected expected)))))



(defun source-archive-cache (source)
  "Return a pathname to the directory where ~source~ stores its archives."
  (uiop:resolve-absolute-location
   `(,(source/cache-directory source)
     "distfiles")
   :ensure-directory t))

(defun url-filename (url)
  "Return the filename of ~url~."
  (file-namestring (puri:uri-path url)))

(defun url-location (url)
  "Return a list of two elements. First is the ~url~ to fetch the file, the
second is the filename of the file located at ~url~."
  (if (listp url)
      (values (first url) (second url))
      (values url (url-filename url))))

(defun ensure-tarball-fetched (release)
  "Ensure that the files needed to install ~release~ have been downloaded."
  (let ((version-url (tarball-release/url release)))
    (multiple-value-bind (url filename)
        (url-location version-url)
      (let ((archive-pathname (merge-pathnames filename
                                               (source-archive-cache (release/source release)))))
        (ensure-file-fetched archive-pathname url)
        archive-pathname))))

(defmethod activate-release-globally! ((release tarball-release))
  (let* ((project (release/project release))
         (project-name (project/name project))
         (install-root (release/lib-pathname release)))
    (register-project-path-globally! project-name install-root)))

(defmethod install-release ((release tarball-release) &key activate-globally)
  (let* ((version-string (release/version release))
         (project (release/project release))
         (project-name (project/name project))
         (install-root (release/lib-pathname release)))
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
                     archive-stream (uiop:pathname-parent-directory-pathname install-root)))))
    (when activate-globally
      (activate-release-globally! release))))
