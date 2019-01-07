;;;; Basic installation support
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/install
    (:use #:cl
          #:alexandria
          #:clpm/archives
          #:clpm/cache
          #:clpm/data
          #:clpm/http-client
          #:clpm/log
          #:clpm/source
          #:clpm/sources/vcs)
  (:import-from #:puri)
  (:export #:install-release))

(in-package #:clpm/install)

(setup-logger)

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

(defun fetch-release (release)
  "Ensure that the files needed to install ~release~ have been downloaded."
  (let ((version-url (first (tarball-release/urls release))))
    (multiple-value-bind (url filename)
        (url-location version-url)
      (let ((archive-pathname (merge-pathnames filename
                                               (source-archive-cache (release/source release)))))
        (ensure-file-fetched archive-pathname url)
        archive-pathname))))

(defgeneric activate-release-globally! (release)
  (:documentation
   "Add ~release~ to the global list of installed and activated releases."))

(defmethod activate-release-globally! ((release tarball-release))
  (let* ((source (release/source release))
         (project (release/project release))
         (project-name (project/name project))
         (version-string (release/version release))
         (global.sexp (source-context-pathname source "global"))
         (global.sexp-form (when (probe-file global.sexp)
                             (uiop:safe-read-file-form global.sexp)))

         (source-registry-cache-pathname (source-source-registry-cache-pathname source))
         (source-registry-cache (when (probe-file source-registry-cache-pathname)
                                  (rest (uiop:safe-read-file-form source-registry-cache-pathname))))

         (system-releases (release/system-releases release))
         (asd-pathnames (mapcar #'system-release/absolute-asd-pathname system-releases))
         (asd-enough-namestrings (mapcar (rcurry #'enough-namestring source-registry-cache-pathname)
                                         asd-pathnames))

         (previous-version (assoc-value global.sexp-form project-name :test #'equal)))

    (when previous-version
      ;; Need to remove the asd files from the previous version from the source
      ;; registry cache.
      (let* ((old-release (project/release project previous-version))
             (old-system-releases (release/system-releases old-release))
             (old-asd-pathnames (mapcar #'system-release/absolute-asd-pathname old-system-releases))
             (old-asd-enough-namestrings (mapcar (rcurry #'enough-namestring source-registry-cache-pathname)
                                                 old-asd-pathnames)))

        (setf source-registry-cache (set-difference source-registry-cache old-asd-enough-namestrings
                                                    :test #'string-equal))))

    ;; Add the new asd files to the source registry cache.
    (setf source-registry-cache (sort (append source-registry-cache
                                              asd-enough-namestrings)
                                      #'string<))

    (setf (assoc-value global.sexp-form project-name :test #'equal)
          version-string)

    ;; Write the global context database.
    (ensure-directories-exist global.sexp)
    (with-open-file (s global.sexp :direction :output
                                   :if-exists :supersede)
      (uiop:with-safe-io-syntax ()
        (write global.sexp-form :stream s)))

    ;; Write the source-registry-cache
    (ensure-directories-exist source-registry-cache-pathname)
    (with-open-file (s source-registry-cache-pathname :direction :output
                                                      :if-exists :supersede)
      (uiop:with-safe-io-syntax ()
        (write (list* :source-registry-cache source-registry-cache)
               :stream s)))))

(defgeneric install-release (release &key activate-globally)
  (:documentation "Install a ~release~ and optinally activate it globally."))

(defmethod install-release ((release tarball-release) &key activate-globally)
  (let* ((version-string (release/version release))
         (project (release/project release))
         (project-name (project/name project))
         (install-root (release/lib-pathname release)))
    (log:info "Installing ~A version ~A to ~A" project-name version-string
              install-root)
    (unless (uiop:probe-file* install-root)
      (let ((archive-pathname (fetch-release release)))
        (log:debug "Package distfiles located at ~A" archive-pathname)
        (with-open-file (archive-stream archive-pathname
                                        :direction :input
                                        :element-type '(unsigned-byte 8))
          (unarchive 'gzipped-tar-archive
                     archive-stream (uiop:pathname-parent-directory-pathname install-root)))))
    (when activate-globally
      (activate-release-globally! release))))

(defmethod install-release ((release git-release) &key activate-globally)
  (declare (ignore activate-globally))
  (ensure-git-release-installed! release))
