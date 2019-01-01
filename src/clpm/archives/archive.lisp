(uiop:define-package #:clpm/archives/archive
    (:use #:cl
          #:alexandria
          #:clpm/archives/defs)
  (:import-from #:babel)
  (:import-from #:chipz
                #:make-decompressing-stream
                #:gzip)
  (:import-from #:flexi-streams
                #:make-flexi-stream
                #:make-in-memory-input-stream)
  (:import-from #:archive
                #:close-archive
                #:do-archive-entries
                #:entry-directory-p
                #:entry-regular-file-p
                #:entry-stream
                #:entry-symbolic-link-p
                #:mtime
                #:name
                #:open-archive)
  (:import-from #:sb-posix
                #:symlink
                #:utimes))

(in-package #:clpm/archives/archive)

(defun strip-components (pathname number)
  (assert (uiop:relative-pathname-p pathname))
  (let* ((dirs (rest (pathname-directory pathname)))
         (filename (file-namestring pathname))
         (components (append dirs (list filename))))
    (when (> (length components) number)
      (uiop:resolve-relative-location (subseq components number)))))

(defun update-utimes (pathname time)
  (declare (ignorable pathname time))
  #-:os-windows
  (utimes pathname time time))

(defmethod unarchive-tar (client archive-stream destination-pathname)
  (let (archive)
    (unwind-protect
         (let ((*default-pathname-defaults* (pathname destination-pathname)))
           (setf archive (open-archive 'archive:tar-archive (make-flexi-stream archive-stream)))
           (archive::extract-files-from-archive archive
                                                (lambda (name)
                                                  (not (null (pathname-directory name))))))
      (close-archive archive))))
