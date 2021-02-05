;;;; ASDF operations for deploying CLPM
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/clpm-program-op
    (:use #:cl
          #:clpm-cli/man)
  (:export #:clpm-core-op
           #:clpm-program-op))

(in-package #:clpm-cli/clpm-program-op)

(defclass clpm-core-op (asdf:image-op)
  ())

(defclass clpm-program-op (asdf:program-op)
  ())

(defclass clpm-manual-op (asdf:selfward-operation)
  ((asdf:selfward-operation
    :initform (list 'asdf:prepare-op))))

(defclass clpm-client-op (asdf:selfward-operation)
  ((asdf:selfward-operation
    :initform (list 'asdf:prepare-op))))

(defun os-arch-tuple ()
  (concatenate 'string
               (string-downcase (uiop:operating-system))
               "-"
               #+x86-64 "amd64"
               #+x86 "x86"
               #+arm "arm"
               #+arm64 "arm64"))

(defun output-dir (c)
  (asdf:system-relative-pathname c (concatenate 'string
                                                "build/"
                                                (os-arch-tuple)
                                                "/")))

(defun copy-directory-tree-ignoring-fasls-and-.git (source target)
  (cond
    ((uiop:directory-pathname-p source)
     (let* ((directory-name (car (last (pathname-directory source))))
            (new-target (merge-pathnames (make-pathname :directory (list :relative directory-name))
                                         target)))
       (unless (equal directory-name ".git")
         (dolist (new-source (uiop:directory* (merge-pathnames uiop:*wild-file* source)))
           (copy-directory-tree-ignoring-fasls-and-.git new-source new-target)))))
    ((not (equal (pathname-type source) "fasl"))
     (ensure-directories-exist target)
     (uiop:copy-file source (merge-pathnames (make-pathname :name (pathname-name source)
                                                            :type (pathname-type source))
                                             target)))))

(defmethod asdf:perform ((o clpm-client-op) (c (eql (asdf:find-system "clpm-cli"))))
  (format t "HERE: ~S ~S~%" (merge-pathnames "client/" (asdf:system-source-directory :clpm-client))
          (merge-pathnames "client/" (output-dir c)))
  (copy-directory-tree-ignoring-fasls-and-.git
   (asdf:system-source-directory :clpm-client)
   (merge-pathnames "client/" (output-dir c))))

(defmethod asdf:operation-done-p ((o clpm-client-op) (c (eql (asdf:find-system "clpm-cli"))))
  nil)

(defmethod asdf:output-files ((o clpm-manual-op) (c (eql (asdf:find-system "clpm-cli"))))
  (let* ((output-dir (output-dir c))
         (man-pages (manual-file-names)))
    (values (mapcar (lambda (x) (merge-pathnames
                                 x
                                 (merge-pathnames
                                  "man/man1/"
                                  output-dir)))
                    man-pages)
            t)))

(defmethod asdf:perform ((o clpm-manual-op) (c (eql (asdf:find-system "clpm-cli"))))
  (let* ((output-files (asdf:output-files o c))
         (man-dir-pathname (uiop:pathname-directory-pathname (first output-files))))
    (output-manual man-dir-pathname)))


(defmethod asdf:component-depends-on ((o clpm-program-op) (c (eql (asdf:find-system "clpm-cli"))))
  `((asdf:prepare-op ,c)
    (clpm-manual-op ,c)
    ;(clpm-client-op ,c)
    ,@(call-next-method)))

(defmethod asdf:output-files ((o clpm-program-op) (c (eql (asdf:find-system "clpm-cli"))))
  (let* ((output-dir (output-dir c))
         (bin (merge-pathnames (if (uiop:os-windows-p)
                                   "clpm.exe"
                                   "clpm")
                               output-dir)))
    (values (list bin)
            t)))

(defmethod asdf:operation-done-p ((o clpm-core-op) (c (eql (asdf:find-system "clpm-cli"))))
  nil)

(defmethod asdf:output-files ((o clpm-core-op) (c (eql (asdf:find-system "clpm-cli"))))
  (let* ((output-dir (output-dir c))
         (bin (merge-pathnames "clpm.core"
                               output-dir)))
    (values (list bin)
            t)))
