;;;; CLPM Manual
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/man
    (:use #:cl
          #:alexandria
          #:clpm-cli/interface-defs)
  (:import-from #:adopt)
  (:export #:manual-file-names
           #:output-manual))

(in-package #:clpm-cli/man)

(defun manual-file-names ()
  (let ((out))
    (maphash
     (lambda (path ui)
       (declare (ignore ui))
       (let ((file-name (if (listp path)
                            (format nil "clpm-~{~A~^-~}.1" path)
                            path)))
         (push file-name out)))
     *uis*)
    out))

(defun output-manual (dir)
  (ensure-directories-exist dir)
  (maphash
   (lambda (path ui)
     (let ((file-name (if (listp path)
                          (format nil "clpm-~{~A~^-~}.1" path)
                          path)))
       (with-open-file (s (merge-pathnames file-name dir)
                        :direction :output
                        :if-exists :supersede)
         (adopt:print-manual ui :stream s))))
   *uis*))
