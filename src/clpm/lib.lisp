(uiop:define-package #:clpm/lib
    (:use #:cl
          #:alexandria
          #:clpm/data
          #:clpm/sources/defs)
  (:import-from #:uiop
                #:enough-pathname))

(in-package #:clpm/lib)

(defun discover-asd-files (system-names root)
  (let ((out nil))
    (flet ((collect (it)
             (when (member (pathname-name it) system-names
                           :test #'string-equal
                           :key #'system/name)
               (push it out))))
      (asdf/source-registry:collect-sub*directories-asd-files root
                                                              :collect #'collect
                                                              :recurse-beyond-asds t))
    out))

(defun discover-release-asd-files (release)
  (let ((root-pathname (release-lib-pathname release)))
    (mapcar #'namestring
            (mapcar (rcurry #'enough-pathname root-pathname)
                    (discover-asd-files (release/systems release) root-pathname)))))
