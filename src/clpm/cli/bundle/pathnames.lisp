;;;; clpm bundle pathnames
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/pathnames
    (:use #:cl
          #:clpm/cli/bundle/common
          #:clpm/cli/entry
          #:clpm/clpmfile
          #:clpm/source)
  (:import-from #:net.didierverna.clon
                #:defsynopsis
                #:make-context
                #:getopt
                #:remainder
                #:help)
  (:export #:cli-bundle-pathnames))

(in-package #:clpm/cli/bundle/pathnames)

(defparameter *synopsis*
  (defsynopsis (:make-default nil
                :postfix "command")
    (text :contents "bundle exec")
    (text :contents
          "Print out the pathnames to asd files that are part of the bundle.")
    *bundle-arguments* *common-arguments*))

(define-bundle-entry pathnames (*synopsis*)
  (let* ((clpmfile-pathname (merge-pathnames (getopt :short-name "f")
                                             (uiop:getcwd)))
         (lockfile-pathname (merge-pathnames (make-pathname :type "lock")
                                             clpmfile-pathname))
         (lockfile (read-lockfile lockfile-pathname))
         (system-files (lockfile/system-files lockfile))
         (asdf-pathnames (mapcar #'system-file/absolute-asd-pathname system-files))
         (missing-pathnames (remove-if #'probe-file asdf-pathnames)))
    (if missing-pathnames
        (progn
          ;; We got here, there is some .asd file not present. Tell the user!
          (format *error-output*
                  "The following system files are missing! Please run `clpm bundle install` and try again!~%~A"
                  missing-pathnames)
          nil)
        (progn
          (format *standard-output* "~{~A~%~}" asdf-pathnames)
          t))))
