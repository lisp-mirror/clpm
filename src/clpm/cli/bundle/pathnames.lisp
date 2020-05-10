;;;; clpm bundle pathnames
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/pathnames
    (:use #:cl
          #:clpm/bundle
          #:clpm/cli/bundle/common
          #:clpm/cli/common-args
          #:clpm/cli/config/common
          #:clpm/cli/subcommands
          #:clpm/clpmfile
          #:clpm/source)
  (:import-from #:adopt)
  (:export #:cli-bundle-pathnames))

(in-package #:clpm/cli/bundle/pathnames)

(defparameter *bundle-pathnames-ui*
  (adopt:make-interface
   :name "clpm bundle pathnames"
   :summary "Common Lisp Package Manager Bundle Pathnames"
   :usage "bundle pathnames [options]"
   :help "Print out the pathnames to asd files that are part of the bundle."
   :contents (list *group-common*
                   *group-bundle*)))

(define-cli-command (("bundle" "pathnames") *bundle-pathnames-ui*) (args options)
  (let* ((clpmfile-pathname (bundle-clpmfile-pathname))
         (lockfile-pathname (merge-pathnames (make-pathname :type "lock")
                                             clpmfile-pathname))
         (lockfile (read-lockfile lockfile-pathname))
         (system-files (lockfile/system-files lockfile))
         (asdf-pathnames (mapcar #'system-file-absolute-asd-pathname system-files))
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
