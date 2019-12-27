;;;; Common bundle CLI functions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/bundle/common
    (:use #:cl
          #:alexandria
          #:clpm/cli/common-args
          #:clpm/cli/subcommands
          #:clpm/config
          #:clpm/utils)
  (:import-from #:adopt)
  (:import-from #:cl-ppcre)
  (:export #:*group-bundle*))

(in-package #:clpm/cli/bundle/common)

(defparameter *option-file*
  (adopt:make-option
   :bundle-file
   :short #\f
   :parameter "FILE"
   :initial-value "clpmfile"
   :help "The path to the clpmfile"
   :reduce #'adopt:last))

(defparameter *group-bundle*
  (adopt:make-group
   :bundle
   :title "Bundle options"
   :help "Common options for bundle commands"
   :options (list *option-file*)))

(defparameter *default-ui*
  (adopt:make-interface
   :name "clpm bundle"
   :summary "Common Lisp Package Manager Bundle"
   :usage "bundle [options] subcommand"
   :help "Bundle commands"
   :contents (list *group-common*
                   *group-bundle*)))

(defun merge-git-auth-config ()
  (let* ((env (posix-environment-alist))
         (git-auth-vars (remove-if-not (curry #'starts-with-subseq "CLPM_BUNDLE_GIT_AUTH_")
                                       env :key #'car))
         (ht (make-hash-table :test 'equal
                              :size (length git-auth-vars))))
    (loop
      :for (name . val) :in git-auth-vars
      :for key := (string-downcase (cl-ppcre:regex-replace-all "__" (subseq name 21) "."))
      :for split-value := (cl-ppcre:split ":" val :limit 2)
      :for new-ht := (make-hash-table :test 'equal)
      :do
         (destructuring-bind (username password) split-value
           (setf (gethash :username new-ht) username
                 (gethash :password new-ht) password)
           (setf (gethash key ht) new-ht)))
    (merge-ht-into-config! (alist-hash-table
                            `((:git . ,(alist-hash-table
                                        `((:remotes . ,ht))
                                        :test 'equal)))
                            :test 'equal))))

(define-cli-command-folder
    (("bundle") *default-ui*)
    (args options)
    (declare (ignore args))
    (let* ((clpmfile-path (merge-pathnames (gethash :bundle-file options)
                                           (uiop:getcwd)))
           (local-config (merge-pathnames ".clpm/bundle.conf"
                                          (uiop:pathname-directory-pathname clpmfile-path))))
      (when (probe-file local-config)
        (merge-file-into-config! local-config))
      (merge-git-auth-config)))
