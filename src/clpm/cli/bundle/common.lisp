(uiop:define-package #:clpm/cli/bundle/common
    (:use #:cl
          #:alexandria
          #:clpm/cli/entry
          #:clpm/config)
  (:import-from #:cl-ppcre)
  (:import-from #:net.didierverna.clon
                #:defsynopsis
                #:defgroup
                #:make-context
                #:getopt
                #:remainder
                #:help)
  (:export #:*bundle-arguments*
           #:define-bundle-entry))

(in-package #:clpm/cli/bundle/common)

(defparameter *bundle-arguments*
  (defgroup (:header "Bundle Common Options")
    (path :short-name "f"
          :type :file
          :default-value #p"clpmfile"
          :description "Path to the clpmfile")))

(defvar *bundle-commands* nil)

(defun register-bundle-command (name function)
  (setf (assoc-value *bundle-commands* name :test 'equal) function))

(defun dispatch-bundle-command (args)
  (let* ((name (find-if (lambda (x)
                          (not (eql #\- (aref x 0))))
                        args))
         (args (remove name args)))
    (let ((function (assoc-value *bundle-commands* name :test 'equal)))
      (cond
        (function
         (funcall function args))
        (t
         (print-bundle-help)
         (uiop:quit))))))

(defun print-bundle-help ()
  (format *standard-output* "CLPM - Lisp Package Manager~%")
  (when net.didierverna.clon:*context*
    (help))
  (format *standard-output*
          "available bundle commands: ~{~A~^ ~}~%"
	      (mapcar #'car *bundle-commands*)))

(defun cli-bundle (args)
  (dispatch-bundle-command args)
  ;; ;; Before making the context, figure out which subcommand to run!
  ;; (make-context :cmdline (list* (concatenate 'string (first (uiop:raw-command-line-arguments))
  ;;                                            " bundle")
  ;;                               args)
  ;;               :synopsis *synopsis*)
  ;; (process-common-arguments)


  ;; (let ((packages-to-install (remainder))
  ;;       (sources (load-sources)))
  ;;   (dolist (clpm-package-name packages-to-install)
  ;;     (install-requirement sources
  ;;                          (make-instance 'requirement :clpm-package-name clpm-package-name)))
  ;;   t)
  )

(register-command "bundle" 'cli-bundle)

(defun merge-git-auth-config ()
  (let* ((env (mapcar (lambda (x)
                        (let* ((split (uiop:split-string (reverse x)
                                                         :max 2 :separator '(#\=)))
                               (name (reverse (second split)))
                               (value (reverse (first split))))
                          (cons name value)))
                      (sb-ext:posix-environ)))
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
           (setf (gethash "username" new-ht) username
                 (gethash "password" new-ht) password)
           (setf (gethash key ht) new-ht)))
    (merge-ht-into-config (alist-hash-table
                           `(("git" . ,(alist-hash-table
                                        `(("remotes" . ,ht))
                                        :test 'equal)))
                           :test 'equal))))

(defmacro define-bundle-entry (name (synopsis) &body body)
  (let ((fun-name (intern
                   (concatenate 'string
                                (uiop:standard-case-symbol-name :bundle-)
                                (uiop:standard-case-symbol-name name))))
        (bundle-name (string-downcase (symbol-name name)))
        (args (gensym)))
    `(progn
       (defun ,fun-name (,args)
         (make-context :cmdline (list* (concatenate 'string (first (uiop:raw-command-line-arguments))
                                                    " bundle "
                                                    ,bundle-name)
                                       ,args)
                       :synopsis ,synopsis)
         (process-common-arguments)
         (let ((local-config (merge-pathnames ".clpm/bundle.conf"
                                              (uiop:getcwd))))
           (merge-git-auth-config)
           (when (probe-file local-config)
             (merge-file-into-config local-config)))
         (unless (progn ,@body)
           (uiop:quit 1)))
       (register-bundle-command ,bundle-name ',fun-name))))
