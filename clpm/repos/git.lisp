;;;; Git Repositories
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/repos/git
    (:use #:cl
          #:alexandria
          #:clpm/archives
          #:clpm/cache
          #:clpm/http-client
          #:clpm/log
          #:clpm/repos/defs
          #:clpm/utils
          #:split-sequence)
  (:import-from #:puri)
  (:export #:git-repo
           #:git-repo-credentials
           #:git-repo-local-dir
           #:git-repo-uri-string))

(in-package #:clpm/repos/git)

(setup-logger)

(defclass git-repo ()
  ())

(defgeneric git-repo-local-dir (repo))

(defgeneric git-repo-uri-string (repo))

(defgeneric git-repo-credentials (repo))


;; * URI parsing

(defclass git-uri (uri)
  ((user-name
    :initform nil
    :accessor git-uri/user-name)
   (real-host
    :initform nil
    :accessor git-uri/real-host))
  (:documentation "A URI class for git uris. holds on to the real host and username."))

(defmethod initialize-instance :after ((uri git-uri) &rest initargs
                                       &key host)
  (declare (ignore initargs))
  (let ((@-pos (position #\@ host)))
    (if @-pos
        (setf (git-uri/real-host uri) (subseq host (1+ @-pos))
              (git-uri/user-name uri) (subseq host 0 @-pos))
        (setf (git-uri/real-host uri) host))))

(defun guess-git-protocol (uri-string)
  (let* ((colon-pos (position #\: uri-string)))
    (if colon-pos
        ;; Either the protocol is directly specified *or* the user is using
        ;; SCP-like syntax.
        (let ((pre-colon (subseq uri-string 0 colon-pos)))
          (switch (pre-colon :test #'string-equal)
            ("http"
             :http)
            ("https"
             :https)
            ("ssh"
             :ssh)
            ("file"
             :file)
            (t
             :scp)))
        ;; Implicit file protocol
        :file-implicit)))

(defun convert-scp-to-ssh (uri-string)
  (concatenate 'string "ssh://"
               (substitute #\/ #\: uri-string :count 1)))

(defun parse-git-uri (uri-string)
  ;; First, guess at the protocol:
  (let ((protocol (guess-git-protocol uri-string)))
    (ecase protocol
      (:file-implicit
       (pathname uri-string))
      (:scp
       (puri:parse-uri (convert-scp-to-ssh uri-string)
                       :class 'git-uri))
      ((:http :https :ssh :file)
       (puri:parse-uri uri-string
                       :class 'git-uri)))))


;; * Git utils

(defun authenticated-git-command-list (credential-alist)
  "Given an alist of credentials (currently recognizes keys ~:username~ and
~:password~), returns two values: a list that can be used as a prefix when
assembling a command to run and an alist of environment variables to set when
running the command. This prefix starts with ~\"git\"~ and contains settings
to appropriately set the username and password.

If a password needs to be set, it attempts to pass the password through an
environment variable, so as to prevent the password from showing up on the
process list."
  (let ((prefix (list "git"))
        (env nil)
        (username (assoc-value credential-alist :username))
        (password (assoc-value credential-alist :password)))

    (when username
      (push "-c" prefix)
      (push (concatenate 'string "credential.username=" username) prefix))

    (when password
      (push (cons "CLPM_GIT_PASS_HELPER_PASS" password) env)
      (push "-c" prefix)
      (push "credential.helper=!f() { while read line; do sleep 0; done; echo \"password=${CLPM_GIT_PASS_HELPER_PASS}\"; }; f"
            prefix))

    (values (nreverse prefix)
            env)))

(defun call-with-git-dir (repo thunk)
  "If repo is set, sets the current directory to the repo's local directory and
then calls the thunk."
  (uiop:with-current-directory ((when repo (git-repo-local-dir repo)))
    (funcall thunk)))

(defmacro with-git-dir ((&optional repo) &body body)
  `(call-with-git-dir ,repo (lambda () ,@body)))

(defun git-archive (ref &key repo)
  "Calls ~git archive~ with the provided ref and returns the output stream. Uses
~uiop:launch-program~ to avoid needing for the process to complete before
returning. The stream must be CLOSEd when finished to clean up the process info."
  (with-git-dir (repo)
    (let ((proc (uiop:launch-program `("git" "archive" ,ref)
                                     :output :stream)))
      (make-instance 'process-stream
                     :process-info proc
                     :true-stream (uiop:process-info-output proc)))))

(defun git-cat-file (commit &key ignore-error-status repo)
  "Calls ~git cat-file~ with the provided commit and returns the result,
optionally changing into the local dir for ~repo~."
  (with-git-dir (repo)
    (uiop:run-program `("git" "cat-file" "-e" ,(uiop:strcat commit "^{commit}"))
                      :output '(:string :stripped t)
                      :ignore-error-status ignore-error-status)))

(defun git-rev-parse (rev &key repo abbrev-ref ignore-error-status)
  "Calls ~git rev-parse~ with the provided revision and returns the result,
optionally changing into the local dir for ~repo~."
  (with-git-dir (repo)
    (uiop:run-program `("git" "rev-parse" ,@(when abbrev-ref '("--abbrev-ref")) ,rev)
                      :output '(:string :stripped t)
                      :ignore-error-status ignore-error-status)))

(defun fetch-repo! (repo)
  "Given a repo, fetch all of its tags and branches locally."
  (let* ((local-dir (git-repo-local-dir repo))
         (uri-string (git-repo-uri-string repo))
         (credentials (git-repo-credentials repo)))
    (log:debug "Fetching ~A" local-dir)
    (multiple-value-bind (prefix env)
        (authenticated-git-command-list credentials)
      (uiop:with-current-directory (local-dir)
        (multiple-value-bind (output error-output exit-code)
            (apply
             #'uiop:run-program
             `(,@prefix "fetch" "--tags" "--prune" ,uri-string "+refs/*:refs/*")
             :input :interactive
             :output '(:string :stripped t)
             :error-output '(:string :stripped t)
             (run-program-augment-env-args env))
          (unless (zerop exit-code)
            (format *error-output* "~&git exited with code ~S~%stdout: ~S~%stderr: ~S~%"
                    exit-code output error-output)
            (error 'retriable-error)))))))

(defmethod clone-repo! ((repo git-repo) &key pathname not-bare-p)
  "Given a repo, clone it locally."
  (let* ((local-dir (or (when pathname (uiop:ensure-directory-pathname pathname))
                        (git-repo-local-dir repo)))
         (uri-string (git-repo-uri-string repo))
         (credentials (git-repo-credentials repo)))
    (multiple-value-bind (prefix env)
        (authenticated-git-command-list credentials)
      (multiple-value-bind (output error-output exit-code)
          (apply
           #'uiop:run-program
           `(,@prefix "clone"
                      ,@(unless not-bare-p
                          '("--bare" "--mirror"))
                      "--verbose"
                      ,uri-string
                      ,(namestring local-dir))
           :input :interactive
           :output '(:string :stripped t)
           :error-output '(:string :stripped t)
           :ignore-error-status t
           (run-program-augment-env-args env))
        (unless (zerop exit-code)
          (format *error-output* "~&git exited with code ~S~%stdout: ~S~%stderr: ~S~%"
                  exit-code output error-output)
          (error 'retriable-error))))))

(defmethod ref-present-p ((repo git-repo) ref)
  (destructuring-bind (ref-type ref-name) ref
    (let ((local-dir (git-repo-local-dir repo)))
      (and (uiop:probe-file* local-dir)
           (if (eql ref-type :commit)
               (zerop (nth-value 2 (git-cat-file ref-name :repo repo :ignore-error-status t)))
               (zerop (nth-value 2 (git-rev-parse ref-name :repo repo :ignore-error-status t))))))))

(defmethod ensure-ref-present-locally! ((repo git-repo) ref)
  "Given a repo and a reference (either a commit, branch, tag, or ref), ensure that
ref is present locally, fetching or cloning the repo as necessary."
  (destructuring-bind (ref-type ref-name) ref
    (let ((local-dir (git-repo-local-dir repo)))
      (cond
        ((uiop:probe-file* local-dir)
         ;; Repo is present, need to fetch if the ref is not present or the ref
         ;; is a branch.
         (when (and (or (eql ref-type :branch)
                        (starts-with-subseq "refs/heads/" ref-name)
                        (not (ref-present-p repo ref)))
                    *fetch-repo-automatically*)
           (with-retries (:max 10 :sleep 5)
             (fetch-repo! repo))
           ;; Make sure the ref is actually present, raising an error otherwise.
           (unless (ref-present-p repo ref)
             (error "ref ~S is not present" ref))))
        (*fetch-repo-automatically*
         ;; Repo is not present at all, need to clone it.
         (with-retries (:max 10 :sleep 5)
           (clone-repo! repo)
           ;; Make sure the ref is actually present, raising an error otherwise.
           (unless (ref-present-p repo ref)
             (error "ref is not present, even after updating."))))
        (t
         ;; Repo is not present and we were told to not fetch it. Error.
         (error "Repo is not present at ~A" local-dir))))))

(defmethod repo-archive-stream ((repo git-repo) ref)
  (values
   (git-archive (second ref) :repo repo)
   'tar-archive))

(defmethod resolve-ref-to-commit ((repo git-repo) ref)
  (git-rev-parse (second ref) :repo repo))

(defmethod repo-current-branch ((repo git-repo))
  (git-rev-parse "HEAD" :repo repo :abbrev-ref t))

(defmethod repo-current-commit ((repo git-repo))
  (git-rev-parse "HEAD" :repo repo))
