;;;; CLI subcommands
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/subcommands
    (:use #:cl
          #:alexandria
          #:clpm/cli/common-args
          #:clpm/config
          #:clpm/log
          #:clpm/version)
  (:import-from #:adopt)
  (:export #:*commands*
           #:*uis*
           #:define-cli-command
           #:define-cli-command-folder
           #:dispatch-subcommand))

(in-package #:clpm/cli/subcommands)

(setup-logger)

(defvar *commands* (make-hash-table :test 'equal))
(defvar *uis* (make-hash-table :test 'equal))

(defun ensure-ht-path (ht path &optional default-ui thunk)
  (let ((next (if path
                  (ensure-gethash (pop path) ht
                                  (make-hash-table :test 'equal))
                  ht)))
    (if path
        (progn
          (unless (hash-table-p next)
            (error "Expected another hash table!"))
          (ensure-ht-path next path default-ui))
        (progn
          (when (or default-ui thunk)
            (setf (gethash :ui next) (list default-ui thunk)))
          next))))

(defmacro define-cli-command-folder ((path default-ui) &rest args-and-body)
  `(progn
     (ensure-ht-path *commands* ',path ,default-ui
                     ,(when args-and-body
                        `(lambda ,(first args-and-body)
                           ,@(rest args-and-body))))))

(defmacro define-cli-command ((path ui) args &body body)
  (let* ((function-name-string (apply
                                #'concatenate 'string
                                (uiop:standard-case-symbol-name :cli)
                                (mapcan
                                 (lambda (x)
                                   (list "/"
                                         (uiop:standard-case-symbol-name x)))
                                 path)))
         (function-name (intern function-name-string)))
    `(progn
       (defun ,function-name ()
         (multiple-value-bind ,args (adopt:parse-options ,ui)
           (declare (ignorable ,@args))
           ;; Pop the path from args
           (dotimes (j ,(length path))
             (pop ,(first args)))
           (when (gethash :help ,(second args))
             (adopt:print-help-and-exit ,ui))
           (config-add-cli-source! ,(second args))
           (set-log-level)
           ,@body))
       (setf (gethash ,(last-elt path) (ensure-ht-path *commands*
                                                       ',(butlast path)))
             ',function-name)
       (setf (gethash ',(ensure-list path) *uis*) ,ui))))

(defun available-subcommands (ht)
  (let ((out nil))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (stringp k)
                 (push k out)))
             ht)
    (sort out #'string<)))

(defun parse-options-ignoring-unrecognized (ui)
  (handler-bind ((adopt:unrecognized-option #'adopt:discard-option))
    (adopt:parse-options ui)))

(defun dispatch-subcommand (working-ht arguments options ui)
  (let* ((subcommand-name (first arguments))
         (dir-or-fun (gethash subcommand-name working-ht)))

    (cond
      ((and dir-or-fun
            (or (functionp dir-or-fun)
                (symbolp dir-or-fun)))
       (funcall dir-or-fun))
      ((hash-table-p dir-or-fun)
       (destructuring-bind (default-ui thunk)
           (gethash :ui dir-or-fun)
         (when thunk
           (multiple-value-bind (sub-args sub-options)
               (parse-options-ignoring-unrecognized default-ui)
             (funcall thunk sub-args sub-options)))
         (dispatch-subcommand dir-or-fun (rest arguments) options default-ui)))
      ((gethash :help options)
       (adopt:print-help ui)
       (terpri)
       (format *standard-output* "Available subcommands: ~{~A~^ ~}~%"
               (available-subcommands working-ht))
       (adopt:exit 0))
      (t
       (adopt:print-help ui)
       (terpri)
       (format *standard-output* "Available subcommands: ~{~A~^ ~}~%"
               (available-subcommands working-ht))
       (adopt:exit 1)))))

(defun set-log-level ()
  (log:config '(clpm) (config-value :log :level)
              :this
              :stream *error-output*
              :immediate-flush :own
              :pattern "%;<;;>;5p [%g{}{}{:downcase}] - %<{pretty}%m%>%n")
  (log:debug "Log level set to ~S" (config-value :log :level)))
