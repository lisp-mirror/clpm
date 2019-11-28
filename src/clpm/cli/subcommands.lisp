;;;; CLI subcommands
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/subcommands
    (:use #:cl
          #:alexandria
          #:clpm/cli/common-args
          #:clpm/log
          #:clpm/version)
  (:import-from #:adopt)
  (:export #:*commands*
           #:define-cli-command
           #:define-cli-command-folder
           #:dispatch-subcommand))

(in-package #:clpm/cli/subcommands)

(setup-logger)

(defvar *commands* (make-hash-table :test 'equal))

(defun ensure-ht-path (ht path &optional default-ui)
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
          (when default-ui
            (setf (gethash :ui next) default-ui))
          next))))

(defmacro define-cli-command-folder (path default-ui)
  `(progn
     (ensure-ht-path *commands* ',path ,default-ui)))

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
           (when (gethash :help ,(second args))
             (adopt:print-help-and-exit ,ui))
           ,@body))
       (setf (gethash ,(last-elt path) (ensure-ht-path *commands*
                                                       ',(butlast path)))
             ',function-name))))

(defun available-subcommands (ht)
  (let ((out nil))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (stringp k)
                 (push k out)))
             ht)
    (sort out #'string<)))

(defun dispatch-subcommand (working-ht arguments options ui)
  (let* ((subcommand-name (first arguments))
         (dir-or-fun (gethash subcommand-name working-ht)))
    (cond
      ((and dir-or-fun
            (or (functionp dir-or-fun)
                (symbolp dir-or-fun)))
       (funcall dir-or-fun))
      ((hash-table-p dir-or-fun)
       (let ((default-ui (gethash :ui dir-or-fun)))
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
