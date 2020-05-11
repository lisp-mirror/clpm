;;;; Macros and functions to define CLI entrypoints
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/cli/interface-defs
    (:use #:cl
          #:alexandria
          #:clpm/cli/common-args
          #:clpm/config
          #:clpm/context
          #:clpm/log
          #:clpm/version)
  (:import-from #:adopt)
  (:export #:*commands*
           #:*uis*
           #:define-cli-command
           #:define-cli-command-folder
           #:dispatch-command
           #:make-diff-validate-fun))

(in-package #:clpm/cli/interface-defs)

(setup-logger)

(defvar *commands* (make-hash-table :test 'equal)
  "Maps paths to a list of two elements. The first element is an Adopt UI to be
used for that command/folder and the second is a function to be called for that
command or folder.

Folder paths start with :DEFAULT.

Folder functions must take four arguments, a thunk that, when called, executes
the remaining functions; the most specific UI found, the remaining arguments
from the command line, and a hash table created by Adopt when parsing the
options.

Command functions must take two arguments: the remaining arguments from the
command line, and a hash table created by Adopt when parsing the options.")

(defvar *uis* (make-hash-table :test 'equal)
  "Maps paths to commands to their UI.")

(defmacro define-cli-command-folder ((path default-ui)
                                     &body args-and-body)
  `(setf (gethash (list* :default ',(reverse path)) *commands*)
         (list ,default-ui
               ,(if args-and-body
                    `(lambda ,@args-and-body)
                    '(lambda (thunk ui args options)
                      (declare (ignore ui args options))
                      (funcall thunk))))))

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
       (defun ,function-name ,args
         ,@body)
       (setf (gethash ',(reverse path) *commands*) (list ,ui ',function-name))
       (setf (gethash ',(ensure-list path) *uis*) ,ui))))

(defun available-subcommands (path)
  (let (out)
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (and (not (eql :default (first k)))
                          (length= (1+ (length path)) k)
                          (ends-with-subseq path k :test #'equal))
                 (push (first k) out)))
             *commands*)
    (sort out #'string<)))

(defun parse-options-ignoring-unrecognized (ui)
  (handler-bind ((adopt:unrecognized-option #'adopt:discard-option))
    (adopt:parse-options ui)))

(defun dispatch-command (path-so-far &optional (depth 0) thunks)
  ;; Find the best UI to use so far...
  (let ((ui-and-thunk (gethash (list* :default path-so-far) *commands*)))
    (if ui-and-thunk
        ;; If UI-AND-THUNK exists, that means the PATH-SO-FAR names a folder. We
        ;; parse ignoring errors, figure out the next segment, and recurse.
        (destructuring-bind (ui thunk) ui-and-thunk
          (multiple-value-bind (args options)
              (parse-options-ignoring-unrecognized ui)
            ;; Pop the positional args we've already used to determine the
            ;; folder.
            (declare (ignore options))
            (dotimes (j depth) (pop args))
            (dispatch-command (list* (first args) path-so-far) (1+ depth) (list* thunk thunks))))
        ;; If UI-AND-THUNK does not exist, that means PATH-SO-FAR is either a
        ;; complete path or a typo.
        (let ((ui-and-thunk (gethash path-so-far *commands*)))
          (if ui-and-thunk
              ;; Not a typo! Can run the command!  We can get the actual UI,
              ;; parse it without ignoring errors, and call the appropriate
              ;; function.
              (destructuring-bind (ui thunk) ui-and-thunk
                (multiple-value-bind (args options)
                    (adopt:parse-options ui)
                  ;; Pop the positional args we've already used to determine the
                  ;; command.
                  (dotimes (j depth) (pop args))
                  (let ((thunks (reverse (list* thunk thunks))))
                    (labels ((call-thunks ()
                               (let ((thunk (pop thunks)))
                                 (if thunks
                                     (funcall thunk #'call-thunks ui args options)
                                     (funcall thunk args options)))))
                      (call-thunks)))))
              ;; Typo or a folder name was typed directly... Either way, pop the
              ;; last element of the path and print the help for the folder's
              ;; UI.
              (let ((ui-and-thunk (gethash (list* :default (rest path-so-far)) *commands*)))
                (adopt:print-help (first ui-and-thunk))
                (terpri)
                (format *standard-output* "Available subcommands: ~{~A~^ ~}~%"
                        (available-subcommands (rest path-so-far)))
                (adopt:exit 1)))))))

(defun make-diff-validate-fun (&key yesp (stream *standard-output*))
  (lambda (diff)
    (block nil
      (unless (context-diff-has-diff-p diff)
        (return t))
      (print-context-diff diff stream)
      (or yesp (y-or-n-p "Proceed?")))))
