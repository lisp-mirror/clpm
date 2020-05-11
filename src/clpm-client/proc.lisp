;;;; Interacting with the CLPM executable.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)


;; * Conditions

(define-condition clpm-error (error)
  ((error-output
    :initarg :error-output
    :reader clpm-error-error-output)
   (wrapped-condition
    :initarg :wrapped-condition
    :reader clpm-error-wrapped-condition))
  (:report (lambda (c s)
             (format s "Error interacting with CLPM! Error output:~%~A~%~%Original condition:~%~A"
                     (clpm-error-error-output c)
                     (clpm-error-wrapped-condition c)))))


;; * Dribbling

(defvar *clpm-dribble* nil
  "A pathname or stream where input and output to/from CLPM is copied. If a
pathname, it is appended to.")

(defvar *clpm-error-dribble* nil
  "A pathname or stream where CLPM's error output is sent. If a pathname, it is
appended to.")

(defvar *clpm-dribble-input-prefix* "CLPM-IN>  "
  "The prefix for lines to *CLPM-DRIBBLE* that are sent by this process.")

(defvar *clpm-dribble-output-prefix* "CLPM-OUT> "
  "The prefix for lines to *CLPM-DRIBBLE* that are read by this process.")

(defun call-with-dribble-stream (thunk designator)
  (cond
    ((streamp designator)
     ;; If this is already a stream, someone else is doing the maintenance of
     ;; the stream.
     (funcall thunk designator))
    (designator
     ;; Otherwise, we need to open it and close it.
     (with-open-file (s designator :direction :output :if-exists :append)
       (funcall thunk s)))))

(defmacro with-dribble-stream ((stream designator) &body body)
  `(call-with-dribble-stream (lambda (,stream) ,@body) ,designator))


;; * Executable path

(defvar *clpm-executable* nil
  "A list of command and arguments needed to run CLPM.
If the clpm executable is not on your PATH, change this to be an absolute path
to the executable.")

(defun clpm-executable ()
  "If *CLPM-EXECUTABLE* is set, return that. Otherwise use clpm from the path
or the same CLPM used to execute the bundle."
  (or *clpm-executable*
      (if (inside-bundle-exec-p)
          (bundle-command)
          (list "clpm-live"))))

(defun ensure-list (arg)
  (if (listp arg)
      arg
      (list arg)))


;; * Process

(defclass clpm-proc ()
  ((proc-info
    :initarg :proc-info
    :reader clpm-proc-info)
   (error-string-output-stream
    :initarg :error-string-output-stream
    :reader clpm-proc-error-string-output-stream)
   (error-dribble
    :initarg :error-dribble
    :reader clpm-proc-error-dribble))
  (:documentation
   "A CLPM process."))

(defun open-dribble-stream (designator)
  (cond
    ((streamp designator)
     designator)
    ((or (pathnamep designator) (stringp designator))
     (open designator :direction :output :if-exists :append))))

(defun make-clpm-proc ()
  "Makes a child CLPM process with the CLPM REPL running. The returned CLPM-PROC
*must* be stopped in order to free up resources belonging to the process."
  (let* ((error-string-output-stream (make-string-output-stream))
         (error-dribble-stream (open-dribble-stream *clpm-error-dribble*))
         (error-output-stream (if error-dribble-stream
                                  (make-broadcast-stream error-string-output-stream
                                                         error-dribble-stream)
                                  error-string-output-stream))
         (proc-info (uiop:launch-program
                     (append (ensure-list (clpm-executable))
                             (list "client" "repl"))
                     :input :stream
                     :output :stream
                     :error-output error-output-stream)))
    (make-instance 'clpm-proc
                   :proc-info proc-info
                   :error-dribble (unless (streamp *clpm-error-dribble*) error-dribble-stream)
                   :error-string-output-stream error-string-output-stream)))

(defun clpm-proc-print (proc command &key read-eval-p)
  "Print COMMAND to PROC. If READ-EVAL-P is non-NIL, the command is prefixed
with #."
  (let ((stream (uiop:process-info-input (clpm-proc-info proc)))
        (string (with-standard-io-syntax
                  (let ((*package* (find-package :clpm-client)))
                    (prin1-to-string command)))))
    (when read-eval-p
      (setf string (uiop:strcat "#." string)))
    (with-dribble-stream (dribble *clpm-dribble*)
      (with-input-from-string (s string)
        (uiop:copy-stream-to-stream s dribble :linewise t :prefix *clpm-dribble-input-prefix*)
        (terpri dribble)))
    (format stream "~A~%" string)
    (terpri stream)
    (finish-output stream)))

(defun clpm-proc-read (proc &optional (eof-error-p t) eof-value)
  "Read a form from PROC."
  (let ((stream (uiop:process-info-output (clpm-proc-info proc)))
        out)
    (uiop:with-safe-io-syntax (:package :clpm-client)
      (setf out (read stream eof-error-p eof-value)))
    (with-dribble-stream (dribble *clpm-dribble*)
      (with-input-from-string (s (prin1-to-string out))
        (uiop:copy-stream-to-stream s dribble :linewise t :prefix *clpm-dribble-output-prefix*)
        (terpri dribble)))
    out))

(defun clpm-proc-stop (proc &key abort)
  "Sends #.(uiop:quit) to the process and then waits for it to finish. Closes
all streams."
  (when (uiop:process-alive-p (clpm-proc-info proc))
    (if abort
        (uiop:terminate-process (clpm-proc-info proc))
        (clpm-proc-print proc '(uiop:quit) :read-eval-p t)))
  (prog1 (uiop:wait-process (clpm-proc-info proc))
    (when (clpm-proc-error-dribble proc)
      (close (clpm-proc-error-dribble proc)))))

(defun call-with-clpm-proc (thunk)
  (let ((proc (make-clpm-proc))
        (stopped-p nil))
    (unwind-protect
         (handler-case
             (funcall thunk proc)
           (stream-error (c)
             ;; Unable to read or write for some reason.
             ;;
             ;; We need to stop the process before attempting to get the error
             ;; output, otherwise we won't get anything (all of it?)
             (clpm-proc-stop proc :abort t)
             (setf stopped-p t)
             (error 'clpm-error
                    :wrapped-condition c
                    :error-output (get-output-stream-string (clpm-proc-error-string-output-stream proc)))))
      (unless stopped-p
        (clpm-proc-stop proc)))))

(defmacro with-clpm-proc ((proc) &body body)
  `(call-with-clpm-proc (lambda (,proc) ,@body)))
