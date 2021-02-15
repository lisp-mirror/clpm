;;;; User interface functions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)

(defvar *context-diff-approval-method* :error
  "Controls how CLPM prompts the user to approve diffs before acting on
them. Can be one of:

+ :ERROR :: A condition of type CONTEXT-DIFF-NEEDS-APPROVAL is signaled using
ERROR. The APPROVE-DIFF and REJECT-DIFF restarts are made available.

+ :YES-OR-NO-P :: The diff is printed and the user asked to approve or not using
YES-OR-NO-P.

+ T :: The diff is accepted without prompting.")

(define-condition context-diff-needs-approval ()
  ((diff
    :initarg :diff
    :reader context-diff-needs-approval-diff
    :documentation "A CONTEXT-DIFF instance."))
  (:report (lambda (condition stream)
             (format stream "CLPM needs the following diff to be approved or denied:~%")
             (print-context-diff-to-stream (context-diff-needs-approval-diff condition) stream)))
  (:documentation
   "Condition reporting that a CONTEXT-DIFF needs approval."))

(defun context-diff-approved-p/error (context-diff)
  "Signal an error with a CONTEXT-DIFF-NEEDS-APPROVAL condition. Establish
APPROVE-DIFF and REJECT-DIFF restarts."
  (restart-case
      (progn
        (error 'context-diff-needs-approval
               :diff context-diff))
    (approve-diff ()
      :report "Approve diff and continue"
      t)
    (reject-diff ()
      :report "Reject diff"
      nil)))

(defun context-diff-approved-p/yes-or-no-p (context-diff)
  "Use YES-OR-NO-P to ask the user for approval."
  (let ((string
          (with-output-to-string (s)
            (format s "CLPM needs the following diff to be approved or denied:~%")
            (print-context-diff-to-stream context-diff s)
            (format s "~&Do you approve?"))))
    (yes-or-no-p "~A" string)))

(defun context-diff-approved-p (context-diff)
  "Ask the user to approve a diff, based on value of
*CONTEXT-DIFF-APPROVAL-METHOD."
  (ecase *context-diff-approval-method*
    (:error (context-diff-approved-p/error context-diff))
    (:yes-or-no-p (context-diff-approved-p/yes-or-no-p context-diff))
    (t t)))


(defun approve-diff (&optional condition)
  "Invoke the APPROVE-DIFF restart or return NIL if no such restart exists."
  (let ((restart (find-restart 'approve-diff condition)))
    (when restart
      (invoke-restart restart))))

(defun reject-diff (&optional condition)
  "Invoke the REJECT-DIFF restart or return NIL if no such restart exists."
  (let ((restart (find-restart 'reject-diff condition)))
    (when restart
      (invoke-restart restart))))
