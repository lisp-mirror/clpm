;;;; User interface functions
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)

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

(defun context-diff-approved-p (context-diff)
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
