;;;; Support for concatenating source files for a package inferred system
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-asdf/concatenate-package-inferred-system-source-op
    (:use #:cl
          #:asdf)
  (:export #:concatenate-package-inferred-system-source-op))

(in-package #:clpm-asdf/concatenate-package-inferred-system-source-op)

(defclass concatenate-package-inferred-system-source-op
    (asdf/concatenate-source::basic-concatenate-source-op
     non-propagating-operation)
  ()
  (:documentation
   "An operation that concatenates all source files for a package inferred
system."))

(defun chase-parent (c)
  (if (component-parent c)
      (chase-parent (component-parent c))
      c))

(defmethod asdf:input-files ((operation concatenate-package-inferred-system-source-op) (s system))
  (check-type s package-inferred-system)
  ;; Only work on the primary system directly.
  (assert (equal (primary-system-name s) (component-name s)))
  (loop :with encoding = (or (component-encoding s) *default-encoding*)
        :with other-encodings = '()
        :with around-compile = (asdf::around-compile-hook s)
        :with other-around-compile = '()
        :with primary-system-name = (primary-system-name s)
        :for c :in (required-components  ;; see note about similar call to required-components
                    s :goal-operation 'load-op ;;  in bundle.lisp
                      :keep-operation 'asdf::basic-compile-op
                      :other-systems t)
        :append
        (when (and (typep c 'cl-source-file)
                   (equal (primary-system-name (chase-parent c)) primary-system-name))
          (let ((e (component-encoding c)))
            (unless (equal e encoding)
              (let ((a (assoc e other-encodings)))
                (if a (push (component-find-path c) (cdr a))
                    (push (list a (component-find-path c)) other-encodings)))))
          (unless (equal around-compile (asdf::around-compile-hook c))
            (push (component-find-path c) other-around-compile))
          (input-files (make-operation 'compile-op) c)) :into inputs
        :finally
           (when other-encodings
             (warn "~S uses encoding ~A but has sources that use these encodings:~{ ~A~}"
                   operation encoding
                   (mapcar #'(lambda (x) (cons (car x) (list (reverse (cdr x)))))
                           other-encodings)))
           (when other-around-compile
             (warn "~S uses around-compile hook ~A but has sources that use these hooks: ~A"
                   operation around-compile other-around-compile))
           (return inputs)))
