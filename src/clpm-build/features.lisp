;;;; CLPM Build Feature Helpers
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-build/features
    (:use #:cl))

(in-package #:clpm-build/features)

(defparameter *default-linux-feature-set*
  '(:clpm
    :clpm-curl
    :clpm-drakma
    :clpm-firejail
    :clpm-openssl))

(defparameter *default-windows-feature-set*
  '(:clpm
    :clpm-curl
    :deploy-console))

#+:linux
(defun default-features ()
  (copy-list *default-linux-feature-set*))

#+:win32
(defun default-features ()
  (copy-list *default-windows-feature-set*))

(defun validate-features! (clpm-features)
  (flet ((present (feature)
           (member feature clpm-features)))
    (unless (present :clpm)
      (error ":CLPM must be present on the features list."))
    (unless (or (present :clpm-drakma)
                (present :clpm-curl))
      (error "At least one HTTP client must be included."))))

(defun augment-features (clpm-features)
  (flet ((present (feature)
           (member feature clpm-features)))
    ;; If there is not going to be OpenSSL support built in, tell Drakma to not
    ;; load CL+SSL.
    (when (and (present :clpm-drakma)
               (not (present :clpm-openssl)))
      (pushnew :drakma-no-ssl clpm-features))

    ;; If no Lisp clients that use openssl are present, remove it from the
    ;; feature set.
    (unless (present :clpm-drakma)
      (setf clpm-features (remove :clpm-openssl clpm-features))))
  clpm-features)

(defun load-features-from-file ()
  (let ((pn (merge-pathnames (make-pathname :name "customize-target-features"
                                            :type "lisp-expr")
                             (asdf:system-source-file "clpm-build")))
        (clpm-features (default-features)))
    (when (probe-file pn)
      (let* ((form (uiop:read-file-form pn))
             (fun (compile nil form)))
        (setf clpm-features (funcall fun clpm-features))))
    (validate-features! clpm-features)
    (setf *features* (append (augment-features clpm-features) *features*))
    (append (augment-features clpm-features) *features*)
    *features*))

(eval-when (:load-toplevel :execute)
  (load-features-from-file))
