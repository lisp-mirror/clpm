;;;; Build features
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-asdf)

(defparameter *clpm-feature-documentation*
  `((:clpm . "Required feature. Denotes that CLPM is present.")
    (:clpm-curl . "Build support for using the curl executable to download files.")
    (:clpm-dexador . "Build support for using the Dexador library to download files.")
    (:clpm-drakma . "Build support for using the Drakma library to download files.")
    (:clpm-firejail . "EXPERIMENTAL: Build support for using firejail to sandbox grovelers.")
    (:clpm-openssl . "Build support for using openssl (via cl+ssl library) to talk with servers over HTTPS.")))

(defparameter *default-darwin-feature-set*
  '(:clpm
    :clpm-curl
    :clpm-dexador
    :clpm-drakma
    :clpm-firejail
    :clpm-openssl))

(defparameter *default-linux-feature-set*
  '(:clpm
    :clpm-curl
    :clpm-dexador
    :clpm-drakma
    :clpm-firejail
    :clpm-openssl))

(defparameter *default-windows-feature-set*
  '(:clpm
    :clpm-curl
    :clpm-dexador))

(defun default-features ()
  #+:darwin (copy-list *default-darwin-feature-set*)
  #+:linux (copy-list *default-linux-feature-set*)
  #+:windows (copy-list *default-windows-feature-set*))

(defun validate-features (clpm-features)
  (flet ((present (feature)
           (member feature clpm-features)))
    (unless (present :clpm)
      (error ":CLPM must be present on the features list."))
    (unless (or (present :clpm-dexador)
                (present :clpm-drakma)
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

    ;; If there is not going to be OpenSSL support built in, tell Dexador to not
    ;; load CL+SSL.
    (when (and (present :clpm-dexador)
               (not (present :clpm-openssl)))
      (pushnew :dexador-no-ssl clpm-features))

    ;; If no Lisp clients that use openssl are present, remove it from the
    ;; feature set.
    (unless (or (present :clpm-drakma)
                (present :clpm-dexador))
      (setf clpm-features (remove :clpm-openssl clpm-features))))
  clpm-features)

(defun load-features-from-file ()
  (let ((pn (merge-pathnames (make-pathname :name "customize-target-features"
                                            :type "lisp-expr")
                             (asdf:system-source-file "clpm-asdf")))
        (clpm-features (default-features)))
    (when (probe-file pn)
      (let* ((form (uiop:read-file-form pn))
             (fun (compile nil form)))
        (setf clpm-features (funcall fun clpm-features))))
    (validate-features clpm-features)
    (dolist (f (augment-features clpm-features))
      (pushnew f *features*))
    *features*))

(defun clpm-features (&optional (features *features*))
  (remove-if-not (lambda (f) (member f *clpm-feature-documentation* :key #'car))
                 features))

(eval-when (:load-toplevel :execute)
  (unless (member :clpm *features*)
    (load-features-from-file)))
