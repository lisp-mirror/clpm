(uiop:define-package #:clpm/config
    (:use #:cl
          #:alexandria)
  (:import-from #:cl-toml
                #:false
                #:true)
  (:export #:*clpm-config-directories*
           #:clpm-config
           #:config-value
           #:false
           #:merge-file-into-config
           #:merge-ht-into-config
           #:print-config
           #:true))

(in-package #:clpm/config)

(defparameter *default-config*
  (alist-hash-table
   `(("grovel" . ,(alist-hash-table
                   `(("sandbox" . ,(alist-hash-table
                                    `(("method" . "auto"))
                                    :test 'equal)))
                   :test 'equal)))
   :test 'equal)
  "Default configuration of clpm")

(defvar *config* nil
  "Active configuration. Computed at program execution time.")



(defun system-config-directories ()
  (uiop:system-config-pathnames "clpm/"))

(defun user-config-directories ()
  (uiop:xdg-config-pathnames "clpm/"))

(defparameter *default-clpm-config-directories*
  (append (user-config-directories)
          (system-config-directories)))

(defvar *clpm-config-directories* *default-clpm-config-directories*)

(defun compute-clpm-config-dirs ()
  (let* ((env-dirs (uiop:getenv-absolute-directories "CLPM_CONFIG_DIRS"))
         (nil-cell (member nil env-dirs)))
    (if env-dirs
        (progn
          (when nil-cell
            (setf (car nil-cell)
                  (first *default-clpm-config-directories*))
            (setf (cdr nil-cell)
                  (append (rest *default-clpm-config-directories*)
                          (cdr nil-cell))))
          (setf *clpm-config-directories* env-dirs))
        (setf *clpm-config-directories* *default-clpm-config-directories*))))

(defun clpm-config (x &key (direction :input) ensure-directory)
  (let ((files (mapcar (lambda (defaults)
                         (uiop:resolve-absolute-location (list* defaults x)
                                                         :ensure-directory ensure-directory))
                       *clpm-config-directories*)))
    (uiop:find-preferred-file files :direction direction)))


(uiop:register-image-restore-hook 'compute-clpm-config-dirs)
(uiop:register-image-dump-hook (lambda ()
                                 (setf *clpm-config-directories* *default-clpm-config-directories*)))

(defun gethashes (hash-table &rest keys)
  (gethashes* hash-table keys))

(defun gethashes* (hash-table-or-value keys)
  (if keys
      (when hash-table-or-value
        (gethashes* (gethash (first keys) hash-table-or-value) (rest keys)))
      hash-table-or-value))

(defun (setf gethashes*) (value hash-table keys)
  (if (rest keys)
      (setf (gethashes* (ensure-gethash (first keys) hash-table
                                        (make-hash-table :test 'equal))
                        (rest keys))
            value)
      (setf (gethash (first keys) hash-table) value)))

(defun (setf gethashes) (value hash-table &rest keys)
  (setf (gethashes* hash-table keys) value))

(defun config-value (&rest path)
  (gethashes* *config* path))

(defun merge-hts (new-ht default-ht)
  (let ((out (copy-hash-table default-ht)))
    (maphash
     (lambda (k v)
       (multiple-value-bind (default-v exists-p)
           (gethash k out)
         (cond
           ((or
             (hash-table-p v)
             (hash-table-p default-v))
            ;; Either both must be hash tables or the default shouldn't exist.
            (unless (or (not exists-p)
                        (and (hash-table-p default-v)
                             (hash-table-p v)))
              (error "Cannot merge ~S with ~S" v default-v))
            ;; Merge the hash tables together
            (if (null default-v)
                (setf (gethash k out) v)
                (setf (gethash k out) (merge-hts v default-v))))
           ((or
             (listp v)
             (and exists-p (listp default-v)))
            ;; The new value is a list. The default value must be a list.
            (unless (or (not exists-p)
                        (and (listp default-v)
                             (listp v)))
              (error "Cannot merge ~S with ~S" v default-v))
            ;; Append the lists together.
            (setf (gethash k out) (append v default-v)))
           (t
            (setf (gethash k out) v)))))
     new-ht)
    out))

(defun merge-ht-into-config (new-ht)
  (setf *config* (merge-hts new-ht *config*)))

(defun merge-file-into-config (pathname)
  (let ((config-ht (cl-toml:parse-file pathname :array-as :list)))
    (merge-ht-into-config config-ht)))

(defun load-global-config ()
  (let* ((config-file (clpm-config '("clpm.toml"))))
    (setf *config* (copy-hash-table *default-config*))
    (when config-file
      (merge-file-into-config config-file))
    *config*))

(defun clear-global-config ()
  (setf *config* nil))

(defun print-config (stream)
  (cl-toml:encode *config* stream))

(uiop:register-clear-configuration-hook 'clear-global-config)
(uiop:register-image-restore-hook 'load-global-config)
