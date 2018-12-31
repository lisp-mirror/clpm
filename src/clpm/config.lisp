(uiop:define-package #:clpm/config
    (:use #:cl
          #:alexandria
          #:iterate)
  (:export #:*clpm-config-directories*
           #:clpm-config-pathname
           #:config-value
           #:merge-file-into-config
           #:merge-ht-into-config
           #:print-config))

(in-package #:clpm/config)

(uiop:define-package #:clpm-user-config
    (:use #:cl)
  (:import-from #:clpm/config
                #:clpm-config))

(defparameter *default-config*
  `(:grovel
    (:sandbox
     (:method :auto))
    :http-client
    (:curl
     (:path "curl")
     :method :auto))
  "Default configuration of clpm")

(defvar *config* (make-hash-table :test 'equalp)
  "Active configuration. Computed at program execution time.")

(defvar *inner-config* nil)


;; * Parsing configs

(defun names-path-p (path-1 path-2)
  (or (equal path-1 path-2)
      (and (eql '* (first path-2))
           (names-path-p (rest path-1)
                         (rest path-2)))))

(defun path-table-p (path)
  (switch (path :test #'names-path-p)
    ('(:git)
      t)
    ('(:remotes :git)
      t)
    ('(* :remotes :git)
      t)
    ('(:http)
      t)
    ('(:headers :http)
      t)
    ('(* :headers :http)
      t)
    ('(* * :headers :http)
      t)
    ('(:http-client)
      t)
    ('(:curl :http-client)
      t)
    ('(:grovel)
      t)
    ('(:sandbox :grovel)
      t)
    ('(:sources)
      t)
    ('(* :sources)
      t)))

(defun merge-tables (&rest tables)
  (let ((out (make-hash-table :test 'equalp)))
    (iter
      (for table :in (reverse tables))
      (setf out (merge-hts table out)))
    out))

(defun parse-table (path table)
  (let ((out (make-hash-table :test 'equalp)))
    (iter
      (for (key value) :on table :by #'cddr)
      (for new-path := (list* key path))
      (for value-table-p := (path-table-p new-path))
      (if value-table-p
          (setf (gethash key out) (parse-table new-path value))
          (setf (gethash key out) value)))
    out))

(defmacro clpm-config (&rest directives &key &allow-other-keys)
  `(setf *inner-config*
         (merge-tables ,@(iter
                           (for (key value) :on (append directives *default-config*) :by #'cddr)
                           (for table := (gensym))
                           (collect
                               `(let ((,table (make-hash-table :test 'equalp)))
                                  (setf (gethash ,key ,table)
                                        (parse-table (list ,key) ',value))
                                  ,table))))))



(defun system-config-directories ()
  (uiop:system-config-pathnames "clpm/"))

(defun user-config-directories ()
  (uiop:xdg-config-pathnames "clpm/"))

(defparameter *default-clpm-config-directories*
  (list 'user-config-directories
        'system-config-directories))

(defvar *clpm-config-directories*)

(defun compute-clpm-config-dirs ()
  (let* ((env-dirs (uiop:getenv-absolute-directories "CLPM_CONFIG_DIRS"))
         (nil-cell (member nil env-dirs))
         (*default-clpm-config-directories* (reduce #'append (mapcar #'funcall *default-clpm-config-directories*))))
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

(defun clpm-config-pathname (x &key (direction :input) ensure-directory)
  (let ((files (mapcar (lambda (defaults)
                         (uiop:resolve-absolute-location (list* defaults x)
                                                         :ensure-directory ensure-directory))
                       *clpm-config-directories*)))
    (uiop:find-preferred-file files :direction direction)))


(uiop:register-image-restore-hook 'compute-clpm-config-dirs)
(uiop:register-image-dump-hook (lambda ()
                                 (setf *clpm-config-directories* nil)))

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
  (let ((*package* (find-package :clpm-user-config))
        (*inner-config* nil))
    (load pathname)
    (when *inner-config*
      (merge-ht-into-config *inner-config*))))

(defun load-global-config ()
  (let* ((config-file (clpm-config-pathname '("clpm.conf"))))
    (merge-file-into-config config-file)
    *config*))

(defun clear-global-config ()
  (setf *config* (make-hash-table :test 'equalp)))

(defun flatten-hts (ht)
  (iter
    (for (key value) :on (hash-table-plist ht) :by #'cddr)
    (collect key)
    (if (hash-table-p value)
        (collect (flatten-hts value))
        (collect value))))

(defun print-config (stream)
  (format stream "~S~%" (flatten-hts *config*)))

(uiop:register-clear-configuration-hook 'clear-global-config)
(uiop:register-image-restore-hook 'load-global-config)
