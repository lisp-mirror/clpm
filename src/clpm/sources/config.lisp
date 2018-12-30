(uiop:define-package #:clpm/sources/config
    (:use #:cl
          #:alexandria
          #:clpm/cache
          #:clpm/config
          #:clpm/data
          #:clpm/fetch
          #:clpm/sources/defs
          #:clpm/sources/quicklisp
          #:clpm/utils)
  (:import-from #:uiop
                #:read-file-form
                #:with-safe-io-syntax)
  (:export #:load-source-from-form
           #:load-sources))

(in-package #:clpm/sources/config)

(defun sources-config-pathname ()
  "Return the pathname to the sources.conf file containing the list of sources."
  (clpm-config '("sources.conf")))

(defun sources-config-form ()
  "Read the form from SOURCES-CONFIG-PATHNAME."
  (with-safe-io-syntax ()
    (read-file-form (sources-config-pathname))))

(defun raw-sources-config-forms ()
  "Get a list of forms from SOURCES-CONFIG-PATHNAME representing all sources the
user wants to use."
  (let ((sources-config-form (sources-config-form)))
    (assert (eql :clpm-sources (first sources-config-form)))
    (rest sources-config-form)))

(defun sources-map-pathname ()
  "Return the pathname to the source-map file."
  (clpm-data '("source-map")))

(defun sources-map-form ()
  "Returns a form that maps source URLs to a plist containing the source's name
and type."
  (let ((pathname (sources-map-pathname)))
    (when (and pathname
               (probe-file pathname))
      (with-safe-io-syntax ()
        (read-file-form pathname)))))

(defun validate-source (source)
  "Ensures that the source has a name, and a type. Additionally, ensures the URL
matches any previously seen source with the same name and that the type hasn't
changed since this source was last seen."
  (assert (not (typep source 'raw-source)))
  (let* ((name (source/name source))
         (url (source/url source))
         (type-keyword (source-type-keyword source))
         (source-map (sources-map-form))
         (pair-matching-url (find (uri-to-string url) source-map
                                  :key (lambda (x)
                                         (getf (rest x) :url))
                                  :test #'string-equal))
         (pair-matching-name (assoc name source-map :test #'string-equal)))

    (assert (and (stringp name)
                 (not (string-equal "" name))))

    (when pair-matching-url
      ;; We've synced this URL before, make sure the name and type haven't
      ;; changed.
      (assert (eql type-keyword (getf (rest pair-matching-url) :type)))
      (assert (equal name (first pair-matching-url))))

    (when pair-matching-name
      ;; We've seen this name before. Make sure the URLs match.
      (assert (equal (uri-to-string url) (getf (rest pair-matching-name) :url))))))

(defun resolve-raw-source! (s &optional lazy)
  "Given a RAW-SOURCE instance S, determine what type of source it actually is
and change its class."
  (when (typep s 'raw-source)
    (unless (resolve-raw-source-from-metadata! s)
      (unless lazy
        (let* ((source-contents (fetch-url (source/url s))))
          (dolist (test-fun *source-test-functions*)
            (multiple-value-bind (class-name initargs) (funcall test-fun (source/url s) source-contents)
              (when class-name
                (apply #'change-class s class-name (append (source/args s) initargs))
                (validate-source s)
                (return))))))))
  (unless (typep s 'raw-source)
    (validate-source s))
  s)

(defun resolve-raw-source-from-metadata! (s)
  "Try to change S's class to the right type based only on local metadata."
  (check-type s raw-source)
  (let* ((url (uri-to-string (source/url s)))
         (source-map (sources-map-form))
         (source-metadata (assoc-value source-map url :test #'string-equal))
         (type (getf source-metadata :type))
         (name (getf source-metadata :name)))
    (when type
      (apply #'change-class s
             (resolve-type type)
             (append (source/args s)
                     (list :name name)))
      (validate-source s)
      t)))

(defun update-metadata (source)
  (let ((source-map (sources-map-form))
        (source-name (source/name source)))
    (setf (assoc-value source-map source-name :test #'string-equal)
          (rest (source-to-form source)))
    (ensure-directories-exist (sources-map-pathname))
    (with-open-file (s (sources-map-pathname) :direction :output
                                              :if-exists :supersede)
      (with-safe-io-syntax ()
        (print source-map s)))
    source-map))

(defmethod sync-source :before ((source clpm-known-source))
  (validate-source source)
  (update-metadata source))

(defun resolve-type (type)
  (ecase type
    (:quicklisp
     'quicklisp-source)))

(defun load-source-from-form (f)
  (destructuring-bind (name &rest args &key type url &allow-other-keys)
      f
    (assert (stringp name))
    (assert (stringp url))
    (let* ((trimmed-args (remove-from-plist args :url :type))
           (new-args (loop
                       :for key :in trimmed-args :by #'cddr
                       :for value :in (rest trimmed-args) :by #'cddr
                       :appending (list key (case value
                                              (cl-toml:true
                                               t)
                                              (cl-toml:false
                                               nil)
                                              (t
                                               value)))))
           (source
             (if type
                 (apply #'make-instance (resolve-type (make-keyword (string-upcase type)))
                        :name name
                        :url url
                        new-args)
                 (make-instance 'raw-source
                                :name name
                                :url url
                                :args new-args))))
      (resolve-raw-source! source t)
      source)))

(defun table-to-plist (table)
  (let ((out nil))
    (maphash (lambda (k v)
               (push v out)
               (push (make-keyword (string-upcase k)) out))
             table)
    out))

(defun load-sources ()
  (let ((sources-table (config-value "sources"))
        (source-forms nil))
    (maphash (lambda (k v)
               (push (cons k (table-to-plist v)) source-forms))
             sources-table)
    (mapcar #'load-source-from-form source-forms)))

(defmethod sync-source ((source raw-source))
  (resolve-raw-source! source)
  (unless (typep source 'raw-source)
    (sync-source source)))
