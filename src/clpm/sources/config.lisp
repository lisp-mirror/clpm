(uiop:define-package #:clpm/sources/config
    (:use #:cl
          #:alexandria
          #:clpm/cache
          #:clpm/config
          #:clpm/data
          #:clpm/http-client
          #:clpm/sources/defs
          #:clpm/sources/quicklisp
          #:clpm/utils)
  (:import-from #:uiop
                #:read-file-form
                #:with-safe-io-syntax)
  (:export #:load-source-from-form
           #:load-sources))

(in-package #:clpm/sources/config)

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
    (assert (keywordp type))
    (let* ((trimmed-args (remove-from-plist args :url :type))
           (new-args (loop
                       :for key :in trimmed-args :by #'cddr
                       :for value :in (rest trimmed-args) :by #'cddr
                       :collect key
                       :collect value))
           (source (apply #'make-instance (resolve-type type)
                          :name name
                          :url url
                          new-args)))
      (validate-source source)
      source)))

(defun load-sources ()
  (let ((sources-table (config-value :sources))
        (source-forms nil))
    (maphash (lambda (k v)
               (push (cons k (hash-table-plist v)) source-forms))
             sources-table)
    (mapcar #'load-source-from-form source-forms)))
