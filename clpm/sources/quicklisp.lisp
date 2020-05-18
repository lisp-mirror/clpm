;;;; Quicklisp based source
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

;; * define-package
(uiop:define-package #:clpm/sources/quicklisp
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/cache
          #:clpm/data
          #:clpm/http-client
          #:clpm/session
          #:clpm/sources/clpi
          #:clpm/sources/defs
          #:clpm/sources/ql-clpi
          #:clpm/utils
          #:do-urlencode)
  (:import-from #:clpi)
  (:import-from #:puri)
  (:import-from #:ql-clpi)
  (:export #:ql-source))

(in-package #:clpm/sources/quicklisp)


;; * Source

(defclass ql-source (ql-clpi-source)
  ())

(defmethod make-source ((type (eql 'ql-source)) &rest initargs &key url name &allow-other-keys)
  (let ((url-string (if (stringp url) url (uri-to-string url))))
    (with-clpm-session (:key `(make-source ,type ,name ,url-string))
      (apply #'make-instance
             type
             initargs))))

(defmethod source-cache-directory ((source ql-source))
  "Compute the cache location for this source, based on its canonical url."
  (let ((url (source-url source)))
    (clpm-cache-pathname
     `("sources"
       "quicklisp"
       ,(apply #'concatenate 'string
               (string-downcase (puri:uri-host url))
               "_"
               (format nil "~d" (url-port url))
               (awhen (puri:uri-path url)
                 (list "_"
                       (urlencode (subseq it 1))))))
     :ensure-directory t)))

(defmethod source-can-lazy-sync-p ((source ql-source))
  nil)

(defmethod source-lib-directory ((source ql-source))
  "Compute the cache location for this source, based on its canonical url."
  (let ((url (source-url source)))
    (clpm-data-pathname
     `("sources"
       "quicklisp"
       ,(apply #'concatenate 'string
               (string-downcase (puri:uri-host url))
               "_"
               (format nil "~d" (url-port url))
               (awhen (puri:uri-path url)
                 (list "_"
                       (urlencode (subseq it 1))))))
     :ensure-directory t)))

(defmethod source-to-form ((source ql-source))
  (list (source-name source)
        :url (uri-to-string (source-url source))
        :type (source-type-keyword source)))

(defmethod source-type-keyword ((source ql-source))
  :quicklisp)

(defmethod sync-source ((source ql-source))
  (let ((index (clpi-source-index source))
        (ql-dist (make-instance 'ql-clpi:ql-dist
                                :uri (source-url source)
                                :cache-pathname (merge-pathnames
                                                 "quicklisp/"
                                                 (source-cache-directory source))
                                :http-client (get-http-client))))
    (ql-clpi:ql-dist-to-clpi ql-dist index :appendable-p t)
    (clpi:index-save index)))
