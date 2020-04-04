;;;; Definitions for using environment variables as a config source for CLPM.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/config/env-source
    (:use #:cl
          #:alexandria
          #:clpm/config/defs
          #:clpm/config/source-defs)
  (:import-from #:cl-ppcre)
  (:export #:config-env-source))

(in-package #:clpm/config/env-source)

(defclass config-env-source ()
  ()
  (:documentation
   "A configuration source backed by environment variables."))

(defun path-to-env-var (path config-info)
  "Given a config path, return the environment variable which would contain its
value."
  (let* ((wildcard-types (getf (cdr config-info) :wildcard-types))
         (canonical-path (car config-info))
         (type (getf (cdr config-info) :type))
         (hash-table-p (eql type 'hash-table)))
    (format nil "CLPM_~{~A~^_~}~:[~;_~]"
            (mapcar (lambda (x)
                      (if (eql (pop canonical-path) :*)
                          (ecase (pop wildcard-types)
                            ((header string)
                             (if (stringp x)
                                 (string-upcase
                                  (cl-ppcre:regex-replace-all "-" x "_"))
                                 (string-upcase
                                  (cl-ppcre:regex-replace-all "-" (symbol-name x) "_"))))
                            (hostname
                             (uiop:strcat
                              (string-upcase
                               (cl-ppcre:regex-replace-all #\. (cl-ppcre:regex-replace-all "-" x "_") "__"))
                              "_")))
                          (cl-ppcre:regex-replace-all "-" (string-upcase (symbol-name x)) "_")))
                    path)
            hash-table-p)))

(defun parse-env-value (value type)
  (cond
    ((eql type 'string)
     value)
    ((eql type 'boolean)
     (let ((value (string-downcase value))
           (orig-value value))
       (cond
         ((or (equalp value "0")
              (equalp value "n")
              (equalp value "no")
              (equalp value "false")
              (equalp value "nil"))
          nil)
         ((or (equalp value "1")
              (equalp value "y")
              (equalp value "yes")
              (equalp value "true")
              (equalp value "t"))
          t)
         (t
          (error "Unable to parse ~S as a boolean." orig-value)))))
    ((and (listp type)
          (eql (first type) 'member)
          (every (lambda (x) (or (keywordp x) (eql x nil) (eql x t))) (rest type)))
     (cond
       ((equalp value "t")
        t)
       ((equalp value "nil")
        nil)
       (t
        (let ((kw (make-keyword (uiop:standard-case-symbol-name value))))
          (unless (typep kw type)
            (error "Unknown value ~S for type ~S" kw type))
          kw))))
    ((equal type '(or string pathname))
     (uiop:parse-native-namestring value))
    (t
     (error "Unknown type ~S to parse from a string." type))))

(defmethod config-source-value ((config-source config-env-source) path)
  (let* ((config-info (get-config-entry path))
         (type (getf (cdr config-info) :type))
         (env-var-name (path-to-env-var path config-info)))
    (when-let ((env-value (uiop:getenv env-var-name)))
      (values (parse-env-value env-value type) t))))

(defmethod config-source-implicit-keys ((config-source config-env-source) path)
  "We don't currently support looking up implicit keys from the environment."
  nil)
