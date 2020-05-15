;;;; Definitions for using environment variables as a config source for CLPM.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/config/env-source
    (:use #:cl
          #:alexandria
          #:clpm/config/defs
          #:clpm/config/source-defs
          #:clpm/utils)
  (:import-from #:cl-ppcre)
  (:export #:config-env-source))

(in-package #:clpm/config/env-source)

(defclass config-env-source (config-source)
  ()
  (:documentation
   "A configuration source backed by environment variables."))

(defgeneric path-segment-to-env-var (type segment))

(defmethod path-segment-to-env-var (type segment)
  (cl-ppcre:regex-replace-all "-" (string-upcase (if (stringp segment)
                                                     segment
                                                     (symbol-name segment)))
                              "_"))

(defmethod path-segment-to-env-var ((type (eql 'hostname)) segment)
  (uiop:strcat (string-upcase
                (cl-ppcre:regex-replace-all #\. (cl-ppcre:regex-replace-all "-" segment "_") "__"))
               "_"))

(defun path-to-env-var (path &optional config-info)
  "Given a config path, return the environment variable which would contain its
value."
  (let* ((config-info (or config-info (get-config-entry path)))
         (wildcard-types (getf (cdr config-info) :wildcard-types))
         (canonical-path (car config-info))
         (type (getf (cdr config-info) :type))
         (hash-table-p (eql type 'hash-table)))
    (format nil "CLPM_~{~A~^_~}~:[~;_~]"
            (mapcar (lambda (x)
                      (if (eql (pop canonical-path) :*)
                          (path-segment-to-env-var (pop wildcard-types) x)
                          (cl-ppcre:regex-replace-all "-" (string-upcase (symbol-name x)) "_")))
                    path)
            hash-table-p)))

(defmethod config-source-value ((config-source config-env-source) path)
  (let* ((config-info (get-config-entry path))
         (type (getf (cdr config-info) :type))
         (env-var-name (path-to-env-var path config-info)))
    (when-let ((env-value (uiop:getenv env-var-name)))
      (values (parse-string-config-value env-value type) t))))

(defun build-envvar-regex (path types capture-index)
  `(:sequence
    "CLPM_"
    ,@(butlast
       (loop
         :for i :upfrom 0
         :for type :in types
         :for segment :in path
         :if (eql segment :*)
           :collect
           (let ((raw (case type
                        (hostname
                         '(:sequence
                           (:greedy-repetition 0 nil :everything)
                           "_"))
                        (t
                         '(:greedy-repetition 0 nil :everything)))))
             (if (= i capture-index)
                 `(:register ,raw)
                 raw))
         :else
           :collect (path-segment-to-env-var type segment)
         :end
         :collect "_"))))

(defun env-var-to-lisp-path-segment (type env-var-name)
  (ecase type
    ((string header)
     (make-keyword (string-upcase (cl-ppcre:regex-replace-all #\_ env-var-name "-"))))
    (hostname
     ;; Strip off the trailing _, replace __ with . and all remaining _ with -
     (string-downcase
      (cl-ppcre:regex-replace-all
       #\_
       (cl-ppcre:regex-replace-all
        "__"
        (subseq env-var-name 0 (1- (length env-var-name)))
        ".")
       "-")))))

(defmethod config-source-implicit-keys ((config-source config-env-source) path)
  "Given the path, create a regex the matches all possible full extensions of
the path. Then search all defined environment variables to find matches and
extract the children keys."
  (let* ((all-paths-matching-prefix (get-all-config-paths-with-prefix path))
         (wildcard-info (get-config-entry (append path '(:*))))
         (wildcard-type (last-elt (getf (cdr wildcard-info) :wildcard-types)))
         (all-regexes (mapcar (lambda (complete-path)
                                (build-envvar-regex complete-path
                                                    (config-path-key-types complete-path)
                                                    (length path)))
                              all-paths-matching-prefix))
         (all-scanners (mapcar #'cl-ppcre:create-scanner all-regexes))
         out)
    (dolist (scanner all-scanners)
      (dolist (pair (posix-environment-alist))
        (let ((var-name (car pair)))
          (multiple-value-bind (match regs) (cl-ppcre:scan-to-strings scanner var-name :sharedp t)
            (when match
              (pushnew (aref regs 0) out :test #'equal))))))
    (mapcar (curry #'env-var-to-lisp-path-segment wildcard-type) out)))
