;;;; Support for dealing with version strings
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/version-strings
    (:use #:cl
          #:alexandria)
  (:import-from #:cl-semver)
  (:import-from #:uiop
                #:split-string)
  (:export #:parse-semantic-version
           #:parse-version-specifier
           #:parse-version-string
           #:semantic-version<
           #:semantic-version<=
           #:version-spec-satisfied-p/dotted
           #:version-spec-satisfied-p/simple-string
           #:version-spec-satisfied-p/semantic))

(in-package #:clpm/version-strings)

(defparameter *version-segment-separators* '(#\- #\+)
  "A list of characters that separate segments of a version string.")

(defun safe-min (a b)
  "Like ~min~, but treats ~nil~ as positive infinity."
  (check-type a (or null number))
  (check-type b (or null number))
  (cond
    ((and a b)
     (min a b))
    (a
     a)
    (t
     b)))

(defun parse-string-or-integer (string)
  "Given a string, if it represents an integer, return the integer, otherwise
return the string itself."
  (check-type string string)
  (handler-case (parse-integer string)
    (error ()
      string)))

(defun parse-dot-separated (string &optional string-only-p)
  "Given a string consisting of dot separated strings and integers, return a
list of the strings or integers, in order."
  (mapcar (if string-only-p
              #'identity
              #'parse-string-or-integer)
          (split-string string :separator ".")))

(defun parse-version-string (version-string)
  "Given a version string, parse it into a uniform format that is easier to
reason over. The version string consists of a series of segments, separated by a
character from ~*version-segment-separators*~. Each segment consists of integers
or strings separated by period characters. Returns a list where each segment is
represented as a list of integers and/or strings and each segment is separated
by the character used to separate the segments."
  (check-type version-string string)
  (let* ((first-separator-position (reduce #'safe-min
                                           (mapcar (lambda (x) (position x version-string))
                                                   *version-segment-separators*)))
         (first-separator (and first-separator-position
                               (aref version-string first-separator-position)))
         (first-segment (subseq version-string 0 first-separator-position))
         (parsed-first-segment (parse-dot-separated first-segment)))
    (if first-separator-position
        (list* parsed-first-segment
               first-separator
               (parse-version-string (subseq version-string
                                             (1+ first-separator-position))))
        (list parsed-first-segment))))

(defun semantic-version-p (version-list)
  (and (length= 3 version-list)
       (every #'listp version-list)))

(defgeneric parse-semantic-version (string-or-list)
  (:documentation
   "A semantic version identifier has some special rules. A hyphen counts as a
   separator only if it comes before a plus character and there are no hyphens
   preceeding it. This function takes a semantic version identifier (string or
   list) and returns a list with three elements, the first is the base version
   number, the second is the prerelease information or nil, the third is the
   build metadata or nil."))

(defmethod parse-semantic-version ((string string))
  (parse-semantic-version (parse-version-string string)))

(defun semantic-version-merge (list-1 list-2)
  (append (butlast list-1)
          (list (format nil "~A-~A" (last-elt list-1) (first list-2)))
          (rest list-2)))

(defmethod parse-semantic-version ((version list))
  (let ((out nil))
    (loop
      :with pre-release-status := :maybe
      :with build-metadata-status := :maybe
      :with merge-status := nil
      :for thing :in version
      :if (listp thing)
        :if merge-status
          :do (push (semantic-version-merge (pop out) thing)
                    out)
              (setf merge-status nil)
        :else
          :do (push thing out)
        :end
      :end
      :if (eql #\- thing)
        :do
           (if (eql pre-release-status :maybe)
               ;; This is the start of prerelease info!
               (setf pre-release-status t)
               ;; The next segment needs to be merge with the previous one.
               (setf merge-status t))
      :if (eql #\+ thing)
        :do (when (eql pre-release-status :maybe)
              (push nil out))
            (setf pre-release-status nil)
            (setf build-metadata-status t)
            (setf merge-status nil)
      :finally
         (when (eql pre-release-status :maybe)
           (push nil out))
         (when (eql build-metadata-status :maybe)
           (push nil out)))
    (nreversef out)
    (assert (semantic-version-p out))
    out))

(defun semantic-version< (version1 version2)
  (cond
    ((null version1)
     (not (null version2)))
    ((null version2)
     nil)
    (t
     (cl-semver:version< (cl-semver:read-version-from-string version1)
                         (cl-semver:read-version-from-string version2)))))

(defun semantic-version<= (version1 version2)
  (not (semantic-version< version2 version1)))

(defun version-spec-satisfied-p/dotted-1 (spec version)
  (destructuring-bind (test version-spec)
      spec
    (ecase test
      (=
       (and (uiop:version<= version-spec version)
            (uiop:version<= version version-spec)))
      (<=
       (uiop:version<= version version-spec))
      (<
       (uiop:version< version version-spec))
      (>=
       (uiop:version<= version-spec version))
      (>
       (uiop:version< version-spec version))
      (/=
       (or (uiop:version< version-spec version)
           (uiop:version< version version-spec))))))

(defun version-spec-satisfied-p/dotted (spec version)
  (every (rcurry #'version-spec-satisfied-p/dotted-1 version) spec))

(defun version-spec-satisfied-p/semantic-1 (spec version)
  (destructuring-bind (test version-spec)
      spec
    (ecase test
      (=
       (and (semantic-version<= version-spec version)
            (semantic-version<= version version-spec)))
      (<=
       (semantic-version<= version version-spec))
      (<
       (semantic-version< version version-spec))
      (>=
       (semantic-version<= version-spec version))
      (>
       (semantic-version< version-spec version))
      (/=
       (or (semantic-version< version-spec version)
           (semantic-version< version version-spec))))))

(defun version-spec-satisfied-p/semantic (spec version)
  (every (rcurry #'version-spec-satisfied-p/semantic-1 version) spec))

(defun version-spec-satisfied-p/simple-string-1 (spec version)
  (destructuring-bind (test version-spec)
      spec
    (ecase test
      (=
       (string-equal version version-spec))
      (<=
       (string<= version version-spec))
      (<
       (string< version version-spec))
      (>=
       (string>= version version-spec))
      (>
       (string> version version-spec))
      (/=
       (string/= version version-spec)))))

(defun version-spec-satisfied-p/simple-string (spec version)
  (every (rcurry #'version-spec-satisfied-p/simple-string-1 version) spec))

(defun parse-version-specifier-1 (v-spec)
  "Given a string containing a single version specifier, return a s-expr'ized
version of the specifier."
  (flet ((trim-whitespace (x)
           (string-trim '(#\Space #\Tab #\Newline #\Return) x)))
    (let ((v-spec (trim-whitespace v-spec)))
      (switch (v-spec :test (lambda (x y)
                              (starts-with-subseq y x)))
        ("=="
         (list '= (trim-whitespace (subseq v-spec 2))))
        ("="
         (list '= (trim-whitespace (subseq v-spec 1))))
        ("!="
         (list '/= (trim-whitespace (subseq v-spec 2))))
        (">="
         (list '>= (trim-whitespace (subseq v-spec 2))))
        (">"
         (list '> (trim-whitespace (subseq v-spec 1))))
        ("<="
         (list '<= (trim-whitespace (subseq v-spec 2))))
        ("<"
         (list '< (trim-whitespace (subseq v-spec 1))))
        (t
         (list '= v-spec))))))

(defun parse-version-specifier (v-spec)
  "Given a string containing (potentially multiple) version specifiers, return a
list of version specifiers."
  (let ((sub-strs (split-string v-spec :separator '(#\,))))
    (mapcar #'parse-version-specifier-1 sub-strs)))

(defun min-with-nil (&rest args)
  (apply #'min (remove nil args)))

(defun parse-package-atom (atom)
  (assert (stringp atom))
  (let* ((equal-position (search "==" atom))
         (!-position (search "!=" atom))
         (>-position (search ">" atom))
         (<-position (search "<" atom))
         (package-name atom)
         (version-spec-string "")
         ;;(version-specs nil)
         (start-of-version-spec-string (min-with-nil equal-position
                                                     !-position
                                                     >-position
                                                     <-position)))
    (assert (not (and equal-position
                      (or >-position
                          <-position))))
    (setf package-name (subseq atom 0 start-of-version-spec-string)
          version-spec-string (subseq atom start-of-version-spec-string))
    (values package-name
            (mapcar #'parse-version-specifier
                    (split-string version-spec-string :separator ",")))))
