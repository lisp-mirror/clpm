(uiop:define-package #:clpm/version-strings
    (:use #:cl
          #:alexandria)
  (:import-from #:uiop
                #:split-string)
  (:export ;;#:parse-semantic-version
           #:parse-version-specifier
           #:semantic-version<
           #:semantic-version<=
           #:version-spec-satisfied-p/simple-string
           #:version-spec-satisfied-p/semantic))

(in-package #:clpm/version-strings)

(defun has-build-metadata-p (first-hyphen first-plus)
  (declare (ignore first-hyphen))
  first-plus)

(defun has-pre-release-p (first-hyphen first-plus)
  (and first-hyphen
       (or (not first-plus)
           (< first-hyphen first-plus))))

(defun parse-dot-separated (string &optional string-only-p)
  (mapcar (if string-only-p
              #'identity
              #'parse-string-or-integer)
          (split-string string :separator ".")))

(defun parse-string-or-integer (string)
  (handler-case (parse-integer string)
    (error ()
      string)))

(defun parse-semantic-version (version-string &optional (error t))
  (if (stringp version-string)
      (let* ((first-hyphen (position #\- version-string))
             (first-plus (position #\+ version-string))
             (has-build-metadata-p (has-build-metadata-p first-hyphen first-plus))
             (has-pre-release-p (has-pre-release-p first-hyphen first-plus))
             version-number
             pre-release
             build-metadata)
        ;; Break out the separate pieces of the string
        (cond
          (has-pre-release-p
           (setf version-number (subseq version-string 0 first-hyphen)))
          (has-build-metadata-p
           (setf version-number (subseq version-string 0 first-plus)))
          (t
           (setf version-number version-string)))
        (when has-pre-release-p
          (setf pre-release (subseq version-string (1+ first-hyphen) first-plus)))
        (when has-build-metadata-p
          (setf build-metadata (subseq version-string (1+ first-plus))))
        (list (parse-dot-separated version-number)
              (parse-dot-separated pre-release)
              (parse-dot-separated build-metadata t)))
      (when error
        (error "~S is not a string" version-string))))

(defun unparse-version (version-list)
  (destructuring-bind (v pre build)
      version-list
    (format nil "~{~D~^.~}~@[-~{~A~^.~}~]~@[+~{~A~^.~}~]" v pre build)))

(defun integer-or-string< (thing1 thing2)
  "numeric always have lower precedence than strings"
  (cond
    ((and (stringp thing1) (stringp thing2))
     (string< thing1 thing2))
    ((stringp thing1)
     nil)
    ((stringp thing2)
     t)
    (t
     (< thing1 thing2))))

(defun semantic-version< (version1 version2)
  (cond
    ((null version1)
     (not (null version2)))
    ((null version2)
     nil)
    (t
     (destructuring-bind (v1 pre1 build1)
         (parse-semantic-version version1)
       (declare (ignore build1))
       (destructuring-bind (v2 pre2 build2)
           (parse-semantic-version version2)
         (declare (ignore build2))
         (or (uiop:lexicographic< #'< v1 v2)
             (and (equal v1 v2)
                  (uiop:lexicographic< #'integer-or-string< pre1 pre2))))))))

(defun semantic-version<= (version1 version2)
  (not (semantic-version< version2 version1)))

(defun version< (version-1 version-2)
  (check-type version-1 (or string list))
  (check-type version-2 (or string list)))

(defun version-spec-satisfied-p/semantic (spec version)
  (destructuring-bind (test . version-spec)
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

(defun version-spec-satisfied-p/simple-string (spec version)
  (destructuring-bind (test . version-spec)
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

(defun parse-version-specifier (v-spec)
  (list
   (switch (v-spec :test (lambda (x y)
                           (starts-with-subseq y x)))
     ("=="
      (cons '= (subseq v-spec 2)))
     ("="
      (cons '= (subseq v-spec 1)))
     ("!="
      (cons '/= (subseq v-spec 2)))
     (">="
      (cons '>= (subseq v-spec 2)))
     (">"
      (cons '> (subseq v-spec 1)))
     ("<="
      (cons '<= (subseq v-spec 2)))
     ("<"
      (cons '< (subseq v-spec 1)))
     (t
      (cons '= v-spec)))))

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
