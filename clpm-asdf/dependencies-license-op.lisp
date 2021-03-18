;;;; License Operations
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-asdf)

(ops:define-build-op bundled-licenses-file-op (asdf:selfward-operation)
  ())

(defmethod asdf:selfward-operation ((o bundled-licenses-file-op))
  (ops:matching-variant-of o 'ops:program-system-list-op))

(defmethod ops:release-system-dependencies-license-op (o (s clpm-system))
  (ops:matching-variant-of o 'bundled-licenses-file-op))

(defmethod asdf:output-files ((o bundled-licenses-file-op) c)
  (list (uiop:subpathname (ops::build-op-output-directory-pathname o c)
                          "BUNDLED-LICENSES")))

(defmethod asdf:perform ((o bundled-licenses-file-op) c)
  (perform-dependencies-license-op o c))

(defun write-licenses-to-file (pn system-list)
  (with-open-file (stream pn
                          :direction :output
                          :if-exists :supersede)
    (format stream "CLPM contains code from multiple other projects.
This file lists the bundled dependencies and their licenses.~%~%")

    (dolist (project-name system-list)
      (let* ((project-license (asdf:system-relative-pathname :clpm-asdf
                                                             (uiop:strcat "licenses/" project-name)))
             (project-notice (merge-pathnames (make-pathname :type "NOTICE")
                                              project-license)))
        (format stream "
===============================================================================
")
        (format stream "CLPM contains code from the ~A project~%~%" project-name)

        (when (probe-file project-notice)
          (with-open-file (notice project-notice)
            (format stream "The following is the project's NOTICE file:~%~%")
            (uiop:copy-stream-to-stream notice stream))
          (terpri stream)
          (terpri stream))

        (format stream "It is licensed under the following terms:~%~%")

        (with-open-file (license project-license)
          (uiop:copy-stream-to-stream license stream)))
      (terpri stream))))

(defun massage-loaded-systems (systems)
  (sort (list*
         "openssl"
         "sbcl"
         (set-difference
          (remove-duplicates (mapcar #'asdf:primary-system-name systems)
                             :test #'equal)
          '( ;; Covered by the ASDF project.
            "asdf-package-system"
            "uiop"
            ;; First party code.
            "clpm-cli"
            "clpm-asdf"
            "clpm"
            ;; Covered by clpm-multi-http-client
            "clpm-multi-http-client-impl"
            ;; Covered by fare-quasiquote
            "fare-quasiquote-extras"
            "fare-quasiquote-optima"
            "fare-quasiquote-readtable"
            ;; Covered by trivia
            "trivia.balland2006"
            "trivia.level0"
            "trivia.level1"
            "trivia.level2"
            "trivia.quasiquote"
            "trivia.trivial"
            ;; Covered by sbcl
            "sb-bsd-sockets"
            "sb-cltl2"
            "sb-posix"
            "sb-rotate-byte")
          :test #'equal))
        #'string<))

(defun perform-dependencies-license-op (o c)
  (let* ((system-list-file (first (asdf:input-files o c)))
         (all-loaded-systems (uiop:with-safe-io-syntax () (uiop:read-file-form system-list-file))))
    (write-licenses-to-file (asdf:output-file o c)
                            (massage-loaded-systems (mapcar #'first all-loaded-systems)))))
