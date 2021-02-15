;;;; Building the client
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-asdf)

(defclass release-clpm-client-directory (ops::release-module)
  ())

(defmethod ops::release-component-class-by-type (parent (type (eql :clpm-client-directory)))
  'release-clpm-client-directory)

(defmethod asdf:input-files ((o ops:release-stage-op) (c release-clpm-client-directory))
  (list*
   (asdf:system-source-file :clpm-client)
   (asdf:input-files 'asdf:concatenate-source-op :clpm-client)))

(defmethod asdf:output-files ((o ops:release-stage-op) (c release-clpm-client-directory))
  (let* ((input-files (asdf:input-files o c))
         (base-input-directory (asdf:system-source-directory :clpm-client))
         (enough-input-files (mapcar (lambda (pn) (uiop:enough-pathname pn base-input-directory))
                                     input-files))
         (base-output-directory (asdf:component-pathname c)))
    (values
     (mapcar (lambda (pn) (merge-pathnames pn base-output-directory))
             enough-input-files)
     (ops::release-op-ignore-output-translations-p o c))))

(defmethod asdf:perform ((o ops:release-stage-op) (c release-clpm-client-directory))
  (let ((input-files (asdf:input-files o c))
        (output-files (asdf:output-files o c)))
    (loop
      :for in :in input-files
      :for out :in output-files
      :do (funcall (uiop:find-symbol* :copy-file :asdf-release-ops) in out))))
