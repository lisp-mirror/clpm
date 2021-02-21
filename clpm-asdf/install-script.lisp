;;;; Install script
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-asdf)

(defclass release-clpm-install-script (ops::release-file)
  ((type
    :initform "sh")))

(defmethod ops::release-component-class-by-type (parent (type (eql :clpm-install-script)))
  'release-clpm-install-script)

(defmethod ops::release-op-build-action ((o ops:release-stage-op) (c release-clpm-install-script))
  nil)

(defmethod asdf:input-files ((o ops:release-stage-op) (c release-clpm-install-script))
  (list (asdf:system-relative-pathname :clpm "install.sh")))

(defmethod asdf:output-files ((o ops:release-stage-op) (c release-clpm-install-script))
  (values
   (list (merge-pathnames "install.sh" (asdf:component-pathname c)))
   (ops::release-op-ignore-output-translations-p o c)))

(defmethod asdf:perform ((o ops:release-stage-op) (c release-clpm-install-script))
  (let ((input (first (asdf:input-files o c)))
        (output (asdf:output-file o c)))
    (funcall (uiop:find-symbol* :copy-file :asdf-release-ops) input output)))
