;;;; clpm context output-translations
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/context/output-translations
    (:use #:cl
          #:alexandria
          #:clpm/context
          #:clpm-cli/context/common
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt))

(in-package #:clpm-cli/context/output-translations)

(defparameter *context-output-translations-ui*
  (adopt:make-interface
   :name "clpm context output-translations"
   :summary "Common Lisp Package Manager Context Output-Translations"
   :usage "context output-translations [options] CONTEXT-NAME"
   :help "Print an ASDF output-translations form using the projects installed in a context"
   :contents (list *group-common*)))

(define-cli-command (("context" "output-translations") *context-output-translations-ui*) (args options)
  (declare (ignore options))
  (assert (length= 1 args))
  (when-let ((output-translations (context-output-translations (first args))))
    (with-standard-io-syntax
      (let ((*print-case* :downcase))
        (prin1 output-translations))
      (terpri)))
  t)
