;;;; clpm output-translations
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm-cli/commands/output-translations
    (:use #:cl
          #:alexandria
          #:clpm-cli/common-args
          #:clpm-cli/interface-defs)
  (:import-from #:adopt)
  (:import-from #:clpm))

(in-package #:clpm-cli/commands/output-translations)

(defparameter *output-translations-ui*
  (adopt:make-interface
   :name "clpm output-translations"
   :summary "Common Lisp Project Manager Output-Translations"
   :usage "output-translations [options]"
   :help "Print an ASDF output-translations form using the projects installed in a context"
   :contents (list *group-common*
                   *option-context*)))

(define-cli-command (("output-translations") *output-translations-ui*) (args options)
  (declare (ignore options args))
  (when-let ((output-translations (clpm:output-translations)))
    (with-standard-io-syntax
      (let ((*print-case* :downcase))
        (prin1 output-translations))
      (terpri)))
  t)
