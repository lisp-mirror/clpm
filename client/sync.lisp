;;;; Interface to clpm sync
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(in-package #:clpm-client)

(defun sync (&key sources)
  "Sync a set of SOURCES. If no sources are listed, all are synced."
  (with-clpm-proc (proc)
    (clpm-proc-print
     proc
     `(progn
        (sync :sources ',(ensure-list sources))
        t))
    (clpm-proc-read proc)
    (values)))
