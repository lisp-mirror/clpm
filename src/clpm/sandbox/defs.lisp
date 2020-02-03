;;;; Definitions for pluggable sandbox clients.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/sandbox/defs
    (:use #:cl
          #:alexandria
          #:anaphora
          #:clpm/config)
  (:export #:register-sandbox-client
           #:sandbox-augment-command
           #:%sandbox-augment-command
           #:sandbox-client-available-p))

(in-package #:clpm/sandbox/defs)

(defvar *all-sandbox-clients* nil
  "A list of all sandboxing clients loaded into the image. An alist that maps
a (keyword) name to a class name.")

(defvar *available-sandbox-clients* nil
  "A list of sandboxing clients that are loaded into the image *and* have all
their dependencies (such as external programs) met. An alist that maps
a (keyword) name to a client instance. Computed when necessary by
~available-sandbox-clients~.")

(defun register-sandbox-client (key class)
  "Register a new ~key~, ~class~ pair in ~*all-sandbox-clients*~."
  (pushnew (cons key class) *all-sandbox-clients* :test #'equal))

(defun clear-available-sandbox-clients ()
  "Clear the list of available sandbox clients."
  (setf *available-sandbox-clients* nil))

;; Make sure the available sandboxing clients are cleared on image dump.
(uiop:register-clear-configuration-hook 'clear-available-sandbox-clients)

(defun make-sandbox-client (pair)
  "Given a name/class pair, instantiate the class, using any setting specified
by the user's config."
  (destructuring-bind (key . class)
      pair
    (apply #'make-instance class
           (awhen (config-value :sandbox-client key)
             (hash-table-plist it)))))

(defun compute-available-sandbox-clients ()
  "Compute an alist suitable for storing in
~*available-sandbox-clients*~. Instantiate all registered sandbox clients, then
remove the ones where ~sandbox-client-available-p~ returns NIL."
  (let ((client-list (mapcar (lambda (x)
                               (cons (car x) (make-sandbox-client x)))
                             *all-sandbox-clients*)))
    (remove-if-not #'sandbox-client-available-p client-list :key #'cdr)))

(defun available-sandbox-clients ()
  "Return an alist of all available sandbox clients. Caches results in
~*available-sandbox-clients*~."
  (unless *available-sandbox-clients*
    (setf *available-sandbox-clients* (compute-available-sandbox-clients)))
  *available-sandbox-clients*)

(defun get-preferred-sandbox-client ()
  "Return the sandbox client instance that is available and most preferred."
  (let* ((client-key (config-value :grovel :sandbox :type))
         (available-sandbox-clients (available-sandbox-clients))
         (client (if (eql :auto client-key)
                     (cdr (first available-sandbox-clients))
                     (assoc-value available-sandbox-clients client-key))))
    client))


;; * Sandbox Client API

(defgeneric sandbox-client-available-p (client)
  (:documentation
   "Returns T iff ~client~ is able to be used to sandbox."))

(defgeneric %sandbox-augment-command (sandbox-client command &key read-write-pathnames)
  (:documentation
   "Given a ~command~ as a list of strings, return a new command list that runs
the command in a sandbox using ~sandbox-client~."))


;; * Sandboxing processes

(defun sandbox-augment-command (command &key read-write-pathnames)
  "Given a ~command~ as a list of strings, return a new command list that runs
the command in a sandbox using the preferred sandbox client."
  (let ((client (get-preferred-sandbox-client)))
    (if client
        (%sandbox-augment-command client
                                  command
                                  :read-write-pathnames read-write-pathnames)
        command)))
