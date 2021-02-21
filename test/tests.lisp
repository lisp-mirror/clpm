(in-package #:clpm-test)

(defun start-server ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                    :port 8091
                                    :document-root (asdf:system-relative-pathname :clpm "test/web-server/static/"))))

(defun stop-server (server)
  (hunchentoot:stop server))

(defun call-with-server-running (thunk)
  (let ((server (start-server)))
    (unwind-protect
         (funcall thunk)
      (stop-server server))))

(defmacro with-server (() &body body)
  `(call-with-server-running (lambda () ,@body)))

(defun run-tests-with-server (&key (clpm "clpm"))
  (let ((*clpm* (namestring clpm)))
    (with-server ()
      (fiveam:run! :clpm))))
