(uiop:define-package #:clpm/client
    (:use #:cl
          #:clpm-asdf)
  (:export #:clpm-client-string))

(in-package #:clpm/client)

(defvar *client-contents* nil
  "When dumping to an image, the source files of the client are concatenated
together and stored in this variable.")

(defun read-client-contents ()
  "Compile the client (concatenate the source) and read it in. Returns it as a
string."
  (uiop:read-file-string (second (asdf:output-files 'concatenate-source-deliver-asd-op :clpm-client))))

(defun cache-client-contents ()
  (setf *client-contents* (read-client-contents)))
(uiop:register-image-dump-hook 'cache-client-contents)

(defun clpm-client-string ()
  "Returns a string with the contents of the client. Prefers the cache
~*client-contents*~."
  (or *client-contents*
      (read-client-contents)))
