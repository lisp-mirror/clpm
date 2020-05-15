;;;; Definitions for CLPM configuration options.
;;;;
;;;; This software is part of CLPM. See README.org for more information. See
;;;; LICENSE for license information.

(uiop:define-package #:clpm/config/defs
    (:use #:cl
          #:alexandria)
  (:export #:config-path-key-types
           #:get-all-config-paths-with-prefix
           #:get-children-of-config-path
           #:get-config-entry
           #:header
           #:hostname
           #:parse-string-config-value))

(in-package #:clpm/config/defs)

(defparameter *config-info*
  `((()
     :type hash-table)

    ((:archives)
     :type hash-table)
    ((:archives :tar)
     :type hash-table)
    ((:archives :tar :type)
     :type (member :auto :tar)
     :default :auto
     :documentation
     "The tar implementation to use.")

    ((:bundle)
     :type hash-table)
    ((:bundle :clpmfile)
     :type (or string pathname)
     :default "clpmfile"
     :documentation
     "Path to the clpmfile to use when running bundle commands. Relative pathnames are resolved relative to the current directory.")
    ((:bundle :local)
     :type hash-table)
    ((:bundle :local :*)
     :wildcard-types (string)
     :type (or string pathname)
     :documentation
     "Path to a folder to use for a project's git repository instead of a CLPM managed one. Relative pathnames are resolved with respect to the location of the clpmfile.")
    ((:bundle :output-translation)
     :type (member t nil :local)
     :default t
     :documentation
     "Controls whether `bundle exec` configures ASDF's output translations. NIL corresponds to not configuring the output translations. T (default) configures the output translations to translate everything to a folder in CLPM's cache. :LOCAL translates everything into the `./.clpm/fasl-cache/` folder relative to the clpmfile.")

    ((:context)
     :type string
     :default "default"
     :documentation
     "The name of the default context.")

    ((:contexts)
     :type hash-table)
    ((:contexts :*)
     :wildcard-types (string)
     :type hash-table)
    ((:contexts :* :ignore-inherited-source-registry)
     :type boolean
     :default nil
     :documentation
     "Controls whether the source registry for this context ignores inherited configuration.")
    ((:contexts :* :output-translation)
     :type boolean
     :default t
     :documentation
     "Controls whether output translations are used for this context. NIL corresponds to not configuring the output translations. T (default) configures the output translations to translate everything to a folder in CLPM's cache.")
    ((:contexts :* :source-registry.d-files)
     :wildcard-types (string)
     :type (list (or string pathname))
     :documentation
     "Used to inform ASDF where to find systems for this context. Outputs the same contents to every file in a format suitable for inclusion in a source-registry.d directory (see ASDF manual).")
    ((:contexts :* :source-registry-files)
     :wildcard-types (string)
     :type (list (or string pathname))
     :documentation
     "Used to inform ASDF where to find systems for this context. Outputs the same contents to every file in a source-registry.conf format (see ASDF manual).")
    ((:contexts :* :sources)
     :wildcard-types (string)
     :type (or (list string) (eql t))
     :default t
     :documentation
     "A list of global source names that this context can use, or T to represent all global sources.")

    ((:curl)
     :type hash-table)
    ((:curl :path)
     :type (or string pathname)
     :default "curl"
     :documentation
     "The path to the curl executable.")

    ((:dexador)
     :type hash-table)

    ((:drakma)
     :type hash-table)

    ((:firejail)
     :type hash-table)
    ((:firejail :path)
     :type (or string pathname)
     :default "firejail"
     :documentation
     "The path to the firejail executable.")

    ((:git)
     :type hash-table)
    ((:git :path)
     :type (or string pathname)
     :default "git"
     :documentation
     "The path to the git executable.")
    ((:git :remotes)
     :type hash-table)
    ((:git :remotes :*)
     :wildcard-types (hostname)
     :type hash-table)
    ((:git :remotes :* :type)
     :wildcard-types (hostname)
     :type (member :github :gitlab)
     :documentation
     "The type of git server hosted at this hostname.")
    ((:git :remotes :* :method)
     :wildcard-types (hostname)
     :type (member :ssh :https)
     :documentation
     "The preferred method of connecting to this server.")
    ((:git :remotes :* :username)
     :wildcard-types (hostname)
     :type string
     :default "git"
     :documentation
     "The username to use when connecting to this server.")
    ((:git :remotes :* :password)
     :wildcard-types (hostname)
     :type string
     :documentation
     "The password to use when connecting to this server. Only used in HTTPS method.")

    ((:grovel)
     :type hash-table)
    ((:grovel :lisp)
     :type hash-table)
    ((:grovel :lisp :implementation)
     :type keyword
     :default :auto
     :documentation
     "The implementation to use when groveling. Must be recognized by
      lisp-invocation library.")
    ((:grovel :lisp :path)
     :type string
     :documentation
     "The command to use when executing the Lisp process for groveling.")
    ((:grovel :sandbox)
     :type hash-table)
    ((:grovel :sandbox :type)
     :type (member :auto :firejail :none)
     :default :auto
     :documentation
     "The sandbox type to use when groveling.")

    ((:http)
     :type hash-table)
    ((:http :headers)
     :type hash-table)
    ((:http :headers :*)
     :wildcard-types (hostname)
     :type hash-table)
    ((:http :headers :* :*)
     :wildcard-types (hostname header)
     :type hash-table)
    ((:http :headers :* :* :secure-only-p)
     :wildcard-types (hostname header)
     :type boolean
     :default nil
     :documentation
     "If non-NIL, then the specified header should only be sent to hostname if the connection is secure.")
    ((:http :headers :* :* :contents)
     :wildcard-types (hostname header)
     :type (or string pathname)
     :default nil
     :documentation
     "If set, the contents of this file are sent as the header value.")
    ((:http :headers :* :* :exec)
     :wildcard-types (hostname header)
     :type (or string pathname)
     :documentation
     "If set, it must name a program that must print the value of the header to stdout.")
    ((:http :headers :* :* :value)
     :wildcard-types (hostname header)
     :type string
     :documentation
     "If set, the value of the header to send.")

    ((:http-client)
     :type hash-table)
    ((:http-client :type)
     :type (member :auto
                   ,@(when (featurep :clpm-curl) (list :curl))
                   ,@(when (featurep :clpm-dexador) (list :dexador))
                   ,@(when (featurep :clpm-drakma) (list :drakma)))
     :default :auto
     :documentation
     "The HTTP client implementation to use.")

    ((:local)
     :type boolean
     :default nil
     :documentation
     "If T, sources do not download metadata from the internet. Currently, they
     may still download tarballs from the internet, but this may change in a
     future version.")

    ((:log)
     :type hash-table)
    ((:log :level)
     :type (member :off :fatal :error :warn :info :debug :trace)
     :default :warn
     :documentation
     "The default logging level of the application.")

    ((:tar)
     :type hash-table)
    ((:tar :path)
     :type (or string pathname)
     :default "tar"
     :documentation
     "The path to the tar executable.")))

(defun paths-match (canonical-path path)
  (and (length= canonical-path path)
       (every (lambda (left right)
                (or (eql left :*)
                    (equal left right)))
              canonical-path path)))

(defun get-config-entry (path)
  (find-if (rcurry #'paths-match path) *config-info* :key #'car))

(defun get-children-of-config-path (path)
  (let* ((config-info (get-config-entry path))
         (type (getf (cdr config-info) :type))
         (canonical-path (car config-info)))
    (assert config-info)
    (assert (eql type 'hash-table))
    (loop
      :for info :in *config-info*
      :for info-path := (car info)
      :when (and (starts-with-subseq canonical-path info-path)
                 (length= (1+ (length path)) info-path))
        :collect (last-elt info-path))))

(defun get-all-config-paths-with-prefix (path)
  "Returns a list of all paths to values (NOT subpaths to hash-tables) that
start with `PATH`."
  (let* ((config-info (get-config-entry path))
         (type (getf (cdr config-info) :type))
         (canonical-path (car config-info)))
    (assert config-info)
    (assert (eql type 'hash-table))
    (loop
      :for info :in *config-info*
      :for info-path := (car info)
      :when (and (not (eql (getf (cdr info) :type) 'hash-table))
                 (starts-with-subseq canonical-path info-path))
        :collect (append path (nthcdr (length path) info-path)))))

(defun sub-paths (path)
  "Return all sub paths of `PATH` (and the path itself)."
  (loop
    :for i :below (length path)
    :collect (butlast path i)))

(defun config-path-key-types (path)
  "Given a `PATH`, return a list of all the types of keys on the
path. Everything is a `:keyword` except for wildcards."
  (loop
    :for p :in (nreverse (sub-paths path))
    :for config-info := (get-config-entry p)
    :for canonical-path := (car config-info)
    :if (eql (last-elt canonical-path) :*)
      :collect (last-elt (getf (cdr config-info) :wildcard-types))
    :else
      :collect :keyword))

(defun parse-string-config-value (value type)
  (cond
    ((eql type 'string)
     value)
    ((eql type 'boolean)
     (let ((value (string-downcase value))
           (orig-value value))
       (cond
         ((or (equalp value "0")
              (equalp value "n")
              (equalp value "no")
              (equalp value "false")
              (equalp value "nil"))
          nil)
         ((or (equalp value "1")
              (equalp value "y")
              (equalp value "yes")
              (equalp value "true")
              (equalp value "t"))
          t)
         (t
          (error "Unable to parse ~S as a boolean." orig-value)))))
    ((eql type 'keyword)
     (make-keyword (uiop:standard-case-symbol-name value)))
    ((and (listp type)
          (eql (first type) 'member)
          (every (lambda (x) (or (keywordp x) (eql x nil) (eql x t))) (rest type)))
     (cond
       ((equalp value "t")
        t)
       ((equalp value "nil")
        nil)
       (t
        (let ((kw (make-keyword (uiop:standard-case-symbol-name value))))
          (unless (typep kw type)
            (error "Unknown value ~S for type ~S" kw type))
          kw))))
    ((equal type '(or string pathname))
     (uiop:parse-native-namestring value))
    (t
     (error "Unknown type ~S to parse from a string." type))))
