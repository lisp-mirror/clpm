(uiop:define-package #:clpm/sandbox/firejail
    (:use #:cl
          #:clpm/cache
          #:clpm/sandbox/defs))

(in-package #:clpm/sandbox/firejail)

(defmethod sandbox-augment-command ((method (eql :firejail)) command
                                    &key read-write-pathnames)
  `("firejail"
    "--x11=none"
    "--net=none"
    "--private-tmp"
    "--read-only=/home"
    ,(format nil "--read-only=~A" (uiop:getenv "HOME"))
    ,@(loop
        :for place :in read-write-pathnames
        :for namestring := (namestring place)
        :collect (format nil "--read-write=~A" namestring))
    "--quiet"
    "--caps.drop=all"
    "--no3d"
    "--nodbus"
    "--nodvd"
    "--nonewprivs"
    "--noroot"
    "--nosound"
    "--novideo"
    "--private-dev"
    "--seccomp"
    "--shell=none"
    "--"
    ,@command))

(defmethod launch-program-in-sandbox ((sandbox-method (eql :firejail))
                                      sandbox-args
                                      command
                                      &rest args
                                      &key
                                      &allow-other-keys)
  (apply #'uiop:launch-program
         (apply #'sandbox-augment-command command sandbox-args)
         args))

(defmethod sandbox-available-p ((sandbox-method (eql :firejail)))
  (zerop
   (nth-value 2
              (uiop:run-program '("firejail" "--version")
                                :ignore-error-status t))))
