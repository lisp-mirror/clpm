(uiop:define-package #:clpm/cache
    (:use #:cl)
  (:export #:clpm-cache))

(in-package #:clpm/cache)

(defun clpm-cache (x &key ensure-directory)
  (uiop:resolve-absolute-location
   (list*
    (or (uiop:getenv-absolute-directory "CLPM_CACHE")
        (uiop:xdg-cache-home "clpm/"))
    x)
   :ensure-directory ensure-directory))
