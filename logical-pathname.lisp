(in-package :cl-user)

;; Configure our logical pathnames.
;;
;; When using SLIME, LOAD this file (C-c C-l), not COMPILE and LOAD (C-c C-k).
(let ((clpm-root (uiop:pathname-directory-pathname (uiop:ensure-absolute-pathname *load-pathname*))))
  (setf (logical-pathname-translations "clpm")
        `(("clpm:clpm;**;*.*.*" ,(merge-pathnames "clpm/**/*.*" clpm-root))
          ("clpm:cli;**;*.*.*" ,(merge-pathnames "cli/**/*.*" clpm-root))
          ("clpm:features;**;*.*.*" ,(merge-pathnames "features/**/*.*" clpm-root)))))

(pushnew :clpm-logical-pathnames *features*)
