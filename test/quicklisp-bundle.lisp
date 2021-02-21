(in-package #:clpm-test)

(test (quicklisp-bundle :suite :clpm)
  (uiop:with-current-directory ((asdf:system-relative-pathname :clpm "test/quicklisp-bundle/"))
    (finishes
      (uiop:run-program `(,*clpm* "bundle" "install" "-y" "--no-resolve")
                        :output :interactive
                        :error-output :interactive)
      (uiop:run-program `(,*clpm* "bundle" "exec" "--" "sbcl" "--non-interactive" "--eval" "(asdf:load-system :quicklisp-bundle)" "--quit")
                        :output :interactive
                        :error-output :interactive))))
