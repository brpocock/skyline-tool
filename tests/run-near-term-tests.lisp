;;; Run Near-term makefile / toolchain regression subset (SkylineTool test package).
(require :asdf)
(let* ((tests-dir (uiop:pathname-directory-pathname
                   (merge-pathnames *load-pathname* (uiop:getcwd))))
       (skyline-tool-dir (uiop:pathname-parent-directory-pathname tests-dir))
       (project-root (uiop:pathname-parent-directory-pathname skyline-tool-dir)))
  (load (merge-pathnames "setup.lisp" skyline-tool-dir))
  (asdf:load-asd (merge-pathnames "skyline-tool.asd" skyline-tool-dir))
  (asdf:load-system :skyline-tool/test :force t)
  (uiop:chdir project-root)
  (setf *default-pathname-defaults*
        (uiop:ensure-directory-pathname (uiop:getcwd)))
  (setf (symbol-value (find-symbol "*MACHINE*" (find-package :skyline-tool)))
        (parse-integer (or (uiop:getenv "PLATFORM") "7800")))
  (let ((ok (eval (read-from-string
                  "(skyline-tool/test:run-near-term-makefile-parse-tests)"))))
    (if ok
        (progn (format t "~&Near-term makefile parse tests passed.~%")
               (uiop:quit 0))
        (progn (format t "~&Near-term makefile parse tests FAILED.~%")
               (uiop:quit 1)))))
