;;; Narrow Skyline-Tool runner: near-term port master Makefiles must parse under GNU make -n.
;;; Invoke with cwd SkylineTool (the script loads setup relative to this file, then chdir to repo root):
;;;   cd SkylineTool && SKYLINE_TEST_BACKTRACE=t PLATFORM=7800 sbcl --script tests/run-near-term-makefiles.lisp

(require :asdf)

(let* ((load-path (merge-pathnames *load-pathname* (uiop:getcwd)))
       (tests-dir (uiop:pathname-directory-pathname load-path))
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
  (let* ((pkg (find-package :skyline-tool/test))
         (names '(write-master-makefile-lynx-make-n-parses
                  write-master-makefile-intv-make-n-parses
                  write-master-makefile-5200-make-n-parses))
         (runner (find-symbol "RUN!" (find-package :fiveam)))
         (ok
          (every
           (lambda (sym)
             (let ((s (find-symbol (string sym) pkg)))
               (cond
                 ((not s)
                  (format *error-output* "~&Missing test ~a in package ~a~%" sym pkg)
                  nil)
                 (t
                  (format t "~&--- fiveam:run! ~a ---~%" s)
                  (funcall runner s)))))
           names)))
    (if ok
        (progn
          (format t "~&Near-term makefile parse tests passed.~%")
          (uiop:quit 0))
        (progn
          (format t "~&Near-term makefile parse tests FAILED.~%")
          (uiop:quit 1)))))
