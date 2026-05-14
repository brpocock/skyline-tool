;; Comprehensive Skyline-Tool Converter Test Runner
;; Tests all converter functionality suites

;; Load ASDF and setup
(require :asdf)

;; Resolve SkylineTool/ and Phantasia project root from this script's location so the
;; runner works whether invoked as "sbcl --script tests/run-tests.lisp" from SkylineTool
;; or from the repo root (e.g. "sbcl --script SkylineTool/tests/run-tests.lisp").
(let* ((load-path (merge-pathnames *load-pathname* (uiop:getcwd)))
       (tests-dir (uiop:pathname-directory-pathname load-path))
       (skyline-tool-dir (uiop:pathname-parent-directory-pathname tests-dir))
       (project-root (uiop:pathname-parent-directory-pathname skyline-tool-dir)))
  (load (merge-pathnames "setup.lisp" skyline-tool-dir))
  (asdf:load-asd (merge-pathnames "skyline-tool.asd" skyline-tool-dir))
  (asdf:load-system :skyline-tool/test :force t)
  ;; Many tests read Source/Maps/... and similar paths relative to the game repo root.
  (uiop:chdir project-root)
  ;; After loading ASDF/setup, *default-pathname-defaults* may still point at SkylineTool/;
  ;; merge-pathnames/probe-file for relative paths use it, so align with cwd.
  (setf *default-pathname-defaults*
        (uiop:ensure-directory-pathname (uiop:getcwd)))
  ;; Set up the machine environment from PLATFORM (port label or machine number).
  ;; Intv makefiles pass PLATFORM=Intv; numeric strings still work (e.g. 2609).
  (let* ((platform (or (uiop:getenv "PLATFORM") "7800")))
    (if (every #'digit-char-p platform)
        (setf (symbol-value (find-symbol "*MACHINE*" (find-package :skyline-tool)))
              (parse-integer platform))
        (funcall (find-symbol "LOAD-PROJECT.JSON" (find-package :skyline-tool)) platform))
    (let ((all-passed
           (if (string-equal platform "Intv")
               (funcall (find-symbol "RUN-INTV-SKYLINE-TESTS" (find-package :skyline-tool/test)))
               (eval (read-from-string "(fiveam:run-all-tests :summary :end)")))))
      (if all-passed
          (progn
            (format t "~&All tests passed~%")
            (uiop:quit 0))
          (progn
            (format t "~&Tests failed~%")
            (uiop:quit 1))))))
