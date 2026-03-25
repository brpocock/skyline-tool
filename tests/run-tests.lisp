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
  ;; Set up the machine environment from PLATFORM (defaults to 7800 = Atari 7800 / main tree).
  ;; Use FIND-SYMBOL so this file can be read before :skyline-tool exists (package-qualified
  ;; symbols would be resolved at read time and fail).
  ;; SBCL: (find-package "skyline-tool") is NIL because package names are uppercase;
  ;; use (find-package :skyline-tool) so the name matches.
  (setf (symbol-value (find-symbol "*MACHINE*" (find-package :skyline-tool)))
        (parse-integer (or (uiop:getenv "PLATFORM") "7800")))
  ;; Run all tests; exit 1 if any suite fails (same idea as run-skyline-tests in interface.lisp).
  ;; READ-FROM-STRING at runtime so FIVEAM is not resolved while loading this file.
  (let ((all-passed (eval (read-from-string "(fiveam:run-all-tests :summary :end)"))))
    (if all-passed
        (progn
          (format t "~&All tests passed~%")
          (uiop:quit 0))
        (progn
          (format t "~&Tests failed~%")
          (uiop:quit 1)))))
