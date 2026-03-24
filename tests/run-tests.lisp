;; Comprehensive Skyline-Tool Converter Test Runner
;; Tests all converter functionality suites

;; Load ASDF and setup
(require :asdf)

;; Load setup script (sets up Quicklisp and loads ASDF system)
(load "setup.lisp")

;; Ensure ASDF can find the system
(asdf:load-asd "SkylineTool/skyline-tool.asd")

;; Load the test system components explicitly to ensure package definitions are available
(asdf:load-system :skyline-tool/test :force t)

;; Set up the machine environment from PLATFORM (defaults to 7800 = Atari 7800 / main tree).
(setf skyline-tool::*machine*
      (parse-integer (or (uiop:getenv "PLATFORM") "7800")))

;; Run all tests; exit 1 if any suite fails (same idea as run-skyline-tests in interface.lisp).
(let ((all-passed (fiveam:run-all-tests :summary :end)))
  (if all-passed
      (progn
        (format t "~&All tests passed~%")
        (uiop:quit 0))
      (progn
        (format t "~&Tests failed~%")
        (uiop:quit 1))))
