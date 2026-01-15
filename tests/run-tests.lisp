;; Comprehensive Skyline-Tool Converter Test Runner
;; Tests all converter functionality suites

;; Load ASDF and setup
(require :asdf)

;; Load setup script (sets up Quicklisp and loads ASDF system)
(load "SkylineTool/setup.lisp")

;; Ensure ASDF can find the system
(asdf:load-asd "SkylineTool/skyline-tool.asd")

;; Load the test system components explicitly to ensure package definitions are available
(asdf:load-system :skyline-tool/test :force t)

;; Set up the machine environment from PLATFORM environment variable
(setf skyline-tool::*machine* (parse-integer (uiop:getenv "PLATFORM")))

;; Run all tests
(fiveam:run-all-tests)
