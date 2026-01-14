;; Comprehensive Skyline-Tool Converter Test Runner
;; Tests all converter functionality suites

;; Make sure *MACHINE* is set for the test system
(setf skyline-tool:*machine* (or skyline-tool:*machine* 7800))
(format t "~&Starting Skyline-Tool test suite with *MACHINE* = ~A...~%" skyline-tool:*machine*)

;; Run tests via ASDF test-op
(asdf:test-system :skyline-tool/test)
