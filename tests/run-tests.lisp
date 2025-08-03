#!/usr/bin/env sbcl --script

;; Simple test runner for Skyline-Tool action tests
;; This avoids the make dependency issue by running tests in isolation

(require :asdf)

;; Load FiveAM via ASDF
(asdf:load-system :fiveam)

;; Load the test system
(asdf:load-system :skyline-tool/tests)

;; Run the action tests
(format t "~%Running Skyline-Tool action tests...~%")
(fiveam:run! 'skyline-tool::action-tests)
(format t "~%Tests completed.~%") 