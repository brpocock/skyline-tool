#!/usr/bin/env sbcl --script

;; Simple test runner for Skyline-Tool action tests
;; This avoids the make dependency issue by running tests in isolation

(require :asdf)

(load "setup.lisp")

(ql:quickload :fiveam)
(ql:quickload :skyline-tool/test)

(format t "~%Running Skyline-Tool action tests...~%")
(fiveam:run! 'skyline-tool/test:action-tests)
(format t "~%Tests completed.~%") 