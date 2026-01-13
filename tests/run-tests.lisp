#!/usr/bin/env sbcl --script

;; Comprehensive test runner for Skyline-Tool
;; Runs all test suites to ensure functionality

(require :asdf)

(load (merge-pathnames "../setup.lisp" *load-pathname*))

(ql:quickload :fiveam)
;; Load the test ASD file
(asdf:load-asd (merge-pathnames "../skyline-tool.asd" *load-pathname*) :name :skyline-tool/test)
(ql:quickload :skyline-tool/test)

(defparameter *test-results* nil)

(format t "~%Running Skyline-Tool comprehensive test suite...~%")

(uiop:chdir (uiop:pathname-parent-directory-pathname
              (asdf:system-source-directory :skyline-tool)))

;; Run all test suites
(format t "~%Running action tests...~%")
(let ((result (fiveam:run! 'skyline-tool/test:action-tests)))
  (push (cons :action result) *test-results*))

(format t "~%Running animation preview tests...~%")
(let ((result (fiveam:run! 'skyline-tool/test:animation-preview-tests)))
  (push (cons :animation-preview result) *test-results*))

(format t "~%Running graphics tests...~%")
(let ((result (fiveam:run! 'skyline-tool/graphics-test:graphics-tests)))
  (push (cons :graphics result) *test-results*))

(format t "~%Running build regression tests...~%")
(let ((result (fiveam:run! 'skyline-tool/build-test:build-tests)))
  (push (cons :build result) *test-results*))

(format t "~%Running interface tests...~%")
(let ((result (fiveam:run! 'skyline-tool/interface-test:interface-tests)))
  (push (cons :interface result) *test-results*))


(format t "~%Running 5200-specific tests...~%")
(let ((result (fiveam:run! 'skyline-tool/5200-test:5200-tests)))
  (push (cons :5200 result) *test-results*))

;; Check results
(let ((all-passed t))
  (dolist (result *test-results*)
    (let ((suite-name (car result))
          (suite-result (cdr result)))
      (format t "~%~:(~A~) tests: ~A"
              suite-name
              (if suite-result "PASSED" "FAILED"))
      (unless suite-result
        (setf all-passed nil))))
  (format t "~%~%Test Summary: ~A suites run~%"
          (length *test-results*))
  (if all-passed
      (format t "All tests PASSED~%")
      (progn
        (format t "Some tests FAILED - see output above~%")
        (sb-ext:quit :unix-status 1))))
