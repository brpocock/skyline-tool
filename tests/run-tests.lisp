#!/usr/bin/env sbcl --script

;; Comprehensive test runner for Skyline-Tool
;; Runs all test suites to ensure functionality

(require :asdf)

(load (merge-pathnames "../setup.lisp" *load-pathname*))

(ql:quickload :fiveam)
(ql:quickload :skyline-tool/test)
(ql:quickload :skyline-tool/graphics-test)
(ql:quickload :skyline-tool/build-test)
(ql:quickload :skyline-tool/interface-test)

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

;; Check results
(let ((all-passed t))
  (dolist (result *test-results*)
    (let ((suite-name (car result))
          (suite-result (cdr result)))
      (let ((passed (fiveam:test-passed-p suite-result)))
        (format t "~%~:(~A~) tests: ~A"
                suite-name
                (if passed "PASSED" "FAILED"))
        (unless passed
          (setf all-passed nil)))))
  (format t "~%~%Test Summary: ~A suites run~%"
          (length *test-results*))
  (if all-passed
      (format t "All tests PASSED~%")
      (progn
        (format t "Some tests FAILED - see output above~%")
        (sb-ext:exit :code 1)))) 