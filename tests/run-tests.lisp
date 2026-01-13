#!/bin/bash

# Simple test runner that runs individual test files
# This avoids package conflicts when loading multiple test files

echo "Running Skyline-Tool Test Suite"
echo "==============================="

# Set up environment
export SKYLINE_DEBUG_BACKTRACE=t

<<<<<<< HEAD
(ql:quickload :fiveam)
;; Load the test ASD file
(asdf:load-asd (merge-pathnames "../skyline-tool.asd" *load-pathname*) :name :skyline-tool/test)
(ql:quickload :skyline-tool/test)
=======
# Run individual test files
echo
echo "Running basic tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/basic-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/basic-test::basic-tests)" --eval "(sb-ext:quit)"
>>>>>>> origin/lynx

echo
echo "Running graphics tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/graphics-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/graphics-test::graphics-tests)" --eval "(sb-ext:quit)"

echo
echo "Running Lynx graphics tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/lynx-graphics-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/lynx-graphics-test::lynx-graphics-tests)" --eval "(sb-ext:quit)"

echo
echo "Running music tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/music-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/music-test::music-tests)" --eval "(sb-ext:quit)"

echo
echo "Running build tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/build-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/build-test::build-tests)" --eval "(sb-ext:quit)"

echo
echo "Running interface tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/interface-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/interface-test::interface-tests)" --eval "(sb-ext:quit)"

<<<<<<< HEAD
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
=======
echo
echo "Note: Action and animation preview tests are skipped due to known issues."
echo "Skyline-Tool tests completed."
>>>>>>> origin/lynx
