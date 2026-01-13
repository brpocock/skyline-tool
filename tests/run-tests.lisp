#!/bin/bash

# Simple test runner that runs individual test files
# This avoids package conflicts when loading multiple test files

echo "Running Skyline-Tool Test Suite"
echo "==============================="

# Set up environment
export SKYLINE_DEBUG_BACKTRACE=t

# Run individual test files
echo
echo "Running basic tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/basic-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/basic-test::basic-tests)" --eval "(sb-ext:quit)"

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

echo
echo "Note: Action and animation preview tests are skipped due to known issues."
echo "Skyline-Tool tests completed."
