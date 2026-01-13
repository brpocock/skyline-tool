#!/bin/bash

# Skyline-Tool Comprehensive Test Runner
# Tests all platform support and functionality

echo "Running Skyline-Tool Comprehensive Test Suite"
echo "=============================================="

# Set up environment
export SKYLINE_DEBUG_BACKTRACE=t

echo
echo "Running basic tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/basic-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/basic-test::basic-tests)" --eval "(sb-ext:quit)"

echo
echo "Running 5200 tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/5200-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/5200-test::5200-tests)" --eval "(sb-ext:quit)"

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
echo "Running CGB tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/cgb-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/cgb-test::cgb-tests)" --eval "(sb-ext:quit)"

echo
echo "Running DMG tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/dmg-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/dmg-test::dmg-tests)" --eval "(sb-ext:quit)"

echo
echo "Running NES tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/nes-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/nes-test::nes-tests)" --eval "(sb-ext:quit)"

echo
echo "Running SNES tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/snes-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/snes-test::snes-tests)" --eval "(sb-ext:quit)"

echo
echo "Running ColecoVision tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/colecovision-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/colecovision-test::colecovision-tests)" --eval "(sb-ext:quit)"

echo
echo "Running multiplatform tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/multiplatform-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/multiplatform-test::multiplatform-tests)" --eval "(sb-ext:quit)"

echo
echo "All tests completed!"
echo "Note: Many tests currently signal 'unimplemented' errors as placeholders."
echo "These will be implemented as the platform support is developed."
