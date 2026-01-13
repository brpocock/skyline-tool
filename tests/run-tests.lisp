#!/bin/bash

# Comprehensive Skyline-Tool test runner for all platforms and systems
# This runs ALL platform-specific tests and ensures complete coverage

echo "Running Skyline-Tool Comprehensive Test Suite"
echo "=============================================="

# Set up environment
export SKYLINE_DEBUG_BACKTRACE=t

# Run individual test files for all platforms
echo
echo "Running basic infrastructure tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/basic-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/basic-test::basic-tests)" --eval "(sb-ext:quit)"

echo
echo "Running core graphics tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/graphics-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/graphics-test::graphics-tests)" --eval "(sb-ext:quit)"

echo
echo "Running music and sound tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/music-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/music-test::music-tests)" --eval "(sb-ext:quit)"

echo
echo "Running speech synthesis tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/speech-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/speech-test::speech-tests)" --eval "(sb-ext:quit)"

echo
echo "Running build system tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/build-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/build-test::build-tests)" --eval "(sb-ext:quit)"

echo
echo "Running interface and CLI tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/interface-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/interface-test::interface-tests)" --eval "(sb-ext:quit)"

# Platform-specific tests
echo
echo "Running Atari 7800 tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/7800-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/7800-test::7800-tests)" --eval "(sb-ext:quit)"

echo
echo "Running Atari 5200 tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/5200-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/5200-test::5200-tests)" --eval "(sb-ext:quit)"

echo
echo "Running Atari Lynx tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/lynx-graphics-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/lynx-graphics-test::lynx-graphics-tests)" --eval "(sb-ext:quit)"

echo
echo "Running Game Boy Color tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/cgb-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/cgb-test::cgb-tests)" --eval "(sb-ext:quit)"

echo
echo "Running Game Boy tests..."
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
echo "Running Sega platform tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/sega-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/sega-test::sega-tests)" --eval "(sb-ext:quit)"

echo
echo "Running multiplatform tests..."
sbcl --eval "(require :asdf)" --eval "(load \"setup.lisp\")" --eval "(ql:quickload :fiveam)" --eval "(ql:quickload :skyline-tool/test)" --eval "(load \"tests/multiplatform-tests.lisp\")" --eval "(fiveam:run! 'skyline-tool/multiplatform-test::multiplatform-tests)" --eval "(sb-ext:quit)"

echo
echo "Note: Some platform tests are currently stub implementations."
echo "As implementations are completed, these tests will provide full validation."
echo
echo "Skyline-Tool comprehensive testing completed for all platforms!"
