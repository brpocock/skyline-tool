#!/bin/bash
# Run animation preview tests for Skyline-Tool

cd "$(dirname "$0")/.."

sbcl --noinform --disable-debugger \
     --eval "(require :asdf)" \
     --eval "(load \"setup.lisp\")" \
     --eval "(ql:quickload :skyline-tool/test :silent t)" \
     --eval "(format t \"~%~%Running Animation Preview Tests...~%~%\")" \
     --eval "(fiveam:run! 'skyline-tool/test:animation-preview-tests)" \
     --eval "(format t \"~%~%Tests completed.~%\")" \
     --quit
