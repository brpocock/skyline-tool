;; Minimal test to check just our functions
(require 'asdf)

;; Load just the packages we need
(asdf:load-system :cl-ppcre)
(asdf:load-system :split-sequence)
(asdf:load-system :png-read)

;; Load the skyline-tool package definitions
(load "src/package.lisp")

;; Load just the functions we need
(load "src/misc.lisp")
(load "src/asset-allocator.lisp")
(load "src/graphics.lisp")
(load "src/music.lisp")
(load "src/atarivox.lisp")

(format t "Testing function existence...~%")

;; Check if our functions exist
(format t "compile-speech-2609: ~A~%" (fboundp 'skyline-tool::compile-speech-2609))
(format t "compile-art-intv: ~A~%" (fboundp 'skyline-tool::compile-art-intv))
(format t "assemble-intv-rom: ~A~%" (fboundp 'skyline-tool::assemble-intv-rom))

(format t "All checks complete.~%")
(sb-ext:exit)
