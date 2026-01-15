;;; Phantasia SkylineTool/tests/snes-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite snes-tests
  :description "Tests for SNES-specific SkylineTool functionality")

(in-suite snes-tests)

;; Test SNES music compilation functions
(test snes-music-compilation
  "Test SNES music compilation functions"
  (is-true (fboundp 'skyline-tool::compile-music-snes)
           "compile-music-snes should exist")
  ;; Currently just signals error, but shouldn't crash
  (signals error (skyline-tool::compile-music-snes
                   (format nil "Object/~a/test-~x.s" (skyline-tool::machine-directory-name) (sxhash (get-universal-time)))
                   (format nil "Object/~a/test-~x.mid" (skyline-tool::machine-directory-name) (sxhash (get-universal-time))))
           "compile-music-snes should signal error (not yet implemented)"))

;; Test SNES blob ripping functions
(test snes-blob-functions-existence
  "Test that SNES blob ripping functions exist"
  (is-true (fboundp 'skyline-tool::blob-rip-snes-tile)
           "blob-rip-snes-tile should exist")
  (is-true (fboundp 'skyline-tool::blob-rip-snes-sprite)
           "blob-rip-snes-sprite should exist")
  (is-true (fboundp 'skyline-tool::blob-rip-snes-font)
           "blob-rip-snes-font should exist"))

;; Test SNES art compilation
(test snes-art-compilation
  "Test SNES art compilation functions"
  (is-true (fboundp 'skyline-tool::compile-art-snes)
           "compile-art-snes should exist")
  ;; Test with invalid inputs
  (signals error (skyline-tool::compile-art-snes "/nonexistent.in"
                   (format nil "Object/~a/test-~x.out" (skyline-tool::machine-directory-name) (sxhash (get-universal-time))))
           "compile-art-snes should signal error for missing input"))

;; Test SNES blob ripping with invalid inputs
(test snes-blob-ripping-errors
  "Test SNES blob ripping error handling"
  (signals error (skyline-tool::blob-rip-snes-tile "/nonexistent.png")
           "blob-rip-snes-tile should signal error for missing files")
  (signals error (skyline-tool::blob-rip-snes-sprite "/nonexistent.png")
           "blob-rip-snes-sprite should signal error for missing files")
  (signals error (skyline-tool::blob-rip-snes-font "/nonexistent.png")
           "blob-rip-snes-font should signal error for missing files"))

;; Test SNES platform constants
(test snes-platform-constants
  "Test that SNES platform constants are properly defined"
  (is-true (member 6 skyline-tool::*valid-machines*)
           "SNES (machine 6) should be in valid machines list"))

;; Test SNES error conditions
(test snes-error-conditions
  "Test error handling in SNES functions"
  ;; Test music compilation with nil inputs
  (signals error (skyline-tool::compile-music-snes nil nil)
           "compile-music-snes should handle nil inputs")

  ;; Test art compilation with nil inputs
  (signals error (skyline-tool::compile-art-snes nil nil)
           "compile-art-snes should handle nil inputs"))

(defun run-snes-tests ()
  "Run all SNES tests and return results"
  (fiveam:run! 'snes-tests))
