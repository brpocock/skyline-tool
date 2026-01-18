;;; Phantasia SkylineTool/tests/colecovision-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite colecovision-tests
  :description "Tests for ColecoVision-specific SkylineTool functionality")

(in-suite colecovision-tests)

;; Test ColecoVision music compilation functions
(test colecovision-music-compilation
  "Test ColecoVision music compilation functions"
  (is-true (fboundp 'skyline-tool::compile-music-colecovision)
           "compile-music-colecovision should exist")
  ;; Test with invalid inputs
  (signals error (skyline-tool::compile-music-colecovision
                   (format nil "Object/~a/test-~x.s" (skyline-tool::machine-directory-name) (sxhash (get-universal-time)))
                   "/nonexistent.mid")
           "compile-music-colecovision should signal error for missing MIDI file"))

;; Test ColecoVision art compilation
(test colecovision-art-compilation
  "Test ColecoVision art compilation functions"
  (is-true (fboundp 'skyline-tool::compile-art-colecovision)
           "compile-art-colecovision should exist")
  ;; Test with invalid inputs
  (signals error (skyline-tool::compile-art-colecovision "/nonexistent.in"
                   (format nil "Object/~a/test-~x.out" (skyline-tool::machine-directory-name) (sxhash (get-universal-time))))
           "compile-art-colecovision should signal error for missing input"))

;; Test ColecoVision blob ripping functions
(test colecovision-blob-functions-existence
  "Test that ColecoVision blob ripping functions exist"
  (is-true (fboundp 'skyline-tool::blob-rip-colecovision-tile)
           "blob-rip-colecovision-tile should exist")
  (is-true (fboundp 'skyline-tool::blob-rip-colecovision-sprite)
           "blob-rip-colecovision-sprite should exist")
  (is-true (fboundp 'skyline-tool::blob-rip-colecovision-font)
           "blob-rip-colecovision-font should exist")
  (is-true (fboundp 'skyline-tool::detect-colecovision-tile-mode)
           "detect-colecovision-tile-mode should exist"))

;; Test ColecoVision blob ripping with invalid inputs
(test colecovision-blob-ripping-errors
  "Test ColecoVision blob ripping error handling"
  (signals error (skyline-tool::blob-rip-colecovision-tile "/nonexistent.png")
           "blob-rip-colecovision-tile should signal error for missing files")
  (signals error (skyline-tool::blob-rip-colecovision-sprite "/nonexistent.png")
           "blob-rip-colecovision-sprite should signal error for missing files")
  (signals error (skyline-tool::blob-rip-colecovision-font "/nonexistent.png")
           "blob-rip-colecovision-font should signal error for missing files"))

;; Test ColecoVision tile mode detection
(test colecovision-tile-mode-detection
  "Test ColecoVision tile mode detection"
  ;; Test with mock data
  (is-true (skyline-tool::detect-colecovision-tile-mode (make-array '(8 8) :element-type '(unsigned-byte 32)))
           "detect-colecovision-tile-mode should return truthy value for valid input")

  ;; Test with nil input
  (signals error (skyline-tool::detect-colecovision-tile-mode nil)
           "detect-colecovision-tile-mode should handle nil input"))

;; Test ColecoVision platform constants
(test colecovision-platform-constants
  "Test that ColecoVision platform constants are properly defined"
  (is-true (skyline-tool::check-machine-valid 9918)
           "ColecoVision (machine 9918) should be a valid machine"))

;; Test ColecoVision error conditions
(test colecovision-error-conditions
  "Test error handling in ColecoVision functions"
  ;; Test music compilation with nil inputs
  (signals error (skyline-tool::compile-music-colecovision nil nil)
           "compile-music-colecovision should handle nil inputs")

  ;; Test art compilation with nil inputs
  (signals error (skyline-tool::compile-art-colecovision nil nil)
           "compile-art-colecovision should handle nil inputs"))

(defun run-colecovision-tests ()
  "Run all ColecoVision tests and return results"
  (fiveam:run! 'colecovision-tests))
