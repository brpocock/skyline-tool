;;; Phantasia SkylineTool/tests/5200-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite 5200-tests
  :description "Tests for Atari 5200-specific SkylineTool functionality")

(in-suite 5200-tests)

;; Test 5200 graphics functions existence
(test 5200-graphics-functions-existence
  "Test that 5200 graphics functions exist"
  (is-true (fboundp 'skyline-tool::compile-5200-mode-e-bitmap)
           "compile-5200-mode-e-bitmap should exist")
  (is-true (fboundp 'skyline-tool::compile-art-5200)
           "compile-art-5200 should exist"))

;; Test 5200 blob ripping functions
(test 5200-blob-functions-existence
  "Test that 5200 blob ripping functions exist"
  (is-true (fboundp 'skyline-tool::blob-rip-5200-tile)
           "blob-rip-5200-tile should exist")
  (is-true (fboundp 'skyline-tool::blob-rip-5200-pmg)
           "blob-rip-5200-pmg should exist")
  (is-true (fboundp 'skyline-tool::detect-5200-tile-mode)
           "detect-5200-tile-mode should exist"))

;; Test 5200 music compilation
(test 5200-music-compilation
  "Test 5200 music compilation functions"
  (is-true (fboundp 'skyline-tool::compile-music-7800)
           "5200 uses 7800 music compilation (TIA chip)"))

;; Test 5200 graphics compilation
(test 5200-graphics-compilation
  "Test 5200 graphics compilation with mock data"
  (let ((test-pixels (make-array '(16 16) :element-type '(unsigned-byte 32) :initial-element 0)))
    ;; Test compile-5200-mode-e-bitmap
    (finishes (skyline-tool::compile-5200-mode-e-bitmap test-pixels :png-file "/tmp/test.png")
              "compile-5200-mode-e-bitmap should handle basic data")

    ;; Test with invalid data
    (signals error (skyline-tool::compile-5200-mode-e-bitmap nil)
             "compile-5200-mode-e-bitmap should handle nil input")))

;; Test 5200 blob ripping functions
(test 5200-blob-ripping
  "Test 5200 blob ripping functionality"
  ;; Test detect-5200-tile-mode
  (finishes (skyline-tool::detect-5200-tile-mode (make-array '(8 8) :element-type '(unsigned-byte 32)))
            "detect-5200-tile-mode should handle basic arrays")

  ;; Test blob-rip-5200-tile (will fail due to missing file but shouldn't crash)
  (signals error (skyline-tool::blob-rip-5200-tile "/nonexistent.png")
            "blob-rip-5200-tile should signal error for missing files")

  ;; Test blob-rip-5200-pmg
  (signals error (skyline-tool::blob-rip-5200-pmg "/nonexistent.png")
            "blob-rip-5200-pmg should signal error for missing files"))

;; Test 5200 dispatch-png method
(test 5200-dispatch-png
  "Test 5200 PNG dispatch functionality"
  ;; The dispatch-png% method for 5200 should exist
  (is-true (fboundp 'skyline-tool::dispatch-png%)
           "dispatch-png% generic function should exist")

  ;; Test that 5200 dispatch works (will create files in target dir)
  (finishes (ensure-directories-exist "/tmp/5200-test/")
            "Directory creation should work")
  ;; Note: Actual dispatch testing would require PNG files, so we just test setup
  ;; TODO: Add proper dispatch testing when PNG test files are available

;; Test 5200 platform constants
(test 5200-platform-constants
  "Test that 5200 platform constants are properly defined"
  (is-true (member 5200 skyline-tool::*valid-machines*)
           "5200 should be in valid machines list"))

;; Test 5200 error conditions
(test 5200-error-conditions
  "Test error handling in 5200 functions"
  ;; Test compile-art-5200 with invalid inputs
  (signals error (skyline-tool::compile-art-5200 "/nonexistent.in" "/tmp/test.out")
            "compile-art-5200 should signal error for missing input")

  ;; Test blob functions with nil inputs
  (signals error (skyline-tool::detect-5200-tile-mode nil)
           "detect-5200-tile-mode should handle nil input"))

(defun run-5200-tests ()
  "Run all 5200 tests and return results"
  (fiveam:run! '5200-tests))
