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

;; Test 5200 Mode E bitmap compilation (blob ripping)
(test 5200-mode-e-compilation
  "Test that 5200 Mode E bitmap compilation works"
  (is-true (fboundp 'skyline-tool::compile-5200-mode-e-bitmap)
           "compile-5200-mode-e-bitmap should exist")
  ;; Test basic functionality with mock data
  (let ((test-pixels (make-array '(16 16) :element-type '(unsigned-byte 32) :initial-element 0)))
    (finishes (skyline-tool::compile-5200-mode-e-bitmap test-pixels)
              "compile-5200-mode-e-bitmap should handle basic data")))

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

;; Test 5200 Mode E compilation output validation
(test 5200-mode-e-output-validation
  "Test that 5200 Mode E compilation produces correct output"
  (let* ((test-pixels (make-array '(8 8) :element-type '(unsigned-byte 32)
                                  :initial-contents
                                  #(#(0 0 0 0 0 0 0 0)
                                    #(0 1 1 1 1 1 1 0)
                                    #(0 1 0 0 0 0 1 0)
                                    #(0 1 0 1 1 0 1 0)
                                    #(0 1 0 1 1 0 1 0)
                                    #(0 1 0 0 0 0 1 0)
                                    #(0 1 1 1 1 1 1 0)
                                    #(0 0 0 0 0 0 0 0))))
         (temp-file "/tmp/5200-test-output.s"))
    ;; Compile the test data
    (skyline-tool::compile-5200-mode-e-bitmap test-pixels
                                               :png-file "/tmp/test.png"
                                               :target-dir "/tmp/")

    ;; Validate the output file exists
    (is-true (probe-file temp-file)
             "Output file should be created")

    ;; Read and validate the output content
    (when (probe-file temp-file)
      (with-open-file (stream temp-file :direction :input)
        (let ((content (alexandria:read-stream-content-into-string stream)))
          ;; Check that it contains expected assembly structure
          (is-true (search ";;; -*- fundamental -*-" content)
                   "Should contain fundamental mode comment")
          (is-true (search ".block" content)
                   "Should contain .block directive")
          (is-true (search ".bend" content)
                   "Should contain .bend directive")
          (is-true (search "Shape:" content)
                   "Should contain Shape label")
          (is-true (search "CoLu:" content)
                   "Should contain CoLu label")

          ;; Check for valid byte data format (.byte $xx)
          (is-true (cl-ppcre:scan "\\.byte \\$[0-9a-fA-F]{2}" content)
                   "Should contain properly formatted .byte directives")

          ;; Check dimensions are correct
          (is-true (search "Height = 8" content)
                   "Should report correct height")
          (is-true (search "Width = 8" content)
                   "Should report correct width"))))))

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
  "Test that 5200 platform is recognized"
  (let ((old-machine skyline-tool::*machine*))
    (unwind-protect
        (progn
          (setf skyline-tool::*machine* 5200)
          (is-true (skyline-tool::machine-valid-p)
                   "5200 should be a valid machine"))
      (setf skyline-tool::*machine* old-machine))))

;; Test 5200 error conditions
(test 5200-error-conditions
  "Test error handling in 5200 functions"
  ;; Test compile-art-5200 with invalid inputs
  (signals error (skyline-tool::compile-art-5200 "/nonexistent.in" "/tmp/test.out")
           "compile-art-5200 should signal error for missing input")

  ;; Test compile-5200-mode-e-bitmap with invalid inputs
  (signals error (skyline-tool::compile-5200-mode-e-bitmap nil)
           "compile-5200-mode-e-bitmap should handle nil input"))

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

  ;; Note: Actual dispatch testing would require PNG files
  ;; TODO: Add proper dispatch testing when PNG test files are available
)

;; Test 5200 platform constants
(test 5200-platform-constants
  "Test that 5200 platform is recognized"
  (let ((old-machine skyline-tool::*machine*))
    (unwind-protect
        (progn
          (setf skyline-tool::*machine* 5200)
          (is-true (skyline-tool::machine-valid-p)
                   "5200 should be a valid machine"))
      (setf skyline-tool::*machine* old-machine))))

;; Test 5200 error conditions
(test 5200-error-conditions
  "Test error handling in 5200 functions"
  ;; Test compile-art-5200 with invalid inputs
  (signals error (skyline-tool::compile-art-5200 "/nonexistent.in" "/tmp/test.out")
            "compile-art-5200 should signal error for missing input")

  ;; Test compile-5200-mode-e-bitmap with invalid inputs
  (signals error (skyline-tool::compile-5200-mode-e-bitmap nil)
           "compile-5200-mode-e-bitmap should handle nil input"))

(defun run-5200-tests ()
  "Run all 5200 tests and return results"
  (fiveam:run! '5200-tests))
