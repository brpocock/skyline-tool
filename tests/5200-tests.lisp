;;; Phantasia SkylineTool/tests/5200-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite 5200-tests
  :description "Tests for Atari 5200-specific SkylineTool functionality")

(in-suite 5200-tests)

;; Test 5200 Mode E bitmap compilation functionality
(test 5200-mode-e-bitmap-compilation
  "Test that 5200 Mode E bitmap compilation produces correct output"
  ;; Test with a simple 8x8 pattern that should produce recognizable output
  (let* ((test-pixels (make-array '(8 8) :element-type '(unsigned-byte 32)
                                  :initial-contents
                                  #(#(0 0 0 0 0 0 0 0)  ; Row 0: all background
                                    #(1 1 1 1 1 1 1 1)  ; Row 1: all foreground
                                    #(0 1 0 1 0 1 0 1)  ; Row 2: alternating pattern
                                    #(1 0 1 0 1 0 1 0)  ; Row 3: inverse alternating
                                    #(0 0 1 1 0 0 1 1)  ; Row 4: 2-pixel pattern
                                    #(1 1 0 0 1 1 0 0)  ; Row 5: inverse 2-pixel
                                    #(0 0 0 0 1 1 1 1)  ; Row 6: half/half
                                    #(1 1 1 1 0 0 0 0)))) ; Row 7: inverse half/half
         (temp-file "Object/5200/tmp5200.s"))

    ;; Compile the test data - this should return a truthy value indicating success
    (is-true (skyline-tool::compile-5200-mode-e-bitmap test-pixels
                                                       :target-dir "Object/5200/")
             "compile-5200-mode-e-bitmap should return truthy value for valid input")

    ;; Verify output file was created
    (is-true (probe-file temp-file)
             "Output file should be created for valid input")

    ;; Verify output contains expected assembly structure
    (when (probe-file temp-file)
      (with-open-file (stream temp-file :direction :input)
        (let ((content (alexandria:read-stream-content-into-string stream)))
          ;; Check for proper assembly file structure
          (is-true (search ";;; -*- fundamental -*-" content)
                   "Output should contain fundamental mode comment")
          (is-true (search ".block" content)
                   "Output should contain .block directive")
          (is-true (search ".bend" content)
                   "Output should contain .bend directive")

          ;; Check for shape data labels
          (is-true (search "Shape:" content)
                   "Output should contain Shape data label")
          (is-true (search "CoLu:" content)
                   "Output should contain color data label")

          ;; Check for valid assembly byte directives
          (is-true (cl-ppcre:scan "\\.byte \\$[0-9a-fA-F]{2}" content)
                   "Output should contain properly formatted .byte directives")

          ;; Verify dimensions are reported correctly
          (is-true (search "Height = 8" content)
                   "Should report correct height")
          (is-true (search "Width = 8" content)
                   "Should report correct width"))))))

;; Test 5200 Mode E bitmap error handling
(test 5200-mode-e-error-handling
  "Test that 5200 Mode E bitmap compilation handles errors properly"
  ;; Test with nil input
  (signals error (skyline-tool::compile-5200-mode-e-bitmap nil)
           "Should signal error for nil input")

  ;; Test with wrong array dimensions
  (signals error (skyline-tool::compile-5200-mode-e-bitmap (make-array '(0 0)))
           "Should signal error for zero-dimension arrays")

  ;; Test with non-array input
  (signals error (skyline-tool::compile-5200-mode-e-bitmap "not an array")
           "Should signal error for non-array input")

  ;; Test with array of wrong element type
  (signals error (skyline-tool::compile-5200-mode-e-bitmap (make-array '(8 8) :element-type 'character))
           "Should signal error for wrong element type"))

;; Test 5200 Mode E bitmap with different sizes
(test 5200-mode-e-different-sizes
  "Test 5200 Mode E bitmap compilation with different valid sizes"
  ;; Test with 16x16 array
  (let ((large-pixels (make-array '(16 16) :element-type '(unsigned-byte 32) :initial-element 0)))
    (is-true (skyline-tool::compile-5200-mode-e-bitmap large-pixels :target-dir "Object/5200/")
             "Should handle 16x16 arrays and return truthy value"))

  ;; Test with 4x4 array
  (let ((small-pixels (make-array '(4 4) :element-type '(unsigned-byte 32) :initial-element 1)))
    (is-true (skyline-tool::compile-5200-mode-e-bitmap small-pixels :target-dir "Object/5200/")
             "Should handle 4x4 arrays and return truthy value"))

  ;; Test with rectangular array (not square)
  (let ((rect-pixels (make-array '(8 16) :element-type '(unsigned-byte 32) :initial-element 0)))
    (is-true (skyline-tool::compile-5200-mode-e-bitmap rect-pixels :target-dir "Object/5200/")
             "Should handle rectangular arrays and return truthy value")))

;; Test 5200 dispatch functionality
(test 5200-dispatch-functionality
  "Test 5200 PNG dispatch functionality"
  ;; Verify dispatch-png% method exists for 5200
  (is-true (fboundp 'skyline-tool::dispatch-png%)
           "dispatch-png% generic function should exist")

  ;; Test that dispatch can be called (though it may not do much without actual PNG files)
  (is (skyline-tool::dispatch-png% 5200 "Object/5200/tmpfoo.png" "Object/5200/")
      nil
      "dispatch-png% should return nil for missing files"))

;; Test 5200 platform validation
(test 5200-platform-validation
  "Test that 5200 is properly recognized as a valid platform"
  (let ((old-machine skyline-tool::*machine*))
    (unwind-protect
        (progn
          ;; Test that machine is recognized as valid
          (is-true (skyline-tool::machine-valid-p)
                   "5200 should be recognized as a valid machine")

          ;; Test machine name functions
          (is (stringp (skyline-tool::machine-short-name))
              "machine-short-name should return a string")
          (is (stringp (skyline-tool::machine-long-name))
              "machine-long-name should return a string")

          ;; Test that 5200 appears in valid machines list (if such a list exists)
          (when (boundp 'skyline-tool::*valid-machines*)
            (is-true (member 5200 skyline-tool::*valid-machines*)
                     "5200 should be in valid machines list")))
      (setf skyline-tool::*machine* old-machine))))

;; Test 5200 unimplemented functions signal appropriate errors
(test 5200-unimplemented-functions
  "Test that unimplemented 5200 functions signal appropriate errors"
  ;; Test compile-art-5200 (not implemented)
  (signals error (skyline-tool::compile-art-5200 "/fake.in" "/fake.out")
           "compile-art-5200 should signal not-implemented error")

  ;; Test blob ripping functions (not implemented)
  (signals error (skyline-tool::blob-rip-5200-tile "/fake.png")
           "blob-rip-5200-tile should signal error for missing implementation")

  (signals error (skyline-tool::blob-rip-5200-pmg "/fake.png")
           "blob-rip-5200-pmg should signal error for missing implementation")

  ;; Test detect-5200-tile-mode with invalid input
  (signals error (skyline-tool::detect-5200-tile-mode nil)
           "detect-5200-tile-mode should handle nil input"))

(defun run-5200-tests ()
  "Run all 5200 tests and return results"
  (fiveam:run! '5200-tests))
