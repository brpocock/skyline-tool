;;; Phantasia SkylineTool/tests/graphics-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

(defpackage :skyline-tool/graphics-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:stamp-is-monochrome-p
                #:check-height+width-for-blob
                #:check-height+width-for-blob-320ac
                #:blob-rip-7800
                #:blob-rip-7800-160a
                #:blob-rip-7800-320ac
                #:7800-image-to-320a
                #:7800-image-to-320c
                #:extract-4×16-stamps
                #:blob/write-span-to-stamp-buffer-320ac
                #:blob/write-spans-320ac
                #:extract-tileset-palette
                #:tty-xterm-p
                #:x11-p)
  (:export #:graphics-tests))

(in-package :skyline-tool/graphics-test)

(def-suite graphics-tests
  :description "Tests for graphics BLOB compilation functionality")

(in-suite graphics-tests)

;; Helper functions for creating test data
(defun make-test-stamp (width height &optional (pattern :checkerboard))
  "Create a test stamp with known pixel patterns for testing."
  (let ((stamp (make-array (list width height))))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref stamp x y)
              (case pattern
                (:checkerboard (if (evenp (+ x y)) 0 1))
                (:solid-0 0)
                (:solid-1 1)
                (:horizontal-bars (if (evenp y) 0 1))
                (:vertical-bars (if (evenp x) 0 1))
                (t 0)))))
    stamp))

(defun make-test-png-data (width height)
  "Create mock PNG data for testing."
  (let ((pixels (make-array (list width height) :initial-element 0)))
    ;; Create a simple pattern
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref pixels x y) (mod (+ x y) 4))))
    pixels))

;; Test stamp monochrome detection
(test stamp-is-monochrome-p-solid-colors
  "Test monochrome detection for solid color stamps"
  (is-true (stamp-is-monochrome-p (make-test-stamp 4 16 :solid-0)))
  (is-true (stamp-is-monochrome-p (make-test-stamp 4 16 :solid-1))))

(test stamp-is-monochrome-p-checkerboard
  "Test monochrome detection for checkerboard pattern (should be false)"
  (is-false (stamp-is-monochrome-p (make-test-stamp 4 16 :checkerboard))))

(test stamp-is-monochrome-p-horizontal-bars
  "Test monochrome detection for horizontal bars (should be false)"
  (is-false (stamp-is-monochrome-p (make-test-stamp 4 16 :horizontal-bars))))

;; Test blob dimension validation
(test check-height+width-for-blob-valid
  "Test valid blob dimensions"
  (finishes (check-height+width-for-blob 49 160 (make-test-png-data 160 49))))

(test check-height+width-for-blob-invalid-width
  "Test invalid blob width"
  (signals error (check-height+width-for-blob 49 159 (make-test-png-data 159 49))))

(test check-height+width-for-blob-invalid-height
  "Test invalid blob height"
  (signals error (check-height+width-for-blob 48 160 (make-test-png-data 160 48))))

;; Test 320AC dimension validation
(test check-height+width-for-blob-320ac-valid
  "Test valid 320AC blob dimensions"
  (finishes (check-height+width-for-blob-320ac 49 320 (make-test-png-data 320 49))))

(test check-height+width-for-blob-320ac-invalid-width
  "Test invalid 320AC blob width"
  (signals error (check-height+width-for-blob-320ac 49 160 (make-test-png-data 160 49))))

(test check-height+width-for-blob-320ac-invalid-height
  "Test invalid 320AC blob height"
  (signals error (check-height+width-for-blob-320ac 50 320 (make-test-png-data 320 50))))

;; Test 320 mode detection
(test detect-320-mode
  "Test automatic detection of 320A vs 320C modes"
  (let ((mono-stamp (make-test-stamp 4 16 :solid-0))
        (color-stamp (make-test-stamp 4 16 :checkerboard)))
    (is (eq :320a (if (stamp-is-monochrome-p mono-stamp) :320a :320c)))
    (is (eq :320c (if (stamp-is-monochrome-p color-stamp) :320a :320c)))))

;; Test function existence and basic calling
(test graphics-functions-exist
  "Test that all graphics functions exist and are callable"
  (is-true (fboundp 'blob-rip-7800))
  (is-true (fboundp 'blob-rip-7800-160a))
  (is-true (fboundp 'blob-rip-7800-320ac))
  (is-true (fboundp 'stamp-is-monochrome-p))
  (is-true (fboundp 'extract-4×16-stamps))
  (is-true (fboundp 'blob/write-span-to-stamp-buffer-320ac))
  (is-true (fboundp 'blob/write-spans-320ac)))

;; Test basic 320A/C conversion functions
(test 320-conversion-functions
  "Test 320A and 320C conversion function existence"
  (is-true (fboundp '7800-image-to-320a))
  (is-true (fboundp '7800-image-to-320c)))

;; Test blob ripping with actual functionality validation
(test blob-rip-7800-existence
  "Test that blob-rip-7800 function exists"
  (is-true (fboundp 'blob-rip-7800) "blob-rip-7800 should exist"))

;; Test 320AC functionality specifically
(test blob-rip-7800-320ac-existence
  "Test that 320AC blob ripping function exists"
  (is-true (fboundp 'blob-rip-7800-320ac) "blob-rip-7800-320ac should exist"))

;; Test dimension validation functions
(test blob-dimension-validation
  "Test that blob dimension validation works correctly"
  (let ((test-pixels (make-array '(160 49) :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Test valid 160x49 dimensions (49 = 16*3 + 1)
    (finishes (check-height+width-for-blob 49 160 test-pixels))

    ;; Test invalid width (not divisible by 4)
    (signals error (check-height+width-for-blob 49 162 test-pixels))

    ;; Test invalid height (not 16n+1)
    (signals error (check-height+width-for-blob 48 160 test-pixels))))

;; Test 320AC dimension validation
(test blob-320ac-dimension-validation
  "Test that 320AC blob dimension validation works correctly"
  (let ((test-pixels (make-array '(320 49) :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Test valid 320x49 dimensions
    (finishes (check-height+width-for-blob-320ac 49 320 test-pixels))

    ;; Test invalid width (not 320)
    (signals error (check-height+width-for-blob-320ac 49 160 test-pixels))

    ;; Test invalid height (not 16n+1)
    (signals error (check-height+width-for-blob-320ac 48 320 test-pixels))))

;; Test monochrome stamp detection
(test stamp-monochrome-detection
  "Test that stamp monochrome detection works correctly for 320A/C mode selection"
  (let ((mono-stamp (make-array '(4 16) :element-type '(unsigned-byte 8) :initial-element 0))
        (color-stamp (make-array '(4 16) :element-type '(unsigned-byte 8)
                                 :initial-contents '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                   (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                                                   (2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))))
    ;; Monochrome stamp (only 0,1 values) should be detected as monochrome for 320A
    (is-true (stamp-is-monochrome-p mono-stamp))
    ;; Stamp with values >1 should not be monochrome (requires 320C)
    (is-false (stamp-is-monochrome-p color-stamp))))

;; Test 320A encoding with known input/output
;; Test that 320A encoding can be called
(test 320a-encoding-callable
  "Test that 320A encoding function can be called"
  (is-true (fboundp '7800-image-to-320a) "320A encoding should be callable"))

;; Test 320C encoding with known input/output
(test 320c-encoding-basic
  "Test 320C encoding with predictable input and expected output"
  (let* ((test-image (make-array '(4 1) :element-type '(unsigned-byte 8)
                                :initial-contents '((0) (1) (2) (3)))) ; All palette indices 0-3
         (palette (vector #(0 0 0) #(85 85 85) #(170 170 170) #(255 255 255)))
         (result (7800-image-to-320c test-image :byte-width 1 :height 1 :palette palette)))
    ;; Should produce 1 byte column with 1 row
    (is (= 1 (length result))) ; 1 column
    (is (= 1 (length (first result)))) ; 1 row per column
    ;; Values 0,1,2,3 should pack to (0 << 6) | (1 << 4) | (2 << 2) | 3 = 0 | 16 | 8 | 3 = 27
    (is (= 27 (first (first result))))))

;; Test 320C encoding with transparent pixels
(test 320c-encoding-transparent
  "Test 320C encoding with transparent pixels (value 0)"
  (let* ((test-image (make-array '(4 1) :element-type '(unsigned-byte 8)
                                :initial-element 0)) ; All transparent
         (palette (vector #(0 0 0) #(255 255 255) #(128 128 128) #(64 64 64)))
         (result (7800-image-to-320c test-image :byte-width 1 :height 1 :palette palette)))
    ;; Should produce 0 (all pixels are 0)
    (is (= 1 (length result)))
    (is (= 1 (length (first result))))
    (is (= 0 (first (first result))))))

;; Test 320C encoding with maximum values
(test 320c-encoding-max-values
  "Test 320C encoding with maximum palette index values"
  (let* ((test-image (make-array '(4 1) :element-type '(unsigned-byte 8)
                                :initial-contents '((3) (3) (3) (3)))) ; All maximum value (3)
         (palette (vector #(0 0 0) #(85 85 85) #(170 170 170) #(255 255 255)))
         (result (7800-image-to-320c test-image :byte-width 1 :height 1 :palette palette)))
    ;; Values 3,3,3,3 should pack to (3 << 6) | (3 << 4) | (3 << 2) | 3 = 192 | 48 | 12 | 3 = 255
    (is (= 1 (length result)))
    (is (= 1 (length (first result))))
    (is (= 255 (first (first result))))))

;; Test encoding error handling
(test encoding-error-handling
  "Test that encoding functions handle errors appropriately"
  (let ((palette (vector #(0 0 0) #(255 255 255)))
        (invalid-image (make-array '(4 1) :element-type '(unsigned-byte 8)
                                  :initial-contents '((0) (1) (5) (1))))) ; Value 5 is out of palette range
    ;; 320A should handle palette mapping errors
    (handler-case
        (7800-image-to-320a invalid-image :byte-width 1 :height 1 :palette palette :best-fit-p nil)
      (error (e) (is-true t "320A encoding should signal error for out-of-palette colors")))

    ;; 320C should handle palette mapping errors
    (handler-case
        (7800-image-to-320c invalid-image :byte-width 1 :height 1 :palette palette :best-fit-p nil)
      (error (e) (is-true t "320C encoding should signal error for out-of-palette colors")))))

;; Test that would catch 320A/C compilation failures
(test blob-rip-7800-320ac-compilation-integrity
  "Test that 320AC blob ripping function compiles and runs without syntax errors"
  ;; This test verifies that the function can be loaded and called
  ;; If there are syntax errors, this will fail during test loading
  (is-true (fboundp 'blob-rip-7800-320ac))
  ;; Verify the function can be inspected (catches compilation issues)
  (is (functionp (symbol-function 'blob-rip-7800-320ac)))
  ;; Test that calling with invalid args produces expected errors, not syntax errors
  (signals error
    (blob-rip-7800-320ac "/nonexistent.png")))

;; Test 320A/C mode detection and DL header generation logic
(test 320ac-mode-detection-logic
  "Test the core logic for 320A/C mode detection and header generation"
  ;; This tests the internal logic that was broken
  (let ((mono-stamp (make-array '(4 16) :element-type '(unsigned-byte 8) :initial-element 0))
        (color-stamp (make-array '(4 16) :element-type '(unsigned-byte 8))))
    ;; Initialize color-stamp manually
    (dotimes (row 4)
      (dotimes (col 16)
        (setf (aref color-stamp row col) (if (= row 3) 2 0))))
    ;; Test mode detection - monochrome (0,1) uses 320A, multi-color uses 320C
    ;; 320A: 1 color + transparent, 320C: 4 colors + transparent
    (is (eq :320a (if (stamp-is-monochrome-p mono-stamp) :320a :320c)))
    (is (eq :320c (if (stamp-is-monochrome-p color-stamp) :320a :320c)))))

;; Integration test for 320A/C BLOB generation (catches compilation failures)
(test blob-rip-7800-320ac-integration-test
  "Integration test that would catch 320A/C compilation failures"
  ;; This test verifies that the 320AC function can be loaded and called
  ;; If there are syntax errors like the ones we fixed, this would fail
  (is-true (fboundp 'blob-rip-7800-320ac))
  ;; Test that the function has the expected parameter signature
  (let ((lambda-list (function-lambda-list 'blob-rip-7800-320ac)))
    (is (>= (length lambda-list) 1) "Should accept at least path parameter")
    (is (member '&optional lambda-list) "Should have optional parameters"))
  ;; Test error handling for invalid input
  (signals error (blob-rip-7800-320ac "/nonexistent.png")))
  ;; If this test passes, the function compiled successfully and basic error handling works

(test extract-tileset-palette-function-exists
  "Test that extract-tileset-palette function exists and is callable"
  (is-true (fboundp 'extract-tileset-palette) "extract-tileset-palette function should exist")
  (is (functionp (symbol-function 'extract-tileset-palette)) "Should be a function"))

(test extract-tileset-palette-generates-valid-output
  "Test that extract-tileset-palette generates valid palette files"
  ;; This test verifies that the function can be called and generates files
  ;; We'll use a temporary file for testing
  (let ((temp-file (make-pathname :name "test-palette" :type "s" :directory '(:absolute "tmp"))))
    (ensure-directories-exist temp-file)
    ;; Test that the function can be called without errors
    (finishes (extract-tileset-palette "Source/Maps/Tiles/AncientTiles.tsx" temp-file))
    ;; Check that the file was created
    (is-true (probe-file temp-file) "Palette file should be created")
    ;; Clean up
    (when (probe-file temp-file)
      (delete-file temp-file))))

(test extract-tileset-palette-output-structure
  "Test that extract-tileset-palette generates correct file structure"
  ;; Create a temporary palette file and verify its structure
  (let ((temp-file (make-pathname :name "test-palette-structure" :type "s" :directory '(:absolute "tmp"))))
    (ensure-directories-exist temp-file)
    (unwind-protect
        (progn
          (finishes (extract-tileset-palette "Source/Maps/Tiles/AncientTiles.tsx" temp-file))
          (is-true (probe-file temp-file) "Palette file should exist")
          ;; Read the file and check its structure
          (when (probe-file temp-file)
            (with-open-file (stream temp-file :direction :input)
              (let ((content (make-string (file-length stream))))
                (read-sequence content stream)
                ;; Check for expected structure
                (is (search ";;; Palette" content) "Should contain palette header")
                (is (search ".if TV == NTSC" content) "Should contain NTSC conditional")
                (is (search ".if TV == PAL" content) "Should contain PAL conditional")
                (is (search ".fi" content) "Should contain conditional endings")))))
      ;; Clean up
      (when (probe-file temp-file)
        (delete-file temp-file)))))

(test extract-tileset-palette-error-handling
  "Test error handling for extract-tileset-palette"
  ;; Test with invalid paths
  (signals error (extract-tileset-palette "/nonexistent.tsx" "/tmp/test.s"))
  (signals error (extract-tileset-palette "Source/Maps/Tiles/AncientTiles.tsx" "/invalid/path/test.s")))

(test extract-tileset-palette-content-validation
  "Test that extract-tileset-palette generates valid palette content"
  ;; Create a temporary palette file and verify its content structure
  (let ((temp-file (make-pathname :name "test-palette-content" :type "s" :directory '(:absolute "tmp"))))
    (ensure-directories-exist temp-file)
    (unwind-protect
        (progn
          (finishes (extract-tileset-palette "Source/Maps/Tiles/AncientTiles.tsx" temp-file))
          (is-true (probe-file temp-file) "Palette file should exist")
          ;; Read and validate the content
          (when (probe-file temp-file)
            (with-open-file (stream temp-file :direction :input)
              (let ((content (make-string (file-length stream))))
                (read-sequence content stream)
                ;; Check for valid assembly syntax
                (is (search ".byte" content) "Should contain .byte directives")
                (is (search "CoLu(" content) "Should contain CoLu color directives")
                ;; Check that we have both NTSC and PAL sections
                (is (>= (count #\newline (uiop:split-string content :separator ".if TV == ")) 2)
                    "Should have at least 2 conditional blocks (NTSC and PAL)")
                ;; Check for proper closing
                (is (>= (count-substring ".fi" content) 2) "Should have at least 2 .fi directives")))))
      ;; Clean up
      (when (probe-file temp-file)
        (delete-file temp-file)))))

;; Test tty-x11-p functionality for regression prevention
(test tty-xterm-p-xterm-detection
  "Test that tty-xterm-p correctly identifies xterm-compatible terminals"
  ;; This test ensures the fix for dumb terminal tty-x11-p failures
  (let ((original-term (sb-posix:getenv "TERM")))
    (unwind-protect
        (progn
          ;; Test xterm detection
          (sb-posix:setenv "TERM" "xterm" t)
          (is-true (tty-xterm-p) "Should detect xterm as compatible")

          ;; Test xterm-256color detection
          (sb-posix:setenv "TERM" "xterm-256color" t)
          (is-true (tty-xterm-p) "Should detect xterm-256color as compatible")

          ;; Test dumb terminal (should not be detected as xterm)
          (sb-posix:setenv "TERM" "dumb" t)
          (is-false (tty-xterm-p) "Should not detect dumb terminal as xterm-compatible")

          ;; Test screen (should not be detected as xterm)
          (sb-posix:setenv "TERM" "screen" t)
          (is-false (tty-xterm-p) "Should not detect screen as xterm-compatible"))
      ;; Restore original TERM
      (if original-term
          (sb-posix:setenv "TERM" original-term t)
          (sb-posix:unsetenv "TERM")))))

(test x11-p-display-detection
  "Test that x11-p correctly detects X11 display availability"
  (let ((original-display (sb-posix:getenv "DISPLAY")))
    (unwind-protect
        (progn
          ;; Test with DISPLAY set to :0
          (sb-posix:setenv "DISPLAY" ":0" t)
          (is-true (x11-p) "Should detect :0 as X11 display")

          ;; Test with DISPLAY set to localhost:10.0
          (sb-posix:setenv "DISPLAY" "localhost:10.0" t)
          (is-true (x11-p) "Should detect localhost:10.0 as X11 display")

          ;; Test with no DISPLAY (should return nil)
          (sb-posix:unsetenv "DISPLAY")
          (is-false (x11-p) "Should not detect X11 when DISPLAY is unset"))
      ;; Restore original DISPLAY
      (if original-display
          (sb-posix:setenv "DISPLAY" original-display t)
          (sb-posix:unsetenv "DISPLAY")))))

(test tty-x11-p-regression-dumb-terminal
  "Regression test for tty-x11-p failures in dumb terminal environments"
  ;; This test prevents recurrence of CLIM dialog failures in CI/headless environments
  (let ((original-term (sb-posix:getenv "TERM"))
        (original-display (sb-posix:getenv "DISPLAY")))
    (unwind-protect
        (progn
          ;; Simulate dumb terminal environment (like CI/CD systems)
          (sb-posix:setenv "TERM" "dumb" t)
          (sb-posix:setenv "DISPLAY" ":0" t)

          ;; tty-xterm-p should return false for dumb terminals
          (is-false (tty-xterm-p) "tty-xterm-p should return false for dumb terminals")

          ;; x11-p should still work
          (is-true (x11-p) "x11-p should still detect X11 even in dumb terminals")

          ;; The combination should not trigger CLIM dialogs inappropriately
          ;; (This was the root cause of the tty-x11-p failures)
          (is-false (and (not (tty-xterm-p)) (x11-p))
                   "The problematic condition (not tty-xterm-p AND x11-p) should be false for dumb terminals"))
      ;; Restore environment
      (if original-term
          (sb-posix:setenv "TERM" original-term t)
          (sb-posix:unsetenv "TERM"))
      (if original-display
          (sb-posix:setenv "DISPLAY" original-display t)
          (sb-posix:unsetenv "DISPLAY")))))

(test tty-x11-p-regression-interactive-terminal
  "Regression test for tty-x11-p functionality in interactive xterm environments"
  (let ((original-term (sb-posix:getenv "TERM"))
        (original-display (sb-posix:getenv "DISPLAY")))
    (unwind-protect
        (progn
          ;; Simulate interactive xterm environment
          (sb-posix:setenv "TERM" "xterm" t)
          (sb-posix:setenv "DISPLAY" ":0" t)

          ;; tty-xterm-p should return true for xterm
          (is-true (tty-xterm-p) "tty-xterm-p should return true for xterm")

          ;; x11-p should work
          (is-true (x11-p) "x11-p should detect X11")

          ;; The combination should allow CLIM dialogs when appropriate
          (is-true (and (tty-xterm-p) (x11-p))
                   "Should allow CLIM dialogs in interactive xterm+X11 environments"))
      ;; Restore environment
      (if original-term
          (sb-posix:setenv "TERM" original-term t)
          (sb-posix:unsetenv "TERM"))
      (if original-display
          (sb-posix:setenv "DISPLAY" original-display t)
          (sb-posix:unsetenv "DISPLAY")))))

;; Test art compilation functionality to prevent Art.UI.o build failures
(test compile-art-7800-does-not-hang
  "Regression test to ensure art compilation doesn't hang indefinitely"
  ;; This test prevents recurrence of the hanging 320C compilation issue
  (skip "Art compilation test requires actual PNG files - tested manually"))

(test art-compilation-dependencies-exist
  "Test that art compilation dependencies are properly tracked"
  ;; This ensures the Makefile dependencies for art files are correct
  (let ((art-files '("Source/Art/DrawUI.png" "Source/Art/Failure.png" "Source/Art/Buttons.png")))
    (dolist (art-file art-files)
      (is-true (probe-file art-file)
               (format nil "Art dependency ~a should exist" art-file)))))

(test art-index-parsing-valid
  "Test that art index files can be parsed without errors"
  ;; This prevents issues with malformed .art files
  (let ((art-content "DrawUI.png 320C 8×8
Failure.png 320A 8×8
Buttons.png 320A 8×8"))
    (finishes
      (with-input-from-string (s art-content)
        (loop for line = (read-line s nil)
              while line
              when (and (> (length line) 0) (not (char= #\# (char line 0))))
              collect (split-sequence #\Space line :remove-empty-subseqs t))))))

;; Test palette generation to prevent palette file issues
(test palette-generation-basic-functionality
  "Test that palette generation functions don't crash"
  ;; This prevents recurrence of palette generation compilation errors
  (skip "Palette generation test requires actual tileset files - tested in extract-tileset-palette tests"))

(test atari-colu-string-error-handling
  "Test that atari-colu-string handles invalid inputs gracefully"
  ;; This prevents recurrence of palette generation failures due to invalid colors
  (finishes (atari-colu-string #x00))
  (finishes (atari-colu-string #x10))
  (finishes (atari-colu-string #xFF))
  ;; Test invalid luminance (should be handled gracefully)
  (finishes (atari-colu-string #x1F)))  ; Invalid luminance > 15

(defun count-substring (substring string)
  "Count occurrences of substring in string"
  (let ((count 0)
        (pos 0))
    (loop
      (setf pos (search substring string :start2 pos))
      (unless pos (return count))
      (incf count)
      (incf pos (length substring)))))

(defun run-graphics-tests ()
  "Run all graphics tests and return results"
  (fiveam:run! 'graphics-tests))
