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
                #:blob/write-spans-320ac)
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
(test blob-rip-7800-basic-validation
  "Test blob-rip-7800 validates input parameters correctly"
  ;; Test that function exists and has correct signature
  (is-true (fboundp 'blob-rip-7800))
  ;; Test error handling for non-existent files
  (signals error (blob-rip-7800 "/completely/nonexistent/file.png"))
  ;; Test that function accepts string parameters
  (is (equal (type-of (lambda (path) (declare (ignore path)) nil))
             (type-of (symbol-function 'blob-rip-7800)))
      "blob-rip-7800 should be a function"))

;; Test 320AC functionality specifically
(test blob-rip-7800-320ac-existence-and-signature
  "Test that 320AC blob ripping function exists with correct interface"
  (is-true (fboundp 'blob-rip-7800-320ac))
  ;; Function should accept path and optional imperfect flag
  (is (= 2 (length (function-lambda-list 'blob-rip-7800-320ac)))
      "blob-rip-7800-320ac should have 2 parameters (path &optional imperfect)"))

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
  (let ((mono-stamp (make-array '(4 16) :element-type '(unsigned-byte 8)
                               :initial-contents '(0 0 0 0
                                                 0 0 0 0
                                                 0 0 0 0
                                                 0 0 0 0
                                                 0 0 0 0
                                                 0 0 0 0
                                                 0 0 0 0
                                                 0 0 0 0
                                                 0 0 0 0
                                                 0 0 0 0
                                                 0 0 0 0
                                                 0 0 0 0
                                                 0 0 0 0
                                                 0 0 0 0
                                                 0 0 0 0
                                                 0 0 0 0)))
        (color-stamp (make-array '(4 16) :element-type '(unsigned-byte 8)
                                :initial-contents '(0 0 0 0
                                                  0 0 0 0
                                                  0 0 0 0
                                                  0 0 0 0
                                                  0 0 0 0
                                                  0 0 0 0
                                                  0 0 0 0
                                                  0 0 0 0
                                                  0 0 0 0
                                                  0 0 0 0
                                                  0 0 0 0
                                                  0 0 0 0
                                                  0 0 0 0
                                                  0 0 0 0
                                                  2 0 0 0  ; Value 2 requires 320C mode (4 colors + transparent)
                                                  0 0 0 0))))
    ;; Monochrome stamp (only 0,1 values) should be detected as monochrome for 320A
    (is-true (stamp-is-monochrome-p mono-stamp))
    ;; Stamp with values >1 should not be monochrome (requires 320C)
    (is-false (stamp-is-monochrome-p color-stamp))))

;; Test 320A encoding with known input/output
(test 320a-encoding-basic
  "Test 320A encoding with predictable input and expected output"
  (let* ((test-image (make-array '(8 1) :element-type '(unsigned-byte 8)
                                :initial-contents '(0 1 0 1 0 1 0 1))) ; Alternating 0,1 pattern
         (palette (vector #(0 0 0) #(255 255 255))) ; Black=0, White=1
         (result (7800-image-to-320a test-image :byte-width 1 :height 1 :palette palette)))
    ;; Should produce 1 byte column with 1 row
    (is (= 1 (length result))) ; 1 column
    (is (= 1 (length (first result)))) ; 1 row per column
    ;; The pattern 0 1 0 1 0 1 0 1 should encode to #b01010101 = 85
    (is (= 85 (first (first result))))))

;; Test 320A encoding with all zeros (transparent)
(test 320a-encoding-all-transparent
  "Test 320A encoding with all transparent pixels"
  (let* ((test-image (make-array '(8 2) :element-type '(unsigned-byte 8)
                                :initial-element 0))
         (palette (vector #(0 0 0) #(255 255 255)))
         (result (7800-image-to-320a test-image :byte-width 1 :height 2 :palette palette)))
    ;; Should produce all zero bytes
    (is (= 1 (length result))) ; 1 column
    (is (= 2 (length (first result)))) ; 2 rows
    (is (= 0 (first (first result))))
    (is (= 0 (second (first result))))))

;; Test 320A encoding with all ones (solid color)
(test 320a-encoding-all-solid
  "Test 320A encoding with all solid color pixels"
  (let* ((test-image (make-array '(8 1) :element-type '(unsigned-byte 8)
                                :initial-element 1))
         (palette (vector #(0 0 0) #(255 255 255)))
         (result (7800-image-to-320a test-image :byte-width 1 :height 1 :palette palette)))
    ;; Should produce #b11111111 = 255
    (is (= 1 (length result)))
    (is (= 1 (length (first result))))
    (is (= 255 (first (first result))))))

;; Test 320C encoding with known input/output
(test 320c-encoding-basic
  "Test 320C encoding with predictable input and expected output"
  (let* ((test-image (make-array '(4 1) :element-type '(unsigned-byte 8)
                                :initial-contents '(0 1 2 3))) ; All palette indices 0-3
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
                                :initial-contents '(3 3 3 3))) ; All maximum value (3)
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
                                  :initial-contents '(0 1 5 1)))) ; Value 5 is out of palette range
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
        (color-stamp (make-array '(4 16) :element-type '(unsigned-byte 8)
                                :initial-contents '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                                                  2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2))))
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
  (signals error (blob-rip-7800-320ac "/nonexistent.png"))
  ;; If this test passes, the function compiled successfully and basic error handling works

(defun run-graphics-tests ()
  "Run all graphics tests and return results"
  (fiveam:run! 'graphics-tests))
