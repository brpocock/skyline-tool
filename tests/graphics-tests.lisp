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

;; Test blob ripping integration (basic existence check)
(test blob-ripping-integration
  "Test complete blob ripping workflow functions exist"
  (is-true (fboundp 'blob-rip-7800-320ac))
  (is-true (fboundp 'check-height+width-for-blob-320ac))
  (is-true (fboundp 'stamp-is-monochrome-p)))

(defun run-graphics-tests ()
  "Run all graphics tests and return results"
  (fiveam:run! 'graphics-tests))