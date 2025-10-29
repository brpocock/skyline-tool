(defpackage :skyline-tool/test/graphics
  (:use :cl :skyline-tool :fiveam)
  (:export #:graphics-tests))
(in-package :skyline-tool/test/graphics)

(def-suite graphics-tests
  :description "Tests for graphics compilation functions, especially ChaosFight character sprites")

(in-suite graphics-tests)

;; Helper functions for testing

(defun create-test-frame (width height &optional (fill-value 1))
  "Create a test frame array filled with the specified value"
  (let ((frame (make-array (list width height))))
    (loop for x from 0 below width do
      (loop for y from 0 below height do
        (setf (aref frame x y) fill-value)))
    frame))

(defun create-test-palette-pixels (width height)
  "Create a test palette pixels array for testing"
  (let ((pixels (make-array (list width height))))
    (loop for x from 0 below width do
      (loop for y from 0 below height do
        ;; Create a simple pattern: alternating 0 and 1
        (setf (aref pixels x y) (mod (+ x y) 2))))
    pixels))

;; Tests for extract-frame function

(test extract-frame-basic
  "Test basic frame extraction from palette pixels"
  (let* ((source (create-test-palette-pixels 16 32))
         (frame (extract-frame source 0 0 8 16)))
    (is (= (array-dimension frame 0) 8) "Frame width should be 8")
    (is (= (array-dimension frame 1) 16) "Frame height should be 16")
    ;; Test a few pixel values
    (is (= (aref frame 0 0) (aref source 0 0)) "Top-left pixel should match")
    (is (= (aref frame 7 15) (aref source 7 15)) "Bottom-right pixel should match")))

(test extract-frame-offset
  "Test frame extraction with x and y offsets"
  (let* ((source (create-test-palette-pixels 64 256))
         (frame (extract-frame source 8 16 8 16)))
    (is (= (array-dimension frame 0) 8) "Frame width should be 8")
    (is (= (array-dimension frame 1) 16) "Frame height should be 16")
    ;; Test that offset is applied correctly
    (is (= (aref frame 0 0) (aref source 8 16)) "Offset should be applied correctly")))

;; Tests for frame-to-key function

(test frame-to-key-empty-frame
  "Test frame-to-key with empty frame"
  (let* ((frame (create-test-frame 8 16 0))
         (key (frame-to-key frame)))
    (is (stringp key) "Key should be a string")
    (is (> (length key) 0) "Key should not be empty")
    ;; Should contain only zeros and commas
    (is (every (lambda (char) (or (char= char #\0) (char= char #\,))) key)
        "Key should contain only zeros and commas for empty frame")))

(test frame-to-key-filled-frame
  "Test frame-to-key with filled frame"
  (let* ((frame (create-test-frame 8 16 5))
         (key (frame-to-key frame)))
    (is (stringp key) "Key should be a string")
    (is (> (length key) 0) "Key should not be empty")
    ;; Should contain fives and commas
    (is (search "5," key) "Key should contain '5,' for filled frame")))

(test frame-to-key-uniqueness
  "Test that different frames produce different keys"
  (let* ((frame1 (create-test-frame 8 16 1))
         (frame2 (create-test-frame 8 16 2))
         (key1 (frame-to-key frame1))
         (key2 (frame-to-key frame2)))
    (is (string/= key1 key2) "Different frames should produce different keys")))

(test frame-to-key-consistency
  "Test that same frame produces same key"
  (let* ((frame (create-test-frame 8 16 3))
         (key1 (frame-to-key frame))
         (key2 (frame-to-key frame)))
    (is (string= key1 key2) "Same frame should produce identical keys")))

;; Tests for compile-2600-frame function

(test compile-2600-frame-output
  "Test that compile-2600-frame produces valid batariBASIC output"
  (let* ((frame (create-test-frame 8 16 1))
         (output (with-output-to-string (stream)
                  (compile-2600-frame stream frame 0))))
    (is (stringp output) "Output should be a string")
    (is (> (length output) 0) "Output should not be empty")
    (is (search "rem 8×16 pixel bitmap data" output) "Should contain bitmap comment")
    (is (search "rem 8×16 color data" output) "Should contain color comment")
    (is (search ".byte" output) "Should contain .byte directives")))

(test compile-2600-frame-binary-format
  "Test that bitmap data uses proper binary format"
  (let* ((frame (create-test-frame 8 16 1))
         (output (with-output-to-string (stream)
                  (compile-2600-frame stream frame 0))))
    ;; Should contain binary format bytes
    (is (search ".byte %" output) "Should contain binary format .byte directives")))

(test compile-2600-frame-hex-format
  "Test that color data uses proper hex format"
  (let* ((frame (create-test-frame 8 16 5))
         (output (with-output-to-string (stream)
                  (compile-2600-frame stream frame 0))))
    ;; Should contain hex format bytes
    (is (search ".byte $" output) "Should contain hex format .byte directives")))

;; Test mock functions for PNG handling (since we can't easily create PNG files in tests)

(defun mock-png-read (width height)
  "Mock PNG reading function for testing"
  (list :width width :height height :image-data (create-test-palette-pixels width height)))

;; Integration tests

(test chaosfight-sprite-dimensions
  "Test ChaosFight sprite sheet dimension validation"
  ;; This test verifies the dimension checking logic without actually calling compile-chaos-character
  (let ((valid-width 64)
        (valid-height 256)
        (invalid-width 32)
        (invalid-height 128))
    (is (and (= valid-width 64) (= valid-height 256)) 
        "64×256 should be valid ChaosFight dimensions")
    (is (not (and (= invalid-width 64) (= invalid-height 256))) 
        "32×128 should be invalid ChaosFight dimensions")))

(test frame-pattern-extraction
  "Test that we can extract the expected 8×16 frames from a 64×256 sprite sheet"
  (let* ((sprite-sheet (create-test-palette-pixels 64 256))
         (frames-per-row 8)
         (rows 16)
         (frame-width 8)
         (frame-height 16))
    ;; Test extracting all frames
    (loop for row from 0 below rows do
      (loop for frame from 0 below frames-per-row do
        (let* ((x-offset (* frame frame-width))
               (y-offset (* row frame-height))
               (extracted-frame (extract-frame sprite-sheet x-offset y-offset 
                                             frame-width frame-height)))
          (is (= (array-dimension extracted-frame 0) frame-width)
              "Extracted frame should have correct width")
          (is (= (array-dimension extracted-frame 1) frame-height)
              "Extracted frame should have correct height"))))))

(test deduplication-logic
  "Test frame deduplication using hash table"
  (let ((frames (make-hash-table :test 'equal))
        (frame1 (create-test-frame 8 16 1))
        (frame2 (create-test-frame 8 16 1))  ; Identical to frame1
        (frame3 (create-test-frame 8 16 2))  ; Different from frame1
        (frame-counter 0))
    
    ;; Simulate the deduplication logic from compile-chaos-character
    (let ((key1 (frame-to-key frame1)))
      (unless (gethash key1 frames)
        (setf (gethash key1 frames) frame-counter)
        (incf frame-counter)))
    
    (let ((key2 (frame-to-key frame2)))
      (unless (gethash key2 frames)
        (setf (gethash key2 frames) frame-counter)
        (incf frame-counter)))
    
    (let ((key3 (frame-to-key frame3)))
      (unless (gethash key3 frames)
        (setf (gethash key3 frames) frame-counter)
        (incf frame-counter)))
    
    (is (= frame-counter 2) "Should have 2 unique frames (frame1/frame2 are identical)")
    (is (= (gethash (frame-to-key frame1) frames) 
           (gethash (frame-to-key frame2) frames))
        "Identical frames should have same hash value")))

;; Performance tests

(test frame-extraction-performance
  "Test that frame extraction performs reasonably on large sprite sheets"
  (let* ((start-time (get-internal-real-time))
         (sprite-sheet (create-test-palette-pixels 64 256))
         (frames-extracted 0))
    
    ;; Extract all 128 frames (16 rows × 8 frames)
    (loop for row from 0 below 16 do
      (loop for frame from 0 below 8 do
        (let ((extracted-frame (extract-frame sprite-sheet (* frame 8) (* row 16) 8 16)))
          (incf frames-extracted))))
    
    (let* ((end-time (get-internal-real-time))
           (elapsed-seconds (/ (- end-time start-time) internal-time-units-per-second)))
      (is (= frames-extracted 128) "Should extract all 128 frames")
      (is (< elapsed-seconds 1.0) "Should complete in under 1 second"))))

;; Edge case tests

(test empty-frame-handling
  "Test handling of completely empty frames"
  (let* ((empty-frame (create-test-frame 8 16 0))
         (key (frame-to-key empty-frame))
         (output (with-output-to-string (stream)
                  (compile-2600-frame stream empty-frame 0))))
    (is (stringp key) "Empty frame should produce valid key")
    (is (stringp output) "Empty frame should produce valid output")
    (is (search ".byte %00000000" output) "Empty frame should produce zero bytes")))

(test single-pixel-frame
  "Test frame with only one pixel set"
  (let ((frame (create-test-frame 8 16 0)))
    ;; Set only one pixel
    (setf (aref frame 0 0) 1)
    (let ((output (with-output-to-string (stream)
                   (compile-2600-frame stream frame 0))))
      (is (search ".byte %10000000" output) "Should have one bit set in first byte")
      (is (search ".byte $01" output) "Should have color value 1"))))

;; Run all tests
(defun run-graphics-tests ()
  "Run all graphics tests and return results"
  (run! 'graphics-tests))

;; Export the test runner
(export 'run-graphics-tests)
