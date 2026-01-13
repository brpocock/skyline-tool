;;; Phantasia SkylineTool/tests/intv-gram-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC
;;; Test-Driven Development for Intellivision GRAM card compiler

(in-package :skyline-tool/test)

(def-suite intv-gram-tests
  :description "Tests for Intellivision GRAM card compilation")

(in-suite intv-gram-tests)

;; Test helper functions
(defparameter *test-gram-dir* (merge-pathnames "test-gram/" (uiop:temporary-directory)))

(defun ensure-test-gram-dir ()
  "Ensure test directory exists"
  (ensure-directories-exist *test-gram-dir*))

(defun cleanup-test-gram-file (filename)
  "Remove a test output file"
  (let ((path (merge-pathnames filename *test-gram-dir*)))
    (when (probe-file path)
      (delete-file path))))

(defmacro with-temp-gram-output ((output-var filename) &body body)
  "Create temporary output file path and cleanup after"
  `(let ((,output-var (merge-pathnames ,filename *test-gram-dir*)))
     (unwind-protect
          (progn
            (ensure-test-gram-dir)
            ,@body)
       (cleanup-test-gram-file ,filename))))

;; Test 1: Output file name
(test gram-compiler-output-filename
  "Test that GRAM compiler outputs a file with the correct name"
  (with-temp-gram-output (output-path "test-cards.s")
    (let ((input-png (make-pathname :name "test-cards" :type "png")))
      ;; Call the GRAM compiler (function name TBD)
      (skyline-tool::compile-gram-intv input-png *test-gram-dir* :height 8 :width 8 :palette-pixels nil)
      ;; Verify output file exists with correct name
      (is-true (probe-file output-path)
               "Output file should exist: ~A" output-path)
      ;; Verify file has .s extension
      (is (string= "s" (pathname-type output-path))
          "Output file should have .s extension"))))

;; Test 2: DECLE format verification
(test gram-compiler-decle-format
  "Test that GRAM compiler outputs DECLE statements in correct 16-bit format"
  (with-temp-gram-output (output-path "test-card.s")
    (let ((input-png (make-pathname :name "test-card" :type "png")))
      ;; Call the GRAM compiler
      (skyline-tool::compile-gram-intv input-png *test-gram-dir*)
      ;; Read the output file
      (let ((output-content (uiop:read-file-string output-path)))
        ;; Verify file contains DECLE keyword
        (is-true (search "DECLE" output-content)
                 "Output file should contain DECLE statements")
        ;; Verify DECLE format uses 4 hex digits (16-bit format: $0000-$FFFF)
        ;; Pattern: DECLE followed by $ and exactly 4 hex digits
        (let ((decle-pos (search "DECLE" output-content)))
          (is-true decle-pos
                   "Output file should contain DECLE statements")
          (let ((after-decle (subseq output-content decle-pos (min (+ decle-pos 30) (length output-content)))))
            (let ((dollar-pos (position #\$ after-decle)))
              (is-true dollar-pos
                       "DECLE statement should include $ prefix")
              (when dollar-pos
                (let ((hex-start (+ 1 dollar-pos))
                      (hex-end (min (+ hex-start 4) (length after-decle))))
                  (when (>= hex-end hex-start)
                    (let ((hex-str (subseq after-decle hex-start hex-end)))
                      ;; Verify exactly 4 hex digits
                      (is (= 4 (length hex-str))
                          "DECLE value should have exactly 4 hex digits (16-bit format), found: ~A"
                          hex-str)
                      ;; Verify all characters are valid hex digits
                      (is-true (every (lambda (c) (digit-char-p c 16)) hex-str)
                               "DECLE value should contain only hex digits: ~A"
                               hex-str))))))))))))

;; Helper function to create a test palette array
(defun make-test-palette-array (width height &optional (default-color 0))
  "Create a test palette array filled with DEFAULT-COLOR"
  (let ((array (make-array (list width height) :element-type '(unsigned-byte 8))))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref array x y) default-color)))
    array))

;; Test 3: Dimension validation - flooring and minimum card size
(test gram-compiler-dimension-validation
  "Test that dimensions are properly validated and floored"
  (with-temp-gram-output (output-path "test-dim-validation.s")
    (let ((test-png (make-pathname :name "test" :type "png"))
          (test-array (make-test-palette-array 16 16)))
      ;; Test with non-integer dimensions (should be floored)
      (handler-case
          (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                          :height 16.7
                                          :width 16.9
                                          :palette-pixels test-array)
        (error (e)
          (fail "Should accept floored non-integer dimensions: ~A" e)))
      ;; Test with dimensions less than 8 (should error - need at least one card)
      (signals error
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :height 7
                                        :width 8
                                        :palette-pixels (make-test-palette-array 16 16)))
      (signals error
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :height 8
                                        :width 7
                                        :palette-pixels (make-test-palette-array 16 16)))
      ;; Test with zero dimensions (should error)
      (signals error
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :height 0
                                        :width 16
                                        :palette-pixels (make-test-palette-array 16 16)))
      ;; Test with negative dimensions (should error)
      (signals error
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :height -8
                                        :width 16
                                        :palette-pixels (make-test-palette-array 16 16))))))

;; Test 4: Array dimension usage
(test gram-compiler-array-dimension-usage
  "Test that array dimensions are used correctly when height/width not provided"
  (with-temp-gram-output (output-path "test-array-dim.s")
    (let ((test-png (make-pathname :name "test" :type "png"))
          (test-array (make-test-palette-array 24 32)))
      ;; Should use array dimensions when height/width not provided
      (handler-case
          (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                          :palette-pixels test-array)
        (error (e)
          (fail "Should use array dimensions: ~A" e)))
      ;; Verify dimensions match array
      (is (= 24 (array-dimension test-array 0)))
      (is (= 32 (array-dimension test-array 1))))))

;; Test 5: Dimension usage (provided vs array)
(test gram-compiler-dimension-usage
  "Test that provided dimensions are used, array dimensions used when not provided"
  (with-temp-gram-output (output-path "test-dim-usage.s")
    (let ((test-png (make-pathname :name "test" :type "png"))
          (test-array (make-test-palette-array 16 16)))
      ;; Should use provided dimensions if within bounds
      (handler-case
          (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                          :width 8
                                          :height 8
                                          :palette-pixels test-array)
        (error (e)
          (fail "Should use provided dimensions within bounds: ~A" e)))
      ;; Should use array dimensions when not provided
      (handler-case
          (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                          :palette-pixels test-array)
        (error (e)
          (fail "Should use array dimensions: ~A" e))))))

;; Test 5b: Out-of-bounds detection
(test gram-compiler-out-of-bounds
  "Test that out-of-bounds dimensions are detected"
  (with-temp-gram-output (output-path "test-out-of-bounds.s")
    (let ((test-png (make-pathname :name "test" :type "png"))
          (test-array (make-test-palette-array 16 16)))
      ;; Width out of bounds should error
      (signals error
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :width 24
                                        :height 16
                                        :palette-pixels test-array))
      ;; Height out of bounds should error
      (signals error
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :width 16
                                        :height 24
                                        :palette-pixels test-array))
      ;; Both out of bounds should error
      (signals error
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :width 32
                                        :height 32
                                        :palette-pixels test-array)))))

;; Test 6: Flooring division for card counts
(test gram-compiler-card-count-flooring
  "Test that card counts are properly floored"
  (with-temp-gram-output (output-path "test-card-count.s")
    (let ((test-png (make-pathname :name "test" :type "png"))
          ;; 17x17 array: should produce 2x2 cards (floor(17/8) = 2)
          (test-array (make-test-palette-array 17 17)))
      (handler-case
          (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                          :palette-pixels test-array)
        (error (e)
          (fail "Should handle non-multiple dimensions: ~A" e)))
      ;; Verify output contains expected number of DECLE statements
      ;; 2x2 cards = 4 cards, each with 4 DECLE = 16 DECLE total
      (let ((content (uiop:read-file-string output-path)))
        (let ((decle-count 0)
              (pos 0))
          (loop
            (let ((found-pos (search "DECLE" content :start2 pos)))
              (when (null found-pos)
                (return))
              (incf decle-count)
              (setf pos (+ found-pos 5))))
          (is (= 16 decle-count)
              "Expected 16 DECLE statements (2×2 cards × 4 DECLE), got ~D"
              decle-count))))))

;; Test 7: Fuzz test - various array sizes
(test gram-compiler-fuzz-array-sizes
  "Fuzz test with various array sizes"
  (dolist (size '((8 8) (16 8) (8 16) (16 16) (24 24) (32 32) (17 17) (15 15) (9 9)))
    (destructuring-bind (w h) size
      (with-temp-gram-output (output-path (format nil "fuzz-~Dx~D.s" w h))
        (let ((test-png (make-pathname :name (format nil "fuzz-~Dx~D" w h) :type "png"))
              (test-array (make-test-palette-array w h)))
          (handler-case
              (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                              :palette-pixels test-array)
            (error (e)
              (fail "Fuzz test failed for ~D×~D array: ~A" w h e))))))))

;; Test 8: Fuzz test - non-integer dimensions
(test gram-compiler-fuzz-non-integer-dims
  "Fuzz test with non-integer dimension values"
  (dolist (dim-pair '((8.1 8.9) (16.5 16.5) (24.999 24.999) (32.01 32.99)))
    (destructuring-bind (w h) dim-pair
      (let ((floored-w (floor w))
            (floored-h (floor h)))
        (with-temp-gram-output (output-path (format nil "fuzz-floor-~Dx~D.s" floored-w floored-h))
          (let ((test-png (make-pathname :name (format nil "fuzz-floor-~Dx~D" floored-w floored-h) :type "png"))
                (test-array (make-test-palette-array floored-w floored-h)))
            (handler-case
                (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                                :width w
                                                :height h
                                                :palette-pixels test-array)
              (error (e)
                (fail "Fuzz test failed for floored dimensions ~D×~D from ~F×~F: ~A"
                      floored-w floored-h w h e)))))))))

;; Test 9: Regression test - single card output
(test gram-compiler-regression-single-card
  "Regression test: Single 8×8 card produces exactly 4 DECLE statements"
  (with-temp-gram-output (output-path "regression-single-card.s")
    (let ((test-png (make-pathname :name "single-card" :type "png"))
          (test-array (make-test-palette-array 8 8)))
      (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                      :palette-pixels test-array)
      (let ((content (uiop:read-file-string output-path)))
        (let ((decle-count 0)
              (pos 0))
          (loop
            (let ((found-pos (search "DECLE" content :start2 pos)))
              (when (null found-pos)
                (return))
              (incf decle-count)
              (setf pos (+ found-pos 5))))
          (is (= 4 decle-count)
              "Single card should produce exactly 4 DECLE statements, got ~D"
              decle-count))))))

;; Test 10: Regression test - byte packing order
(test gram-compiler-regression-byte-packing
  "Regression test: Verify byte packing into 16-bit words"
  (with-temp-gram-output (output-path "regression-byte-packing.s")
    (let ((test-png (make-pathname :name "byte-packing" :type "png"))
          ;; Create array with pattern: first row all white (palette index 7), rest black
          (test-array (make-array '(8 8) :element-type '(unsigned-byte 8))))
      ;; Set first row (y=0) to white (7), rest to black (0)
      (dotimes (x 8)
        (setf (aref test-array x 0) 7))
      (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                      :palette-pixels test-array)
      ;; First row all white = byte $FF, rest black = byte $00
      ;; Packed: ($FF << 8) | $00 = $FF00, then $0000, $0000, $0000
      (let ((content (uiop:read-file-string output-path)))
        (is-true (search "$FF00" content)
                 "First DECLE should be $FF00 (first row all white packed with second row)")
        (is-true (search "$0000" content)
                 "Remaining DECLE statements should be $0000")))))
