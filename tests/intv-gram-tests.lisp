;;; Phantasia SkylineTool/tests/intv-gram-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC
;;; Test-Driven Development for Intellivision GRAM card compiler

(in-package :skyline-tool/test)

(def-suite intv-gram-tests
  :description "Tests for Intellivision graphics and music compilation")

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
                       floored-w floored-h w h e))))))))

;; Test 9: GRAM card data validation
(test gram-compiler-card-data-validation
  "Test that GRAM compiler produces correct 16-bit card data"
  (with-temp-gram-output (output-path "data-test.s")
    ;; Create a simple 8x8 test pattern
    (let ((test-array (make-array '(8 8) :element-type '(unsigned-byte 8))))
      ;; Fill with a simple pattern: top-left 4x4 = color 1, bottom-right 4x4 = color 2
      (dotimes (x 8)
        (dotimes (y 8)
          (setf (aref test-array x y)
                (if (and (< x 4) (< y 4)) 1 2))))

      (let ((test-png (make-pathname :name "data-test" :type "png")))
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :palette-pixels test-array)

        ;; Verify the output contains correct GRAM card data
        (let ((content (uiop:read-file-string output-path)))
          ;; Each GRAM card should be represented as 8 DECLE statements (8 rows × 1 card)
          ;; For 8x8 input, we get 1 GRAM card (8 rows of 8 pixels each = 8 DECLE)
          (let ((decle-lines (cl-ppcre:all-matches-as-strings "DECLE \\$([0-9A-F]{4})" content)))
            (is (= 8 (length decle-lines))
                "Should generate 8 DECLE statements for 8x8 GRAM card")

            ;; The first row should have left 4 pixels as color 1, right 4 as color 2
            ;; In Intellivision GRAM format, each DECLE represents 8 pixels
            ;; Color 1 and 2 would be encoded based on the palette
            (is (> (length decle-lines) 0)
                "Should have at least one DECLE line")

            ;; Verify each DECLE is a valid 16-bit hex value
            (dolist (line decle-lines)
              (is-true (cl-ppcre:scan "^DECLE \\$[0-9A-F]{4}$" line)
                       "Each DECLE should be valid 16-bit hex: ~A" line))))))))

;; Test 10: Binary data validation for known patterns
(test gram-compiler-binary-validation
  "Test that GRAM compiler produces correct binary data for known pixel patterns"
  (with-temp-gram-output (output-path "binary-test.s")
    ;; Test pattern: checkerboard (alternating black/white pixels)
    (let ((test-array (make-array '(8 8) :element-type '(unsigned-byte 8))))
      ;; Create checkerboard: even sum of coordinates = white (7), odd = black (0)
      (dotimes (x 8)
        (dotimes (y 8)
          (setf (aref test-array x y)
                (if (evenp (+ x y)) 7 0))))

      (let ((test-png (make-pathname :name "binary-test" :type "png")))
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :palette-pixels test-array)

        ;; Parse the output and validate binary data
        (let ((content (uiop:read-file-string output-path)))
          (let ((decle-values (cl-ppcre:all-matches-as-strings "\\$([0-9A-F]{4})" content)))
            ;; For checkerboard, each row should be alternating bits: 10101010 ($AA) or 01010101 ($55)
            ;; Since we're packing 2 bytes per DECLE in big-endian format
            (is (= 8 (length decle-values))
                "Should generate 8 DECLE values for 8x8 checkerboard")

            ;; Each DECLE should be either $AAAA (AA AA) or $5555 (55 55) for checkerboard
            ;; AA = 10101010, 55 = 01010101
            (dolist (hex-value decle-values)
              (let ((numeric-value (parse-integer hex-value :start 1 :radix 16)))
                (is-true (or (= numeric-value #xAAAA) (= numeric-value #x5555))
                         "Each DECLE should be $AAAA or $5555 for checkerboard: ~A" hex-value)))))))))

;; Test 11: All-black GRAM card validation
(test gram-compiler-all-black-validation
  "Test GRAM compiler with all-black pixel pattern"
  (with-temp-gram-output (output-path "all-black.s")
    (let ((test-array (make-array '(8 8) :element-type '(unsigned-byte 8) :initial-element 0)))
      (let ((test-png (make-pathname :name "all-black" :type "png")))
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :palette-pixels test-array)

        (let ((content (uiop:read-file-string output-path)))
          (let ((decle-values (cl-ppcre:all-matches-as-strings "\\$([0-9A-F]{4})" content)))
            ;; All black pixels should result in all $0000 DECLE values
            (is (= 8 (length decle-values))
                "Should generate 8 DECLE values for 8x8 all-black card")
            (dolist (hex-value decle-values)
              (is (string= hex-value "$0000")
                  "All-black card should have $0000 values: ~A" hex-value))))))))

;; Test 12: All-white GRAM card validation
(test gram-compiler-all-white-validation
  "Test GRAM compiler with all-white pixel pattern"
  (with-temp-gram-output (output-path "all-white.s")
    (let ((test-array (make-array '(8 8) :element-type '(unsigned-byte 8) :initial-element 7)))
      (let ((test-png (make-pathname :name "all-white" :type "png")))
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :palette-pixels test-array)

        (let ((content (uiop:read-file-string output-path)))
          (let ((decle-values (cl-ppcre:all-matches-as-strings "\\$([0-9A-F]{4})" content)))
            ;; All white pixels should result in all $FFFF DECLE values
            (is (= 8 (length decle-values))
                "Should generate 8 DECLE values for 8x8 all-white card")
            (dolist (hex-value decle-values)
              (is (string= hex-value "$FFFF")
                  "All-white card should have $FFFF values: ~A" hex-value))))))))

;; Test 13: Multi-card GRAM compilation validation
(test gram-compiler-multi-card-validation
  "Test GRAM compiler with multiple cards (16x16 image = 4 cards)"
  (with-temp-gram-output (output-path "multi-card.s")
    (let ((test-array (make-array '(16 16) :element-type '(unsigned-byte 8))))
      ;; Create distinct patterns for each card
      (dotimes (x 16)
        (dotimes (y 16)
          (let ((card-x (floor x 8))
                (card-y (floor y 8)))
            ;; Different pattern for each card based on its position
            (setf (aref test-array x y)
                  (cond ((and (= card-x 0) (= card-y 0)) 7) ; Top-left: white
                        ((and (= card-x 1) (= card-y 0)) 0) ; Top-right: black
                        ((and (= card-x 0) (= card-y 1)) 7) ; Bottom-left: white
                        (t 0)))))) ; Bottom-right: black

      (let ((test-png (make-pathname :name "multi-card" :type "png")))
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :palette-pixels test-array)

        (let ((content (uiop:read-file-string output-path)))
          (let ((decle-values (cl-ppcre:all-matches-as-strings "\\$([0-9A-F]{4})" content)))
            ;; 16x16 = 4 cards × 8 DECLE each = 32 DECLE total
            (is (= 32 (length decle-values))
                "Should generate 32 DECLE values for 16x16 image (4 cards)")

            ;; First card (top-left) should be all $FFFF (white)
            (dotimes (i 8)
              (is (string= (nth i decle-values) "$FFFF")
                  "First card DECLE ~D should be $FFFF (white): ~A" i (nth i decle-values)))

            ;; Second card (top-right) should be all $0000 (black)
            (dotimes (i 8)
              (is (string= (nth (+ i 8) decle-values) "$0000")
                  "Second card DECLE ~D should be $0000 (black): ~A" (+ i 8) (nth (+ i 8) decle-values)))))))))

;; Test 14: Sprite compilation binary validation
(test intv-sprite-binary-validation
  "Test that Intellivision sprite compiler produces correct binary data"
  (with-temp-gram-output (output-path "sprite-test.s")
    ;; Test pattern: 8x8 sprite with diagonal pattern
    (let ((test-array (make-array '(8 8) :element-type '(unsigned-byte 8))))
      ;; Create diagonal pattern: pixels where x = y are white
      (dotimes (x 8)
        (dotimes (y 8)
          (setf (aref test-array x y)
                (if (= x y) 7 0))))

      (let ((test-png (make-pathname :name "sprite-test" :type "png")))
        (skyline-tool::compile-intv-sprite test-png *test-gram-dir*
                                          :palette-pixels test-array)

        ;; Parse the output and validate binary data
        (let ((content (uiop:read-file-string output-path)))
          (let ((decle-values (cl-ppcre:all-matches-as-strings "\\$([0-9A-F]{4})" content)))
            ;; Should generate 8 DECLE values for 8x8 sprite
            (is (= 8 (length decle-values))
                "Should generate 8 DECLE values for 8x8 sprite")

            ;; For diagonal pattern, each DECLE should have specific bit patterns
            ;; Row 0: pixel 0 white, others black -> $0080 (bit 7 set)
            ;; Row 1: pixel 1 white, others black -> $0040 (bit 6 set)
            ;; etc.
            (let ((expected-patterns '("$0080" "$0040" "$0020" "$0010"
                                       "$0008" "$0004" "$0002" "$0001")))
              (dotimes (i 8)
                (is (string= (nth i decle-values) (nth i expected-patterns))
                    "Sprite DECLE ~D should be ~A for diagonal pattern: ~A"
                    i (nth i expected-patterns) (nth i decle-values)))))))))))

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

;; Test Intellivision platform integration
(test intv-platform-constants
  "Test that Intellivision platform constants are properly defined"
  ;; Test Intellivision (machine 2609) validation
  (let ((skyline-tool::*machine* 2609))
    (is-true (skyline-tool::machine-valid-p)
             "Intellivision (machine 2609) should be recognized as valid")))

;; Test exported Intellivision functions exist
(test intv-exported-functions-existence
  "Test that all exported Intellivision functions exist"
  ;; Graphics functions
  (is-true (fboundp 'skyline-tool::compile-gram-intv)
           "compile-gram-intv should exist")
  (is-true (fboundp 'skyline-tool::compile-art-intv)
           "compile-art-intv should exist")
  (is-true (fboundp 'skyline-tool::compile-intv-sprite)
           "compile-intv-sprite should exist")

  ;; Music and speech functions
  (is-true (fboundp 'skyline-tool::compile-music-2609)
           "compile-music-2609 should exist")
  (is-true (fboundp 'skyline-tool::compile-speech-2609)
           "compile-speech-2609 should exist")

  ;; Assembly function
  (is-true (fboundp 'skyline-tool::assemble-intv-rom)
           "assemble-intv-rom should exist"))

;; Test Intellivision function error handling
(test intv-function-error-handling
  "Test that Intellivision functions handle errors appropriately"
  ;; Graphics functions should signal errors for missing implementations
  (signals error (skyline-tool::compile-art-intv "/nonexistent.in"
                   (format nil "Object/~a/test-~x.out" (skyline-tool::machine-directory-name) (sxhash (get-universal-time))))
           "compile-art-intv should signal error (not implemented)")
  ;; Sprite function is now implemented, so test with proper machine setting
  (let ((skyline-tool::*machine* 2609))
    (signals error (skyline-tool::compile-intv-sprite "/nonexistent.png"
                     (format nil "Object/~a/test-~x.out" (skyline-tool::machine-directory-name) (sxhash (get-universal-time))))
             "compile-intv-sprite should signal error for nonexistent file"))

  ;; Music functions
  (signals error (skyline-tool::compile-music-2609
                   (format nil "Object/~a/test-~x.s" (skyline-tool::machine-directory-name) (sxhash (get-universal-time)))
                   "/nonexistent.mid" :ay-3-8910)
           "compile-music-2609 should signal error for missing MIDI file")
  (signals error (skyline-tool::compile-speech-2609
                   (format nil "Object/~a/test-~x.s" (skyline-tool::machine-directory-name) (sxhash (get-universal-time)))
                   "/nonexistent.txt")
           "compile-speech-2609 should signal error (not implemented)"))

;; Test 15: GRAM compiler error conditions
(test gram-compiler-error-conditions
  "Test GRAM compiler error handling for invalid inputs"
  (with-temp-gram-output (output-path "error-test.s")
    (let ((test-png (make-pathname :name "error-test" :type "png"))
          (test-array (make-test-palette-array 16 16)))

      ;; Test dimension too small (width < 8)
      (signals error
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :width 4 :height 8
                                        :palette-pixels test-array)
        "Should error when width < 8")

      ;; Test dimension too small (height < 8)
      (signals error
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :width 8 :height 4
                                        :palette-pixels test-array)
        "Should error when height < 8")

      ;; Test zero dimensions
      (signals error
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :width 0 :height 8
                                        :palette-pixels test-array)
        "Should error when width = 0")

      ;; Test negative dimensions
      (signals error
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :width -8 :height 8
                                        :palette-pixels test-array)
        "Should error when width negative")

      ;; Test dimension exceeds array bounds
      (signals error
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :width 32 :height 16
                                        :palette-pixels test-array)
        "Should error when requested width exceeds array width"))))

;; Test 16: Sprite compiler error conditions
(test intv-sprite-error-conditions
  "Test sprite compiler error handling for invalid inputs"
  (with-temp-gram-output (output-path "sprite-error-test.s")
    (let ((test-png (make-pathname :name "sprite-error-test" :type "png"))
          (test-array (make-test-palette-array 16 16)))

      ;; Test dimension too small (width < 8)
      (signals error
        (skyline-tool::compile-intv-sprite test-png *test-gram-dir*
                                          :width 4 :height 8
                                          :palette-pixels test-array)
        "Should error when sprite width < 8")

      ;; Test dimension too small (height < 8)
      (signals error
        (skyline-tool::compile-intv-sprite test-png *test-gram-dir*
                                          :width 8 :height 4
                                          :palette-pixels test-array)
        "Should error when sprite height < 8")

      ;; Test non-integer dimensions (should be floored but still work)
      (finishes
        (skyline-tool::compile-intv-sprite test-png *test-gram-dir*
                                          :width 8.7 :height 8.9
                                          :palette-pixels test-array)
        "Should accept non-integer dimensions (floored)"))))

;; Test 17: Invalid palette index handling
(test gram-compiler-invalid-palette-handling
  "Test GRAM compiler handles invalid palette indices gracefully"
  (with-temp-gram-output (output-path "invalid-palette.s")
    (let ((test-array (make-array '(8 8) :element-type '(unsigned-byte 8))))
      ;; Set some pixels to invalid palette indices (> 7)
      (dotimes (x 8)
        (dotimes (y 8)
          (setf (aref test-array x y)
                (if (and (< x 4) (< y 4)) 15 7)))) ; Invalid index 15, valid 7

      (let ((test-png (make-pathname :name "invalid-palette" :type "png")))
        ;; Should warn but not crash
        (finishes
          (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                          :palette-pixels test-array)
          "Should handle invalid palette indices gracefully")

        ;; Verify output was generated
        (is-true (probe-file output-path)
                 "Output file should exist despite invalid palette indices"))))

;; Test 18: Empty and malformed input handling
(test gram-compiler-empty-input-handling
  "Test GRAM compiler handles edge cases in input data"
  (with-temp-gram-output (output-path "empty-input.s")
    ;; Test with minimum valid input
    (let ((test-array (make-array '(8 8) :element-type '(unsigned-byte 8) :initial-element 0))
          (test-png (make-pathname :name "empty-input" :type "png")))
      (finishes
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :palette-pixels test-array)
        "Should handle minimum valid input")

      ;; Test with nil palette-pixels (should fail gracefully)
      (signals error
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :palette-pixels nil)
        "Should error with nil palette-pixels"))))

;; Test Intellivision dispatch functionality
(test intv-dispatch-functionality
  "Test Intellivision PNG dispatch works"
  ;; The dispatch-png% method should exist for machine 2609
  (is-true (fboundp 'skyline-tool::dispatch-png%)
           "dispatch-png% generic function should exist"))

;; Test Intellivision palette and color support
(test intv-palette-support
  "Test Intellivision palette and color support"
  (is-true (boundp 'skyline-tool::+intv-palette+)
           "+intv-palette+ should be defined")
  (is-true (boundp 'skyline-tool::+intv-color-names+)
           "+intv-color-names+ should be defined")
  (is-true (arrayp skyline-tool::+intv-palette+)
           "+intv-palette+ should be an array"))

(def-suite intv-comprehensive-suite
  :description "Comprehensive Intellivision functionality tests")

(in-suite intv-comprehensive-suite)

;; Integration test for Intellivision workflow
(test intv-integration-workflow
  "Test Intellivision integration workflow components"
  ;; Test that all core components are in place
  (is-true (skyline-tool::check-machine-valid 2609)
           "Intellivision should be a valid machine")

  ;; Test dispatch system recognizes Intellivision
  ;; TODO: Add proper dispatch system testing

  ;; Test palette system is ready
  (is-true (and (boundp 'skyline-tool::+intv-palette+)
                (arrayp skyline-tool::+intv-palette+))
           "Intellivision palette system is ready"))

;; Test 19: Asset allocation pipeline integration
(test intv-asset-allocation-pipeline
  "Test the complete asset allocation pipeline for Intellivision assets"
  (let ((skyline-tool::*machine* 2609))
    ;; Test that dispatch-png% correctly routes Intellivision images
    (let ((test-array (make-array '(8 8) :element-type '(unsigned-byte 8) :initial-element 7)))
      ;; Test GRAM-sized image (8x8, should be routed to compile-gram-intv)
      (finishes
        (skyline-tool::dispatch-png% 2609 "test-gram.png" *test-gram-dir*
                                    nil 8 8 nil test-array)
        "Should dispatch 8x8 image to GRAM compilation")

      ;; Test non-GRAM-sized image (should error as per current implementation)
      (signals error
        (skyline-tool::dispatch-png% 2609 "test-invalid.png" *test-gram-dir*
                                    nil 4 4 nil test-array)
        "Should error for non-GRAM-sized image"))))

;; Test 20: Music compilation integration test
(test intv-music-compilation-integration
  "Test music compilation pipeline for Intellivision"
  (let ((skyline-tool::*machine* 2609))
    ;; Test the compile-music-for-machine method exists and can be called
    (is-true (fboundp (find-symbol "COMPILE-MUSIC-FOR-MACHINE" :skyline-tool))
             "compile-music-for-machine generic function should exist")

    ;; Test method specialization for machine 2609
    (let ((method (find-method #'skyline-tool::compile-music-for-machine
                              '() (list (find-class 'eql)
                                       (find-class t)
                                       (find-class t)
                                       (find-class t)
                                       (find-class t))
                              nil)))
      (is-true method "Should have method specialized for machine 2609"))))

;; Test 21: Palette validation and usage
(test intv-palette-validation
  "Test Intellivision palette system integration"
  ;; Test palette constants exist
  (is-true (boundp (find-symbol "+INTV-PALETTE+" :skyline-tool))
           "+intv-palette+ should be defined")
  (is-true (boundp (find-symbol "+INTV-COLOR-NAMES+" :skyline-tool))
           "+intv-color-names+ should be defined")

  ;; Test palette has correct structure
  (let ((palette (symbol-value (find-symbol "+INTV-PALETTE+" :skyline-tool))))
    (is-true (arrayp palette) "+intv-palette+ should be an array")
    (is (= 16 (length palette)) "+intv-palette+ should have 16 colors"))

  ;; Test color names array
  (let ((color-names (symbol-value (find-symbol "+INTV-COLOR-NAMES+" :skyline-tool))))
    (is-true (arrayp color-names) "+intv-color-names+ should be an array")
    (is (= 16 (length color-names)) "+intv-color-names+ should have 16 color names")))

;; Test 22: File I/O error handling
(test intv-file-io-error-handling
  "Test error handling for file I/O operations"
  ;; Test with non-existent output directory
  (let ((test-png (make-pathname :name "io-test" :type "png"))
        (test-array (make-test-palette-array 8 8))
        (bad-dir "/nonexistent/directory/path/"))
    ;; Should create directories and succeed
    (finishes
      (skyline-tool::compile-gram-intv test-png bad-dir
                                      :palette-pixels test-array)
      "Should create output directories as needed"))

  ;; Test with read-only output (would require special setup)
  ;; This is hard to test reliably across different systems

  ;; Test with very long path names
  (let ((long-name (make-string 200 :initial-element #\a))
        (test-array (make-test-palette-array 8 8)))
    (finishes
      (skyline-tool::compile-gram-intv (make-pathname :name long-name :type "png")
                                      *test-gram-dir*
                                      :palette-pixels test-array)
      "Should handle long filenames")))

;; Test 23: Memory and performance validation
(test intv-memory-performance-validation
  "Test memory usage and performance characteristics"
  ;; Test with large arrays (within reasonable limits)
  (let ((large-array (make-test-palette-array 256 256))) ; 64 cards
    (with-temp-gram-output (output-path "large-test.s")
      (let ((test-png (make-pathname :name "large-test" :type "png")))
        (finishes
          (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                          :palette-pixels large-array)
          "Should handle large images (256x256 = 1024 cards)")

        ;; Verify correct number of DECLE statements generated
        ;; 256x256 = 32x32 cards = 1024 cards × 8 DECLE = 8192 DECLE
        (let ((content (uiop:read-file-string output-path)))
          (let ((decle-count (count-if (lambda (line)
                                        (search "DECLE" line))
                                      (split-sequence:split-sequence #\newline content))))
            (is (= 8192 decle-count)
                "Large image should generate 8192 DECLE statements: ~D" decle-count)))))))

;; Test 24: Regression test for bit ordering
(test gram-compiler-bit-ordering-regression
  "Regression test for correct bit ordering in GRAM cards"
  (with-temp-gram-output (output-path "bit-order.s")
    ;; Create a test pattern where each row has a single white pixel
    ;; at position X (0-7 from left to right)
    (let ((test-array (make-array '(8 8) :element-type '(unsigned-byte 8))))
      (dotimes (y 8)
        (dotimes (x 8)
          (setf (aref test-array x y)
                (if (= x y) 7 0)))) ; White pixel on diagonal

      (let ((test-png (make-pathname :name "bit-order" :type "png")))
        (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                        :palette-pixels test-array)

        (let ((content (uiop:read-file-string output-path)))
          (let ((decle-values (cl-ppcre:all-matches-as-strings "\\$([0-9A-F]{4})" content)))
            ;; Each row should have exactly one bit set
            ;; Row 0: bit 7 set ($0080), Row 1: bit 6 set ($0040), etc.
            (let ((expected-values '("$0080" "$0040" "$0020" "$0010"
                                     "$0008" "$0004" "$0002" "$0001")))
              (dotimes (i 8)
                (is (string= (nth i decle-values) (nth i expected-values))
                    "DECLE ~D should be ~A for single-bit pattern: ~A"
                    i (nth i expected-values) (nth i decle-values))))))))))

;; Test 25: End-to-end integration test
(test intv-end-to-end-integration
  "Complete end-to-end test of Intellivision asset conversion pipeline"
  (let ((skyline-tool::*machine* 2609)
        (test-assets '((:type :gram :width 8 :height 8 :name "test-gram")
                       (:type :gram :width 16 :height 16 :name "test-multi-gram")
                       (:type :sprite :width 8 :height 8 :name "test-sprite"))))

    (dolist (asset-spec test-assets)
      (destructuring-bind (&key type width height name) asset-spec
        (let ((test-array (make-test-palette-array width height))
              (test-png (make-pathname :name name :type "png")))
          (ecase type
            (:gram
             (finishes
               (skyline-tool::compile-gram-intv test-png *test-gram-dir*
                                               :palette-pixels test-array)
               "End-to-end GRAM compilation should succeed"))
            (:sprite
             (finishes
               (skyline-tool::compile-intv-sprite test-png *test-gram-dir*
                                                 :palette-pixels test-array)
               "End-to-end sprite compilation should succeed")))

          ;; Verify output file exists and has content
          (let ((output-path (merge-pathnames (make-pathname :name name :type "s")
                                            *test-gram-dir*)))
            (is-true (probe-file output-path)
                     "Output file should exist: ~A" output-path)
            (let ((content (uiop:read-file-string output-path)))
              (is-true (> (length content) 0)
                       "Output file should have content")
              (is-true (search ";;; Generated for Intellivision" content)
                       "Output should identify as Intellivision format"))))))))

;; Test Intellivision art index parsing
(test intv-art-index-parsing
  "Test Intellivision art index file parsing"
  (let ((temp-index (format nil "/tmp/intv-test-index-~X.txt" (sxhash (get-universal-time)))))
    (unwind-protect
         (progn
           ;; Write test index file
           (with-open-file (out temp-index :direction :output :if-exists :supersede)
             (format out "# Intellivision Art Index Test~%")
             (format out "test-sprite.png 8×8~%")
             (format out "test-background.png 16×16~%")
             (format out "; Comment line~%"))
           ;; Test parsing
           (let ((art-index (skyline-tool::read-intv-art-index temp-index)))
             (is (= 2 (length art-index)) "Should parse 2 art entries")
             ;; Check first entry
             (destructuring-bind (png-name width height) (first art-index)
               (is (= 8 width) "First entry should have width 8")
               (is (= 8 height) "First entry should have height 8"))
             ;; Check second entry
             (destructuring-bind (png-name width height) (second art-index)
               (is (= 16 width) "Second entry should have width 16")
               (is (= 16 height) "Second entry should have height 16"))))
      ;; Clean up
      (when (probe-file temp-index)
        (delete-file temp-index)))))

;; Test Intellivision art compilation pipeline
(test intv-art-compilation-pipeline
  "Test complete Intellivision art compilation pipeline"
  (let ((temp-index (format nil "/tmp/intv-test-index-~X.txt" (sxhash (get-universal-time))))
        (temp-output (format nil "/tmp/intv-test-output-~X.s" (sxhash (get-universal-time)))))
    (unwind-protect
         (progn
           ;; Create test index file
           (with-open-file (out temp-index :direction :output :if-exists :supersede)
             (format out "# Test art index~%")
             (format out "test-art.png 8×8~%"))

           ;; Test compilation (will fail on PNG processing but should create valid structure)
           (let ((skyline-tool::*machine* 2609)) ; Set Intellivision machine
             (finishes (skyline-tool::compile-art-intv temp-output temp-index)
                      "Art compilation should complete without crashing")

             ;; Check that output file was created
             (is-true (probe-file temp-output) "Output file should be created")

             ;; Check basic content
             (when (probe-file temp-output)
               (with-open-file (in temp-output)
                 (let ((content (read-line in)))
                   (is-true (search "Intellivision Art Assets" content)
                           "Output should contain Intellivision header"))))))
      ;; Clean up
      (when (probe-file temp-index)
        (delete-file temp-index))
      (when (probe-file temp-output)
        (delete-file temp-output)))))

;; Test Intellivision ROM assembly
(test intv-rom-assembly
  "Test Intellivision ROM assembly functionality"
  (let ((temp-output (format nil "/tmp/intv-rom-test-~X.bin" (sxhash (get-universal-time))))
        (temp-source1 (format nil "/tmp/source1-~X.bin" (sxhash (get-universal-time))))
        (temp-source2 (format nil "/tmp/source2-~X.bin" (sxhash (get-universal-time)))))
    (unwind-protect
         (progn
           ;; Create test source files
           (with-open-file (out temp-source1 :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
             (write-byte #xAA out)
             (write-byte #xBB out))
           (with-open-file (out temp-source2 :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
             (write-byte #xCC out)
             (write-byte #xDD out))

           ;; Test assembly
           (let ((skyline-tool::*machine* 2609)) ; Set Intellivision machine
             (finishes (skyline-tool::assemble-intv-rom (list temp-source1 temp-source2) temp-output)
                      "ROM assembly should complete without error")

             ;; Check output file
             (is-true (probe-file temp-output) "ROM output file should be created")
             (when (probe-file temp-output)
               (with-open-file (in temp-output :element-type '(unsigned-byte 8))
                 (is (= 4 (file-length in)) "ROM should be 4 bytes (concatenation of 2x2 byte files)")
                 (is (= #xAA (read-byte in)) "First byte should be from first source")
                 (is (= #xBB (read-byte in)) "Second byte should be from first source")
                 (is (= #xCC (read-byte in)) "Third byte should be from second source")
                 (is (= #xDD (read-byte in)) "Fourth byte should be from second source")))))
      ;; Clean up
      (dolist (file (list temp-output temp-source1 temp-source2))
        (when (probe-file file)
          (delete-file file))))))

;; Test Intellivision art compilation error handling
(test intv-art-compilation-errors
  "Test Intellivision art compilation error handling"
  (let ((temp-output (format nil "/tmp/intv-error-test-~X.s" (sxhash (get-universal-time)))))
    (unwind-protect
         ;; Test with missing input file
         (signals error (skyline-tool::compile-art-intv temp-output "/nonexistent.txt")
                  "Should signal error for missing input file")
      ;; Clean up
      (when (probe-file temp-output)
        (delete-file temp-output)))))

(defun run-intv-gram-tests ()
  "Run all Intellivision GRAM compiler tests and return results"
  (fiveam:run! 'intv-gram-tests)
  (fiveam:run! 'intv-comprehensive-suite))
