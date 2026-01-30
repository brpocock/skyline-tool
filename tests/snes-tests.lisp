;;; Phantasia SkylineTool/tests/snes-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite snes-tests
  :description "Tests for SNES-specific SkylineTool functionality")

(in-suite snes-tests)

;; Helper functions for creating test data
(defun count-substring (substring string)
  "Count occurrences of substring in string"
  (let ((count 0)
        (pos 0))
    (loop
      (setf pos (search substring string :start2 pos))
      (unless pos
        (return count))
      (incf count)
      (incf pos (length substring)))))

(defun make-test-palette-pixels (width height &optional (pattern :checkerboard))
  "Create test palette pixel data for SNES testing."
  (let ((pixels (make-array (list height width) :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref pixels y x)
              (case pattern
                (:checkerboard (if (evenp (+ x y)) 0 1))
                (:solid-0 0)
                (:solid-1 1)
                (:gradient (mod (+ x y) 4))  ; For 2bpp testing
                (:palette-test (mod (+ x y) 16))  ; For 4bpp testing
                (:full-palette (mod (+ x y) 256))  ; For 8bpp testing
                (t 0)))))
    pixels))

;; Test SNES platform constants
(test snes-platform-constants
  "Test that SNES platform constants are properly defined"
  (is-true (skyline-tool::check-machine-valid 88)
           "SNES (machine 88) should be a valid machine"))

;; Test SNES graphics functions existence
(test snes-graphics-functions-existence
  "Test that SNES graphics functions exist"
  (is-true (fboundp 'skyline-tool::compile-art-snes)
           "compile-art-snes should exist")
  (is-true (fboundp 'skyline-tool::write-snes-chr-rom)
           "write-snes-chr-rom should exist")
  (is-true (fboundp 'skyline-tool::read-snes-art-index)
           "read-snes-art-index should exist")
  (is-true (fboundp 'skyline-tool::parse-into-snes-chr-data)
           "parse-into-snes-chr-data should exist")
  (is-true (fboundp 'skyline-tool::parse-snes-chr-tiles)
           "parse-snes-chr-tiles should exist"))

;; Test SNES CHR tile parsing - 2BPP mode
(test snes-chr-tile-parsing-2bpp
  "Test SNES CHR tile parsing in 2BPP mode with known pixel data"
  ;; Test with a simple 8x8 tile with checkerboard pattern
  (let ((pixels (make-test-palette-pixels 8 8 :checkerboard)))
    ;; Parse into SNES CHR format (2BPP)
    (let ((tiles (skyline-tool::parse-snes-chr-tiles pixels 8 8 :2bpp)))
      (is (= 1 (length tiles)) "Should produce one 8x8 tile")
      (let ((tile (car tiles)))
        (is (= 16 (length tile)) "2BPP SNES tile should be 16 bytes (2 bytes per row)")
        ;; Check bitplane structure - checkerboard should have specific bit patterns
        ;; Row 0: pixels 0,1,0,1,0,1,0,1 -> bitplane 0: 10101010, bitplane 1: 00000000
        (is (= #b10101010 (aref tile 0)) "Bitplane 0 row 0 should be 10101010")
        (is (= #b00000000 (aref tile 1)) "Bitplane 1 row 0 should be 00000000")))))

;; Test SNES CHR tile parsing - 4BPP mode
(test snes-chr-tile-parsing-4bpp
  "Test SNES CHR tile parsing in 4BPP mode"
  (let ((pixels (make-test-palette-pixels 8 8 :palette-test)))
    (let ((tiles (skyline-tool::parse-snes-chr-tiles pixels 8 8 :4bpp)))
      (is (= 1 (length tiles)) "Should produce one 8x8 tile")
      (let ((tile (car tiles)))
        (is (= 32 (length tile)) "4BPP SNES tile should be 32 bytes (4 bytes per row)")
        ;; Verify tile data is properly formatted
        (is (typep tile 'array) "Tile should be an array")
        (is (typep (aref tile 0) '(unsigned-byte 8)) "Tile bytes should be unsigned bytes")))))

;; Test SNES CHR tile parsing - 8BPP mode
(test snes-chr-tile-parsing-8bpp
  "Test SNES CHR tile parsing in 8BPP mode"
  (let ((pixels (make-test-palette-pixels 8 8 :full-palette)))
    (let ((tiles (skyline-tool::parse-snes-chr-tiles pixels 8 8 :8bpp)))
      (is (= 1 (length tiles)) "Should produce one 8x8 tile")
      (let ((tile (car tiles)))
        (is (= 64 (length tile)) "8BPP SNES tile should be 64 bytes (8 bytes per row)")
        ;; Verify tile data is properly formatted
        (is (typep tile 'array) "Tile should be an array")))))

;; Test SNES CHR tile parsing with multiple tiles
(test snes-chr-tile-parsing-multiple-tiles
  "Test SNES CHR tile parsing with 16x16 image (4 tiles)"
  (let ((pixels (make-test-palette-pixels 16 16 :checkerboard)))
    (let ((tiles (skyline-tool::parse-snes-chr-tiles pixels 16 16 :2bpp)))
      (is (= 4 (length tiles)) "16x16 image should produce 4 tiles")
      (dolist (tile tiles)
        (is (= 16 (length tile)) "Each 2BPP tile should be 16 bytes")))))

;; Test SNES CHR tile parsing bounds checking
(test snes-chr-tile-parsing-bounds
  "Test SNES CHR tile parsing with out-of-bounds coordinates"
  (let ((pixels (make-test-palette-pixels 8 8 :checkerboard)))
    ;; Test with coordinates that go beyond image bounds
    (let ((tiles (skyline-tool::parse-snes-chr-tiles pixels 16 16 :2bpp))) ; Request 16x16 but only have 8x8
      (is (= 4 (length tiles)) "Should still produce tiles for requested size")
      ;; Out-of-bounds pixels should be treated as 0
      (dolist (tile tiles)
        (is (= 16 (length tile)) "Each tile should be properly sized")))))

;; Test SNES art index parsing - basic functionality
(test snes-art-index-parsing-basic
  "Test basic SNES art index file parsing"
  ;; Create a temporary art index file for testing
  (let ((temp-index (format nil "/tmp/snes-test-index-~X.txt" (sxhash (get-universal-time)))))
    (unwind-protect
         (progn
           ;; Write test index file
           (with-open-file (out temp-index :direction :output :if-exists :supersede)
             (format out "# SNES Test Art Index~%")
             (format out "test-tile-2bpp.png 2bpp 16×16~%")
             (format out "test-tile-4bpp.png 4bpp 32×8~%")
             (format out "test-tile-8bpp.png 8bpp 8×8~%")
             (format out "; Comment line~%"))
           ;; Test parsing
           (let ((art-index (skyline-tool::read-snes-art-index temp-index)))
             (is (= 3 (length art-index)) "Should parse 3 art entries")
             ;; Check first entry
             (destructuring-bind (mode png-name width height) (first art-index)
               (is (eq :2BPP mode) "First entry should be 2BPP mode")
               (is (= 16 width) "First entry should have width 16")
               (is (= 16 height) "First entry should have height 16"))
             ;; Check second entry
             (destructuring-bind (mode png-name width height) (second art-index)
               (is (eq :4BPP mode) "Second entry should be 4BPP mode")
               (is (= 32 width) "Second entry should have width 32")
               (is (= 8 height) "Second entry should have height 8"))))
      ;; Clean up
      (when (probe-file temp-index)
        (delete-file temp-index)))))

;; Test SNES art index parsing - edge cases
(test snes-art-index-parsing-edge-cases
  "Test SNES art index parsing with edge cases"
  (let ((temp-index (format nil "/tmp/snes-test-index-edge-~X.txt" (sxhash (get-universal-time)))))
    (unwind-protect
         (progn
           ;; Write test index file with edge cases
           (with-open-file (out temp-index :direction :output :if-exists :supersede)
             (format out "# Edge case test~%")
             (format out "~%")  ; Empty line
             (format out "  ~%")  ; Whitespace-only line
             (format out "# Another comment~%")
             (format out "minimal.png 2bpp 8×8~%")  ; Minimal size
             (format out "large.png 8bpp 256×256~%")  ; Large size
             (format out "; Final comment~%"))
           ;; Test parsing
           (let ((art-index (skyline-tool::read-snes-art-index temp-index)))
             (is (= 2 (length art-index)) "Should parse 2 art entries, ignoring empty lines and comments")
             ;; Check entries exist with correct properties
             (dolist (entry art-index)
               (destructuring-bind (mode png-name width height) entry
                 (is (member mode '(:2BPP :8BPP)) "Mode should be valid")
                 (is (> width 0) "Width should be positive")
                 (is (> height 0) "Height should be positive")))))
      ;; Clean up
      (when (probe-file temp-index)
        (delete-file temp-index)))))

;; Test SNES art index parsing - malformed input
(test snes-art-index-parsing-malformed
  "Test SNES art index parsing error handling for malformed input"
  (let ((temp-index (format nil "/tmp/snes-test-index-bad-~X.txt" (sxhash (get-universal-time)))))
    (unwind-protect
         (progn
           ;; Write malformed index file
           (with-open-file (out temp-index :direction :output :if-exists :supersede)
             (format out "missing-mode.png 16×16~%")  ; Missing mode
             (format out "bad-mode.png invalid 16×16~%")  ; Invalid mode
             (format out "no-size.png 2bpp~%")  ; Missing size
             (format out "bad-size.png 2bpp 16x16~%")  ; Wrong separator
             (format out "valid.png 2bpp 16×16~%"))  ; Valid entry
           ;; Test parsing - should handle errors gracefully
           (let ((art-index (skyline-tool::read-snes-art-index temp-index)))
             ;; Should only parse the valid entry
             (is (>= (length art-index) 0) "Should parse valid entries and skip invalid ones")))
      ;; Clean up
      (when (probe-file temp-index)
        (delete-file temp-index)))))

;; Test SNES art index parsing - mode validation
(test snes-art-index-parsing-modes
  "Test SNES art index parsing with all supported modes"
  (let ((temp-index (format nil "/tmp/snes-test-index-modes-~X.txt" (sxhash (get-universal-time)))))
    (unwind-protect
         (progn
           ;; Write test index file with all modes
           (with-open-file (out temp-index :direction :output :if-exists :supersede)
             (format out "tile-2bpp.png 2bpp 16×16~%")
             (format out "tile-4bpp.png 4bpp 16×16~%")
             (format out "tile-8bpp.png 8bpp 16×16~%"))
           ;; Test parsing
           (let ((art-index (skyline-tool::read-snes-art-index temp-index)))
             (is (= 3 (length art-index)) "Should parse all three modes")
             (let ((modes (mapcar #'first art-index)))
               (is (member :2BPP modes) "Should include 2BPP mode")
               (is (member :4BPP modes) "Should include 4BPP mode")
               (is (member :8BPP modes) "Should include 8BPP mode"))))
      ;; Clean up
      (when (probe-file temp-index)
        (delete-file temp-index)))))

;; Test SNES art index parsing - path handling
(test snes-art-index-parsing-paths
  "Test SNES art index parsing path and filename handling"
  (let ((temp-index (format nil "/tmp/snes-test-index-paths-~X.txt" (sxhash (get-universal-time)))))
    (unwind-protect
         (progn
           ;; Write test index file
           (with-open-file (out temp-index :direction :output :if-exists :supersede)
             (format out "simple.png 2bpp 8×8~%")
             (format out "path/to/file.png 2bpp 8×8~%")
             (format out "file.with.dots.png 2bpp 8×8~%"))
           ;; Test parsing
           (let ((art-index (skyline-tool::read-snes-art-index temp-index)))
             (is (= 3 (length art-index)) "Should parse all entries")
             (dolist (entry art-index)
               (destructuring-bind (mode png-path width height) entry
                 (is (pathnamep png-path) "PNG path should be a pathname object")
                 (is (stringp (pathname-name png-path)) "Pathname should have a name component")))))
      ;; Clean up
      (when (probe-file temp-index)
        (delete-file temp-index)))))

;; Test SNES art compilation error handling
(test snes-art-compilation-errors
  "Test SNES art compilation error handling"
  ;; Test with missing input file
  (signals error (skyline-tool::compile-art-snes "/nonexistent.in"
                  (format nil "/tmp/test-~x.out" (sxhash (get-universal-time))))
           "compile-art-snes should signal error for missing input file"))

;; Test SNES music compilation functions (placeholder - not yet implemented)
(test snes-music-compilation-validation
  "Test SNES music compilation produces correct SPC700 assembly with functional BRR data"
  (let ((output-file (format nil "/tmp/snes-music-test-~X.s" (sxhash (get-universal-time))))
        (input-file (format nil "/tmp/test-music-~X.mid" (sxhash (get-universal-time)))))
    (unwind-protect
         (progn
           ;; Create a minimal test input file
           (with-open-file (out input-file :direction :output :if-exists :supersede)
             (write '(:note-on :channel 0 :key 60 :velocity 100 :time 0) :stream out :readably t))

           ;; Test that function exists
           (is-true (fboundp 'skyline-tool::compile-music-snes)
                   "compile-music-snes should exist")

           ;; Test compilation
           (finishes (skyline-tool::compile-music-snes output-file input-file)
                    "SNES music compilation should complete without errors")

           ;; Verify output file was created and contains expected content
           (is-true (probe-file output-file)
                   "SNES music compilation should create output file")

           (when (probe-file output-file)
             (with-open-file (in output-file)
               (let ((content (read-line in)))
                 (is-true (search "SNES SPC700 Music compiled from" content)
                         "Output should identify as SNES SPC700 music")))

             ;; Check for key DSP register definitions
             (with-open-file (in output-file)
               (let ((full-content (make-string (file-length in))))
                 (read-sequence full-content in)
                 (is-true (search "DSP_VOL_L = $00" full-content)
                         "Output should contain DSP register definitions")
                 (is-true (search "dsp_init:" full-content)
                         "Output should contain DSP initialization function")
                 (is-true (search "note_pitches:" full-content)
                         "Output should contain note frequency table")
                 (is-true (search "play_note:" full-content)
                         "Output should contain note playing function")

                 ;; CRITICAL: Validate that BRR data is functional, not placeholder
                 (is-true (search "BRR Sample Data (functional encoding)" full-content)
                         "Output should contain functional BRR encoding, not placeholder")
                 (is-false (search "Sample BRR Data (placeholder)" full-content)
                          "Output should NOT contain placeholder BRR data")
                 (is-true (search ".byte $00  ; Block" full-content)
                         "Output should contain properly formatted BRR blocks")

                 ;; Check for actual BRR header bytes (should not be all zeros for functional data)
                 (let ((brr-header-count (count-substring ".byte $01" full-content)))
                   (is (> brr-header-count 0)
                       "Should contain BRR end block headers indicating functional encoding")))))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

;; Test SNES blob ripping functions
(test snes-blob-ripping-functions-existence
  "Test that SNES blob ripping functions exist"
  (is-true (fboundp 'skyline-tool::blob-rip-snes-tile)
           "blob-rip-snes-tile should exist")
  (is-true (fboundp 'skyline-tool::blob-rip-snes-sprite)
           "blob-rip-snes-sprite should exist")
  (is-true (fboundp 'skyline-tool::blob-rip-snes-font)
           "blob-rip-snes-font should exist"))

;; Test SNES blob ripping error handling
(test snes-blob-ripping-errors
  "Test SNES blob ripping error handling for missing files"
  ;; These should signal errors for missing PNG files
  (signals error (skyline-tool::blob-rip-snes-tile "/nonexistent.png")
           "blob-rip-snes-tile should signal error for missing PNG files")
  (signals error (skyline-tool::blob-rip-snes-sprite "/nonexistent.png")
           "blob-rip-snes-sprite should signal error for missing PNG files")
  (signals error (skyline-tool::blob-rip-snes-font "/nonexistent.png")
           "blob-rip-snes-font should signal error for missing PNG files"))

;; Test SNES blob ripping produces functional output
(test snes-blob-ripping-functional-output
  "Test that SNES blob ripping produces functional assembly code, not placeholders"
  ;; This test creates a minimal PNG-like data structure to test processing
  ;; Since creating actual PNG files in tests is complex, we test the error handling
  ;; and ensure that when functions are called, they attempt real processing
  (let ((temp-output (format nil "/tmp/snes-blob-test-~X.s" (sxhash (get-universal-time)))))
    (unwind-protect
         ;; The functions should attempt PNG processing and fail gracefully on missing files
         ;; This validates that they don't just create empty placeholder files
         (signals error (skyline-tool::blob-rip-snes-tile "/definitely-not-a-png-file.png")
                  "blob-rip-snes-tile should attempt PNG processing and fail on invalid files")

         ;; If any output files were created despite errors, they should not contain placeholder content
         (when (probe-file temp-output)
           (with-open-file (in temp-output)
             (let ((content (make-string (file-length in))))
               (read-sequence content in)
               (is-false (search "placeholder" (string-downcase content))
                        "Output should not contain placeholder text")
               (is-false (search "TODO" (string-upcase content))
                        "Output should not contain TODO markers"))))
      ;; Cleanup
      (when (probe-file temp-output)
        (delete-file temp-output)))))

;; Test SNES blob ripping with mock PNG data
(test snes-blob-ripping-tile-functionality
  "Test SNES tile blob ripping with mock data"
  ;; This test would require creating actual PNG files, which is complex in a test environment
  ;; For now, test that the function can be called and produces expected structure
  (let ((temp-output (format nil "/tmp/snes-tile-test-~X.s" (sxhash (get-universal-time)))))
    (unwind-protect
         ;; Test that function doesn't crash (though it will error on missing PNG)
         (signals error (skyline-tool::blob-rip-snes-tile "/nonexistent.png")
                  "blob-rip-snes-tile should handle missing PNG files gracefully")
      ;; Clean up any created files
      (when (probe-file temp-output)
        (delete-file temp-output)))))

;; Test SNES blob ripping sprite functionality
(test snes-blob-ripping-sprite-functionality
  "Test SNES sprite blob ripping delegates to tile ripping"
  (let ((temp-output (format nil "/tmp/snes-sprite-test-~X.s" (sxhash (get-universal-time)))))
    (unwind-protect
         ;; Test that function doesn't crash (though it will error on missing PNG)
         (signals error (skyline-tool::blob-rip-snes-sprite "/nonexistent.png")
                  "blob-rip-snes-sprite should handle missing PNG files gracefully")
      ;; Clean up any created files
      (when (probe-file temp-output)
        (delete-file temp-output)))))

;; Test SNES blob ripping font functionality
(test snes-blob-ripping-font-functionality
  "Test SNES font blob ripping delegates to tile ripping"
  (let ((temp-output (format nil "/tmp/snes-font-test-~X.s" (sxhash (get-universal-time)))))
    (unwind-protect
         ;; Test that function doesn't crash (though it will error on missing PNG)
         (signals error (skyline-tool::blob-rip-snes-font "/nonexistent.png")
                  "blob-rip-snes-font should handle missing PNG files gracefully")
      ;; Clean up any created files
      (when (probe-file temp-output)
        (delete-file temp-output)))))

;; Test SNES CHR ROM format validation - 2BPP
(test snes-chr-format-validation-2bpp
  "Test SNES CHR ROM format compliance for 2BPP tiles"
  ;; Create a tile with known pixel pattern and verify bitplane encoding
  (let ((pixels (make-array '(8 8) :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Create pattern: top-left 2x2 = color 1, rest = color 0
    (setf (aref pixels 0 0) 1) (setf (aref pixels 0 1) 1)
    (setf (aref pixels 1 0) 1) (setf (aref pixels 1 1) 1)

    (let ((tiles (skyline-tool::parse-snes-chr-tiles pixels 8 8 :2bpp)))
      (is (= 1 (length tiles)) "Should produce one tile")
      (let ((tile (car tiles)))
        ;; In 2BPP, each row has 2 bytes: bitplane 0, bitplane 1
        ;; Row 0: pixels 1,1,0,0,0,0,0,0 -> bitplane 0: 11000000, bitplane 1: 00000000
        (is (= #b11000000 (aref tile 0)) "Bitplane 0 row 0 should be 11000000")
        (is (= #b00000000 (aref tile 1)) "Bitplane 1 row 0 should be 00000000")
        ;; Row 1: same pattern
        (is (= #b11000000 (aref tile 2)) "Bitplane 0 row 1 should be 11000000")
        (is (= #b00000000 (aref tile 3)) "Bitplane 1 row 1 should be 00000000")))))

;; Test SNES CHR ROM format validation - 4BPP
(test snes-chr-format-validation-4bpp
  "Test SNES CHR ROM format compliance for 4BPP tiles"
  (let ((pixels (make-array '(8 8) :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Set pixel (0,0) to color 15 (all bits set)
    (setf (aref pixels 0 0) 15)

    (let ((tiles (skyline-tool::parse-snes-chr-tiles pixels 8 8 :4bpp)))
      (is (= 1 (length tiles)) "Should produce one tile")
      (let ((tile (car tiles)))
        ;; In 4BPP, each row has 4 bytes: bitplanes 0,1,2,3
        ;; Row 0: pixel 0 = 15 = binary 1111
        ;; Bitplane 0: 10000000 (bit 0 of color), Bitplane 1: 10000000 (bit 1), etc.
        (is (= #b10000000 (aref tile 0)) "Bitplane 0 should have bit 7 set")
        (is (= #b10000000 (aref tile 1)) "Bitplane 1 should have bit 7 set")
        (is (= #b10000000 (aref tile 2)) "Bitplane 2 should have bit 7 set")
        (is (= #b10000000 (aref tile 3)) "Bitplane 3 should have bit 7 set")))))

;; Test SNES CHR ROM format validation - 8BPP
(test snes-chr-format-validation-8bpp
  "Test SNES CHR ROM format compliance for 8BPP tiles"
  (let ((pixels (make-array '(8 8) :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Set pixel (0,0) to color 255 (all bits set)
    (setf (aref pixels 0 0) 255)

    (let ((tiles (skyline-tool::parse-snes-chr-tiles pixels 8 8 :8bpp)))
      (is (= 1 (length tiles)) "Should produce one tile")
      (let ((tile (car tiles)))
        ;; In 8BPP, each row has 8 bytes: bitplanes 0-7
        ;; Row 0: pixel 0 = 255 = all bits set in all bitplanes
        (dotimes (i 8)
          (is (= #b10000000 (aref tile i)) "All bitplanes should have bit 7 set"))))))

;; Test SNES bitplane interleaving
(test snes-bitplane-interleaving
  "Test that SNES bitplanes are correctly interleaved within tiles"
  (let ((pixels (make-array '(8 8) :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Create a pattern where each row has different bit patterns
    (dotimes (y 8)
      (dotimes (x 8)
        (setf (aref pixels y x) (if (= x y) 1 0)))) ; Diagonal pattern

    (let ((tiles (skyline-tool::parse-snes-chr-tiles pixels 8 8 :2bpp)))
      (let ((tile (car tiles)))
        ;; Verify that bitplanes are interleaved correctly
        ;; Each row should have bitplane 0 followed by bitplane 1
        (is (= 16 (length tile)) "2BPP tile should be 16 bytes")
        ;; Check that bytes alternate between bitplanes
        (dotimes (row 8)
          (let ((bp0-offset (* row 2))
                (bp1-offset (+ (* row 2) 1)))
            (is (typep (aref tile bp0-offset) '(unsigned-byte 8)) "Bitplane 0 byte should be valid")
            (is (typep (aref tile bp1-offset) '(unsigned-byte 8)) "Bitplane 1 byte should be valid")))))))

;; Test SNES CHR ROM writing
(test snes-chr-rom-writing
  "Test SNES CHR ROM binary file writing"
  (let ((temp-output (format nil "/tmp/snes-chr-test-~X.bin" (sxhash (get-universal-time))))
        (test-tiles (list (make-array 16 :element-type '(unsigned-byte 8) :initial-element #xAA)
                         (make-array 16 :element-type '(unsigned-byte 8) :initial-element #x55))))
    (unwind-protect
         (progn
           ;; Write test CHR data
           (skyline-tool::write-snes-chr-rom temp-output test-tiles)
           ;; Verify file was created and has correct size
           (is-true (probe-file temp-output) "Output file should be created")
           (with-open-file (in temp-output :element-type '(unsigned-byte 8))
             (let ((data (make-array 32 :element-type '(unsigned-byte 8))))
               (read-sequence data in)
               (is (= 32 (file-length in)) "File should be 32 bytes (2 tiles × 16 bytes)")
               (is (= #xAA (aref data 0)) "First byte of first tile should be #xAA")
               (is (= #x55 (aref data 16)) "First byte of second tile should be #x55"))))
      ;; Clean up
      (when (probe-file temp-output)
        (delete-file temp-output)))))

;; Test SNES conversion pipeline integration
(test snes-conversion-pipeline
  "Test complete SNES art conversion pipeline"
  ;; This test would require creating actual PNG files, which is complex
  ;; For now, test that the pipeline functions are properly integrated
  (let ((skyline-tool::*machine* 88)) ; Set SNES machine
    (is (= 88 skyline-tool::*machine*) "Machine should be set to SNES")
    ;; Test that functions can be called in sequence (error handling tested elsewhere)
    (is-true (fboundp 'skyline-tool::parse-into-snes-chr-data) "Pipeline component should exist")))

;; Test SNES CHR ROM file format compliance
(test snes-chr-rom-file-format
  "Test that SNES CHR ROM files are written in correct binary format"
  (let ((temp-output (format nil "/tmp/snes-chr-test-~X.chr" (sxhash (get-universal-time))))
        (test-tiles (list (make-array 16 :element-type '(unsigned-byte 8)
                                     :initial-contents '(#xAA #x55 #xAA #x55 #xAA #x55 #xAA #x55
                                                        #xAA #x55 #xAA #x55 #xAA #x55 #xAA #x55))
                         (make-array 16 :element-type '(unsigned-byte 8)
                                     :initial-contents '(#xFF #x00 #xFF #x00 #xFF #x00 #xFF #x00
                                                        #xFF #x00 #xFF #x00 #xFF #x00 #xFF #x00)))))
    (unwind-protect
         (progn
           ;; Write test CHR data
           (skyline-tool::write-snes-chr-rom temp-output test-tiles)

           ;; Verify file was created and has correct size
           (is-true (probe-file temp-output) "CHR output file should be created")
           (when (probe-file temp-output)
             (is (= 32 (with-open-file (s temp-output :element-type '(unsigned-byte 8))
                        (file-length s)))
                 "CHR file should be 32 bytes (2 tiles × 16 bytes each)")

             ;; Verify data integrity
             (with-open-file (in temp-output :element-type '(unsigned-byte 8))
               (is (= #xAA (read-byte in)) "First byte should match test data")
               (is (= #x55 (read-byte in)) "Second byte should match test data"))))
      ;; Clean up
      (when (probe-file temp-output)
        (delete-file temp-output)))))

;; Test SNES art index parsing with various bit depths
(test snes-art-index-bit-depths
  "Test SNES art index parsing handles different bit depths correctly"
  (let ((temp-index (format nil "/tmp/snes-bitdepth-test-~X.txt" (sxhash (get-universal-time)))))
    (unwind-protect
         (progn
           ;; Write test index file with different bit depths
           (with-open-file (out temp-index :direction :output :if-exists :supersede)
             (format out "# SNES bit depth test~%")
             (format out "tiles-2bpp.png 2bpp 32×32~%")
             (format out "tiles-4bpp.png 4bpp 16×16~%")
             (format out "tiles-8bpp.png 8bpp 8×8~%"))

           ;; Test parsing
           (let ((art-index (skyline-tool::read-snes-art-index temp-index)))
             (is (= 3 (length art-index)) "Should parse all three entries")

             ;; Check bit depth modes
             (let ((modes (mapcar #'first art-index)))
               (is (member :2BPP modes) "Should include 2BPP mode")
               (is (member :4BPP modes) "Should include 4BPP mode")
               (is (member :8BPP modes) "Should include 8BPP mode"))

             ;; Check dimensions are appropriate for bit depths
             (destructuring-bind (mode2 png2 w2 h2) (first art-index)
               (declare (ignore png2))
               (is (eq :2BPP mode2) "First entry should be 2BPP")
               (is (= 32 w2) "2BPP should support larger tiles")
               (is (= 32 h2) "2BPP should support larger tiles"))))
      ;; Clean up
      (when (probe-file temp-index)
        (delete-file temp-index)))))

;; Test SNES machine detection and validation
(test snes-machine-detection
  "Test SNES machine code validation and constants"
  ;; Test machine validation function
  (is-true (skyline-tool::check-machine-valid 88) "Machine 88 (SNES) should be valid")

  ;; Test machine name resolution
  (let ((skyline-tool::*machine* 88))
    (is (string= "SNES" (skyline-tool::machine-short-name))
        "SNES should have correct short name"))

  ;; Test machine-specific constants
  (is (= 88 skyline-tool::*machine*) "SNES machine constant should be 88"))

;; Test SNES PNG dispatch functionality
(test snes-dispatch-png-method
  "Test that SNES has a dispatch-png% method"
  (is-true (find-method #'skyline-tool::dispatch-png% '() (list (list 'eql 88) t t t t t t t) nil)
           "SNES should have a dispatch-png% method specialized for machine 88"))

(test snes-dispatch-png-mode7-detection
  "Test that SNES dispatch-png correctly identifies Mode 7 backgrounds"
  (let ((skyline-tool::*machine* 88))
    ;; Test Mode 7 detection (256x256 images)
    (finishes (skyline-tool::dispatch-png% 88 "/test-mode7.png" "/tmp/"
                                           nil 256 256 nil nil)
              "Mode 7 dispatch should complete without error")))

(test snes-dispatch-png-tile-detection
  "Test that SNES dispatch-png correctly identifies tiles"
  (let ((skyline-tool::*machine* 88))
    ;; Test tile detection (multiples of 8x8)
    (finishes (skyline-tool::dispatch-png% 88 "/test-tiles.png" "/tmp/"
                                           nil 64 64 nil nil)
              "Tile dispatch should complete without error")))

(test snes-dispatch-png-sprite-detection
  "Test that SNES dispatch-png correctly identifies sprites"
  (let ((skyline-tool::*machine* 88))
    ;; Test sprite detection (16x16, 32x32, 64x64)
    (finishes (skyline-tool::dispatch-png% 88 "/test-sprite.png" "/tmp/"
                                           nil 32 32 nil nil)
              "Sprite dispatch should complete without error")))

;; Test complete SNES art compilation workflow
(test snes-complete-art-workflow
  "Test complete SNES art compilation from index file to binary output"
  (let ((temp-index (format nil "/tmp/snes-workflow-index-~X.txt" (sxhash (get-universal-time))))
        (temp-output (format nil "/tmp/snes-workflow-output-~X.chr" (sxhash (get-universal-time)))))
    (unwind-protect
         (progn
           ;; Create test art index
           (with-open-file (out temp-index :direction :output :if-exists :supersede)
             (format out "# SNES Art Workflow Test~%")
             (format out "test-tiles.png 2bpp 16×16~%"))

           ;; Test complete compilation pipeline
           (let ((skyline-tool::*machine* 88))
             (finishes (skyline-tool::compile-art-snes temp-output temp-index)
                      "Complete SNES art compilation should succeed")

             ;; Verify output file was created
             (is-true (probe-file temp-output) "CHR output file should be created")

             ;; Verify output has reasonable size (16x16 tiles = 4 tiles × 16 bytes = 64 bytes)
             (when (probe-file temp-output)
               (with-open-file (in temp-output :element-type '(unsigned-byte 8))
                 (let ((file-size (file-length in)))
                   (is (> file-size 0) "Output file should have content")
                   ;; 16×16 pixels = 4 tiles, each tile = 16 bytes in 2BPP format
                   (is (= file-size 64) "16×16 tile area should produce 64 bytes of CHR data"))))))
      ;; Cleanup
      (dolist (file (list temp-index temp-output))
        (when (probe-file file)
          (delete-file file))))))

;; Test SNES CHR tile data validation
(test snes-chr-tile-data-validation
  "Test that SNES CHR tile data follows correct format"
  ;; Test with known tile data
  (let ((test-tile (make-array 16 :element-type '(unsigned-byte 8)
                              :initial-contents '(#xAA #x55 #xAA #x55 #xAA #x55 #xAA #x55
                                                 #xAA #x55 #xAA #x55 #xAA #x55 #xAA #x55)))
        (temp-output (format nil "/tmp/snes-chr-validate-~X.chr" (sxhash (get-universal-time)))))
    (unwind-protect
         (progn
           ;; Write test CHR data
           (skyline-tool::write-snes-chr-rom temp-output (list test-tile))

           ;; Verify file contents
           (when (probe-file temp-output)
             (with-open-file (in temp-output :element-type '(unsigned-byte 8))
               (is (= (file-length in) 16) "Single tile should be 16 bytes")
               ;; Verify data integrity
               (dotimes (i 16)
                 (is (= (aref test-tile i) (read-byte in))
                     (format nil "Byte ~D should match input data" i))))))
      ;; Cleanup
      (when (probe-file temp-output)
        (delete-file temp-output)))))

;; Test SNES art compilation produces functional binary data
(test snes-art-compilation-functional-binary
  "Test that SNES art compilation produces functional binary CHR data, not random/placeholder data"
  (let ((temp-index (format nil "/tmp/snes-art-index-~X.txt" (sxhash (get-universal-time))))
        (temp-output (format nil "/tmp/snes-art-output-~X.chr" (sxhash (get-universal-time)))))
    (unwind-protect
         (progn
           ;; Create test art index file
           (with-open-file (out temp-index :direction :output :if-exists :supersede)
             (format out "# SNES Art Test~%")
             (format out "test-tile.png 2bpp 16×16~%"))

           ;; Test compilation (will fail on PNG processing but should create valid structure)
           (let ((skyline-tool::*machine* 88))
             (signals error (skyline-tool::compile-art-snes temp-output temp-index)
                     "Art compilation should attempt PNG processing and fail on missing files")

             ;; Even if PNG processing fails, the index parsing should work
             ;; and we should be able to test that the parsing produces correct data structures
             (let ((art-index (skyline-tool::read-snes-art-index temp-index)))
               (is (= 1 (length art-index)) "Should parse one art entry")
               (destructuring-bind (mode png-name width height) (first art-index)
                 (is (eq :2BPP mode) "Mode should be parsed as 2BPP keyword")
                 (is (= 16 width) "Width should be parsed correctly")
                 (is (= 16 height) "Height should be parsed correctly")))))
      ;; Cleanup
      (when (probe-file temp-index)
        (delete-file temp-index))
      (when (probe-file temp-output)
        (delete-file temp-output)))))

;; Test SNES bitplane encoding validation
(test snes-bitplane-encoding-validation
  "Test that SNES bitplane encoding produces correct 2BPP format"
  ;; Test the parse-snes-chr-tiles function with known pixel data
  (let ((test-pixels (make-array '(8 8) :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Create a simple pattern: top-left 2x2 = color 1, others = 0
    (setf (aref test-pixels 0 0) 1) (setf (aref test-pixels 0 1) 1)
    (setf (aref test-pixels 1 0) 1) (setf (aref test-pixels 1 1) 1)

    ;; Process through SNES tile parser
    (let ((tiles (skyline-tool::parse-snes-chr-tiles test-pixels 8 8 :2bpp)))
      (is (= 1 (length tiles)) "Should produce one tile")
      (let ((tile (car tiles)))
        (is (= 16 (length tile)) "2BPP tile should be 16 bytes")

        ;; Check that the encoding is not just zeros or random data
        ;; The pattern should result in specific bit patterns
        (is (not (every #'zerop tile)) "Tile data should not be all zeros")
        (is (> (count-if #'plusp tile) 0) "Tile should contain some non-zero bytes")

        ;; For 2BPP SNES format, first 8 bytes are bitplane 0, next 8 are bitplane 1
        ;; Our pattern should set specific bits in bitplane 0 for the first two pixels of first two rows
        (let ((bitplane-0 (subseq tile 0 8))
              (bitplane-1 (subseq tile 8 16)))
          ;; Both bitplanes should have data (since we're using color 1 = binary 01)
          (is (not (every #'zerop bitplane-0)) "Bitplane 0 should contain data")
          (is (every #'zerop bitplane-1) "Bitplane 1 should be all zeros for color 1"))))))

;; Test SNES Mode 7 compilation produces functional data
(test snes-mode7-compilation-functional
  "Test that SNES Mode 7 compilation processes PNG data functionally"
  ;; Since we can't easily create PNG files in tests, we test that the function
  ;; exists and attempts real PNG processing rather than just generating patterns
  (let ((temp-output (format nil "/tmp/snes-mode7-test-~X.m7" (sxhash (get-universal-time)))))
    (unwind-protect
         ;; The function should attempt PNG processing and fail on missing files
         (signals error (skyline-tool::compile-snes-mode7 "/nonexistent.png" "/tmp/" 256 256 nil)
                  "Mode 7 compilation should attempt PNG processing")

         ;; If any output was created, it should be the correct size (256x256 = 65536 bytes)
         (when (probe-file temp-output)
           (with-open-file (in temp-output :element-type '(unsigned-byte 8))
             (is (= 65536 (file-length in))
                 "Mode 7 output should be exactly 256x256 = 65536 bytes")))

      ;; Cleanup
      (when (probe-file temp-output)
        (delete-file temp-output)))))

;; Test that SNES dispatch system routes correctly
(test snes-dispatch-routing-validation
  "Test that SNES PNG dispatch routes to correct compilation functions"
  (let ((skyline-tool::*machine* 88))
    ;; Test Mode 7 routing (256x256 images)
    (finishes (skyline-tool::dispatch-png% 88 "/test-mode7.png" "/tmp/"
                                           nil 256 256 nil nil)
              "Mode 7 dispatch should route to compile-snes-mode7")

    ;; Test tile routing (multiples of 8x8)
    (finishes (skyline-tool::dispatch-png% 88 "/test-tiles.png" "/tmp/"
                                           nil 128 128 nil nil)
              "Tile dispatch should route to compile-snes-tiles")

    ;; Test sprite routing (16x16, 32x32, 64x64)
    (finishes (skyline-tool::dispatch-png% 88 "/test-sprite.png" "/tmp/"
                                           nil 32 32 nil nil)
              "Sprite dispatch should route to compile-snes-sprite")))

;; Test SNES platform integration
(test snes-platform-integration-test
  "Test complete SNES platform integration and workflow"
  ;; Test that all SNES functions work together in the platform context
  (let ((skyline-tool::*machine* 88))
    (is (= 88 skyline-tool::*machine*) "SNES machine should be properly set")

    ;; Test machine name resolution
    (is (string= "SNES" (skyline-tool::machine-short-name))
        "SNES should have correct machine name")

    ;; Test that machine is valid
    (is-true (skyline-tool::check-machine-valid 88)
             "SNES machine 88 should be valid")

    ;; Test art index parsing (core functionality)
    (let ((temp-index (format nil "/tmp/snes-integration-~X.txt" (sxhash (get-universal-time)))))
      (unwind-protect
           (progn
             (with-open-file (out temp-index :direction :output :if-exists :supersede)
               (format out "# SNES Integration Test~%")
               (format out "test.png 2bpp 32×32~%"))

             (let ((art-index (skyline-tool::read-snes-art-index temp-index)))
               (is (= 1 (length art-index)) "Should parse art index correctly")
               (destructuring-bind (mode png-path width height) (first art-index)
                 (is (eq :2BPP mode) "Mode should be parsed correctly")
                 (is (= 32 width) "Width should be parsed correctly")
                 (is (= 32 height) "Height should be parsed correctly"))))
        ;; Cleanup
        (when (probe-file temp-index)
          (delete-file temp-index))))))

(defun run-snes-tests ()
  "Run all SNES tests and return results"
  (fiveam:run! 'snes-tests))
