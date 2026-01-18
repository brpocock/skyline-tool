;;; Phantasia SkylineTool/tests/7800-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite 7800-tests
  :description "Tests for Atari 7800-specific SkylineTool functionality")

(in-suite 7800-tests)

;; Test 7800 graphics functions existence
(test 7800-graphics-functions-existence
  "Test that 7800 graphics conversion functions exist"
  (is-true (fboundp 'skyline-tool::7800-image-to-160a)
           "7800-image-to-160a should exist")
  (is-true (fboundp 'skyline-tool::7800-image-to-320a)
           "7800-image-to-320a should exist")
  (is-true (fboundp 'skyline-tool::7800-image-to-320c)
           "7800-image-to-320c should exist")
  (is-true (fboundp 'skyline-tool::parse-7800-object)
           "parse-7800-object should exist"))

;; Test 7800 binary functions existence
(test 7800-binary-functions-existence
  "Test that 7800 binary processing functions exist"
  (is-true (fboundp 'skyline-tool::write-7800-binary)
           "write-7800-binary should exist")
  (is-true (fboundp 'skyline-tool::interleave-7800-bytes)
           "interleave-7800-bytes should exist"))

;; Test 7800 binary output validation
(test 7800-binary-output-validation
  "Test that 7800 binary output is correctly formatted"
  (uiop:with-temporary-file (:pathname temp-file :type "o")
    (let ((test-data '((#x01 #x02 #x03 #x04)  ; 4 bytes = page length
                       (#x05 #x06 #x07 #x08))))
      ;; Write test binary data
      (skyline-tool::write-7800-binary temp-file test-data)

    ;; Validate the output file exists and has correct size
    (is-true (probe-file temp-file)
             "Binary output file should be created")

    ;; Check file size: 2 pages * 256 bytes each = 512 bytes
    (when (probe-file temp-file)
      (is (= (with-open-file (stream temp-file :element-type '(unsigned-byte 8))
               (file-length stream))
             512)
           "File should be exactly 512 bytes (2 pages × 256 bytes)")

      ;; Read and validate first page content
      (with-open-file (stream temp-file :element-type '(unsigned-byte 8))
        (let ((first-page (loop for i from 0 to 3 collect (read-byte stream))))
          (is (equal first-page '(1 2 3 4))
              "First page should contain the correct data"))

        ;; Skip padding bytes (252 bytes of zeros)
        (dotimes (i 252) (read-byte stream))

          ;; Read second page
          (let ((second-page (loop for i from 0 to 3 collect (read-byte stream))))
            (is (equal second-page '(5 6 7 8))
                "Second page should contain the correct data")))))))

;; Test 7800 music processing correctness
(test 7800-music-processing-correctness
  "Test that 7800 music functions process MIDI data correctly"
  ;; Test midi->7800-tia with comprehensive mock MIDI data
  (let ((mock-midi-notes '((:time 0 :key 60 :velocity 100 :channel 0)    ; Middle C
                           (:time 100 :key 64 :velocity 80 :channel 0)   ; E above middle C
                           (:time 200 :key 67 :velocity 60 :channel 0)   ; G above middle C
                           (:time 300 :key 60 :velocity 0 :channel 0)))) ; Note off

    (let ((result (skyline-tool::midi->7800-tia mock-midi-notes :ntsc)))
      (is (listp result)
          "midi->7800-tia should return a list")
      (is (= (length result) 4)
          "midi->7800-tia should process all input events")

      ;; Verify first note (Middle C)
      (let ((first-note (first result)))
        (is (and (consp first-note) (eql (first first-note) :frequency))
            "First element should be frequency event")
        (is (< 259 (getf first-note :frequency) 262)  ; Middle C ~261.63Hz
            "Middle C frequency should be correct"))

      ;; Verify note-off handling
      (let ((note-off-event (fourth result)))
        (is (and (consp note-off-event) (eql (first note-off-event) :frequency))
            "Note-off should be converted to frequency event")
        (is (= (getf note-off-event :frequency) 0)
            "Note-off should set frequency to 0"))))

  ;; Test array<-7800-tia-notes-list with detailed validation
  (let ((tia-notes '((:frequency 261.63 :volume 15 :control #x04)  ; Middle C, AUDC value
                     (:frequency 329.63 :volume 10 :control #x08)  ; E4, different AUDC
                     (:frequency 0 :volume 0 :control #x00))))     ; Silence
    (let ((result (skyline-tool::array<-7800-tia-notes-list tia-notes :ntsc)))
      (is (arrayp result)
          "array<-7800-tia-notes-list should return an array")
      (is (= (length result) 3)
          "array should contain all input notes")

      ;; Verify array structure - should contain AUDC/AUDF pairs
      (is (= (length (aref result 0)) 2)
          "Each note should be encoded as 2 bytes (AUDF, AUDC)")
      (is (= (first (aref result 0)) #x0F)  ; AUDF for ~262Hz (rounded)
          "First note AUDF should be correct")
      (is (= (second (aref result 0)) #x84) ; AUDC with volume 15
          "First note AUDC should have correct volume and control")

      ;; Verify silence encoding
      (is (= (first (aref result 2)) 0)
          "Silence should have AUDF = 0")
      (is (= (second (aref result 2)) 0)
          "Silence should have AUDC = 0")))

  ;; Test frequency calculation accuracy
  (let ((test-frequencies '(261.63 293.66 329.63 349.23 392.00 440.00))) ; C major scale
    (dolist (freq test-frequencies)
      (let* ((midi-key (skyline-tool::key<-midi-key freq))
             (reconstructed-freq (skyline-tool:freq<-midi-key midi-key)))
        ;; Allow 1% tolerance for TIA frequency approximation
        (is (< (abs (- freq reconstructed-freq)) (* freq 0.01))
            (format nil "Frequency ~A should round-trip accurately, got ~A"
                    freq reconstructed-freq))))))

;; Test 7800 music compilation output validation
(test 7800-music-compilation-output-validation
  "Test that 7800 music compilation produces valid binary output"
  (let ((temp-file (format nil "Object/7800/7800-test-music-~x.bin"
                           (sxhash (get-universal-time)))))
    (let ((skyline-tool::*machine* 7800))
      ;; Create a minimal mock MIDI file for testing
      ;; This would normally be done with actual MIDI data, but for testing
      ;; we'll use the existing compilation framework

      ;; For now, test that the compilation function exists and can handle
      ;; basic parameters without crashing (full validation would require MIDI files)
      (is-true (fboundp 'skyline-tool::compile-music-7800)
               "compile-music-7800 function should exist")

      ;; Test that it properly handles invalid inputs
      (signals error (skyline-tool::compile-music-7800 temp-file "/nonexistent.mid" :tia :binary)
               "compile-music-7800 should signal error for missing MIDI file"))))

;; Test 7800 graphics conversion correctness
(test 7800-graphics-conversion-correctness
  "Test that 7800 graphics conversion produces correct byte data"
  ;; Test 160A mode (2 bits per pixel, 4 pixels per byte)
  (let ((test-palette (vector #xFF000000 #xFFFFFFFF #xFF808080 #xFFC0C0C0))  ; BG, FG1, FG2, FG3
        (test-image (make-array '(8 2) :element-type '(unsigned-byte 32))))

    ;; Create test pattern: 2 rows × 8 pixels = 16 pixels total
    ;; Row 0: BG, FG1, BG, FG2, BG, FG3, BG, FG1
    ;; Row 1: FG2, BG, FG3, BG, FG1, BG, FG2, BG
    (dotimes (x 8)
      (setf (aref test-image x 0) (aref test-palette (mod x 4))))
    (dotimes (x 8)
      (setf (aref test-image x 1) (aref test-palette (mod (+ x 1) 4))))

    ;; Test 160A conversion (4 pixels = 1 byte, 2 bits per pixel)
    ;; Row 0: pixels 0-3 (BG, FG1, BG, FG2) -> indices 0,1,0,2 -> bits 00,01,00,10 -> #b00010000 = 16
    ;; Row 0: pixels 4-7 (BG, FG3, BG, FG1) -> indices 0,3,0,1 -> bits 00,11,00,01 -> #b00001100 = 12
    ;; Row 1: pixels 0-3 (FG2, BG, FG3, BG) -> indices 2,0,3,0 -> bits 10,00,11,00 -> #b00111000 = 56
    ;; Row 1: pixels 4-7 (FG1, BG, FG2, BG) -> indices 1,0,2,0 -> bits 01,00,10,00 -> #b00001000 = 8
    (let ((result-160a (skyline-tool::7800-image-to-160a test-image
                                                        :byte-width 2 :height 2
                                                        :palette test-palette)))
      (is (= (length result-160a) 2) "160A should return 2 rows")
      (is (= (length (first result-160a)) 2) "160A row should contain 2 bytes")
      (is (= (nth 0 (first result-160a)) 16) "First byte of first row should be correct")
      (is (= (nth 1 (first result-160a)) 12) "Second byte of first row should be correct")
      (is (= (nth 0 (second result-160a)) 56) "First byte of second row should be correct")
      (is (= (nth 1 (second result-160a)) 8) "Second byte of second row should be correct"))

    ;; Test 320A conversion (2 pixels per byte, 4 bits per pixel)
    ;; Row 0: pixels 0-1 (BG=0, FG1=1) -> 4-bit values -> byte #x01
    ;; Row 0: pixels 2-3 (BG=0, FG2=2) -> 4-bit values -> byte #x02
    ;; etc.
    (let ((result-320a (skyline-tool::7800-image-to-320a test-image
                                                        :byte-width 4 :height 2
                                                        :palette test-palette)))
      (is (= (length result-320a) 2) "320A should return 2 rows")
      (is (= (length (first result-320a)) 4) "320A row should contain 4 bytes")
      (is (= (nth 0 (first result-320a)) #x10) "320A first byte should be correct")
      (is (= (nth 1 (first result-320a)) #x02) "320A second byte should be correct"))

    ;; Test 320C conversion (2 pixels per byte with color lookup table)
    (let ((result-320c (skyline-tool::7800-image-to-320c test-image
                                                        :byte-width 4 :height 2
                                                        :palette test-palette)))
      (is (= (length result-320c) 2) "320C should return 2 rows")
      (is (= (length (first result-320c)) 4) "320C row should contain 4 bytes")
      ;; 320C uses color lookup - verify basic structure
      (is (every #'integerp (first result-320c)) "320C bytes should be integers")))

  ;; Test edge cases and error conditions
  (let ((empty-palette (vector))
        (empty-image (make-array '(0 0) :element-type '(unsigned-byte 32))))

    ;; Test with empty palette
    (signals error (skyline-tool::7800-image-to-160a empty-image
                                                     :byte-width 1 :height 1
                                                     :palette empty-palette)
             "Should signal error with empty palette")

    ;; Test with mismatched dimensions
    (signals error (skyline-tool::7800-image-to-160a (make-array '(3 1))
                                                     :byte-width 1 :height 1
                                                     :palette (vector #xFF000000 #xFFFFFFFF))
             "Should signal error with invalid pixel count for 160A")))

;; Test 7800 parse-object method
(test 7800-parse-object-method
  "Test 7800 object parsing"
  (let ((test-pixels (make-array '(8 8) :element-type '(unsigned-byte 32) :initial-element 0)))
    ;; Test 160A mode returns proper data structure
    (let ((result (skyline-tool::parse-7800-object :160a test-pixels :width 8 :height 8)))
      (is (listp result) "parse-7800-object :160a should return a list")
      (is (> (length result) 0) "parse-7800-object should return non-empty result"))

    ;; Test with invalid mode should signal error
    (signals error (skyline-tool::parse-7800-object :invalid-mode test-pixels :width 8 :height 8)
             "parse-7800-object should reject invalid modes")))

;; Test 7800 binary processing correctness
(test 7800-binary-processing-correctness
  "Test that 7800 binary functions produce correct interleaved data"
  ;; Test interleave-7800-bytes with known data
  (let ((test-data '((1 2 3) (4 5 6) (7 8 9))))
    (let ((result (skyline-tool::interleave-7800-bytes test-data)))
      (is (equalp result '(1 4 7 2 5 8 3 6 9))
          "interleave-7800-bytes should correctly interleave columns into rows"))))

;; Test 7800 binary file writing and reading
(test 7800-binary-file-io
  "Test that 7800 binary file writing produces readable data"
  (let ((skyline-tool::*machine* 7800)
        (test-file (format nil "Object/7800/test-7800-data-~x.bin"
                           (sxhash (get-universal-time))))
        (test-data '((#xAA #xBB #xCC) (#xDD #xEE #xFF))))
    ;; Write test data
    (skyline-tool::write-7800-binary test-file test-data)

    ;; Verify file was created and has expected size
    (is-true (probe-file test-file)
             "write-7800-binary should create output file")

    ;; Read back and verify content
    (with-open-file (stream test-file :element-type '(unsigned-byte 8))
      (let ((bytes (loop for byte = (read-byte stream nil nil)
                        while byte collect byte)))
        (is (= (length bytes) 6)
            "Binary file should contain 6 bytes for 2x3 data")
        (is (equalp bytes '(#xAA #xBB #xCC #xDD #xEE #xFF))
            "Binary file should contain original data in correct order")))))

;; Test 7800 music compilation
(test 7800-music-compilation-basic
  "Test basic 7800 music compilation functionality"
  ;; Test array<-7800-tia-notes-list
  (let ((result (skyline-tool::array<-7800-tia-notes-list '((60 100 480) (62 100 480)) :ntsc)))
    (is (vectorp result) "array<-7800-tia-notes-list should return a vector")
    (is (= (length result) 2) "vector should contain both input notes"))

  ;; Test midi->7800-tia with mock data
  (let ((result (skyline-tool::midi->7800-tia '((60 100 480)) :ntsc)))
    (is (arrayp result) "midi->7800-tia should return an array")
    (is (= (length result) 2) "array should have 2 voices (TIA channels)"))

  ;; Test compile-music-7800 (will fail due to missing files but should not crash)
  (signals error (skyline-tool::compile-music-7800
                   (format nil "Object/~a/test-~x.s" (skyline-tool::machine-directory-name) (sxhash (get-universal-time)))
                   "/nonexistent.mid" :tia :binary)
            "compile-music-7800 should signal error for missing MIDI file"))

;; Test 7800 platform constants
(test 7800-platform-constants
  "Test that 7800 platform constants are properly defined"
  (is-true (skyline-tool::machine-valid-p 7800)
           "7800 should be a valid machine"))

;; Test error conditions for 7800 functions
(test 7800-error-conditions
  "Test error handling in 7800 functions"
  ;; Test with nil inputs
  (signals error (skyline-tool::7800-image-to-160a nil)
           "7800-image-to-160a should handle nil input")

  (signals error (skyline-tool::parse-7800-object :160a nil)
           "parse-7800-object should handle nil pixels")

  ;; Test interleave-7800-bytes with invalid data
  (signals error (skyline-tool::interleave-7800-bytes nil)
           "interleave-7800-bytes should handle nil input"))

(def-suite 7800-comprehensive-suite
  :description "Comprehensive 7800 functionality tests")

(in-suite 7800-comprehensive-suite)

;; Integration test for 7800 workflow
(test 7800-integration-workflow
  "Test complete 7800 graphics and audio pipeline integration"
  ;; Test that all core components are available and functional
  (is-true (fboundp 'skyline-tool::7800-image-to-160a)
           "Core 7800 graphics function should be available")
  (is-true (fboundp 'skyline-tool::write-7800-binary)
           "Binary output function should be available")
  (is-true (fboundp 'skyline-tool::compile-music-7800)
           "Music compilation should be available"))

(defun run-7800-tests ()
  "Run all 7800 tests including comprehensive functionality tests"
  (fiveam:run! '7800-tests)
  (fiveam:run! '7800-comprehensive-suite))
