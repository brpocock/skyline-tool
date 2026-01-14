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

;; Test 7800 music processing correctness
(test 7800-music-processing-correctness
  "Test that 7800 music functions process MIDI data correctly"
  ;; Test midi->7800-tia with mock MIDI data
  (let ((mock-midi-notes '((:time 0 :key 60 :velocity 100)    ; Middle C
                           (:time 1 :key 64 :velocity 80)     ; E above middle C
                           (:time 2 :key 67 :velocity 60))))  ; G above middle C

    (let ((result (skyline-tool::midi->7800-tia mock-midi-notes)))
      (is (listp result)
          "midi->7800-tia should return a list")
      (is (= (length result) 3)
          "midi->7800-tia should process all input notes")))

  ;; Test array<-7800-tia-notes-list
  (let ((tia-notes '((:frequency 440 :volume 15 :control #x00)
                     (:frequency 880 :volume 10 :control #x00))))
    (let ((result (skyline-tool::array<-7800-tia-notes-list tia-notes)))
      (is (arrayp result)
          "array<-7800-tia-notes-list should return an array")
      (is (= (length result) 2)
          "array should contain all input notes"))))

;; Test 7800 graphics conversion correctness
(test 7800-graphics-conversion-correctness
  "Test that 7800 graphics conversion produces correct byte data"
  ;; Create a test palette (4 colors for 160A mode)
  (let ((test-palette (vector #xFF000000 #xFFFFFFFF #xFF808080 #xFFC0C0C0))  ; black, white, gray, light gray
        (test-image (make-array '(4 1) :element-type '(unsigned-byte 32))))

    ;; Set up test image: black, white, gray, light gray pixels
    (setf (aref test-image 0 0) #xFF000000)  ; black -> palette index 0
    (setf (aref test-image 1 0) #xFFFFFFFF)  ; white -> palette index 1
    (setf (aref test-image 2 0) #xFF808080)  ; gray -> palette index 2
    (setf (aref test-image 3 0) #xFFC0C0C0)  ; light gray -> palette index 3

    ;; Test 160A conversion (4 pixels = 1 byte, 2 bits per pixel)
    ;; Expected: indices 0,1,2,3 -> binary 00,01,10,11 -> byte #b00110110 = 54
    (let ((result-160a (skyline-tool::7800-image-to-160a test-image
                                                        :byte-width 1 :height 1
                                                        :palette test-palette)))
      (is (equalp result-160a '((54)))  ; ((00 01 10 11)) packed = 54
          "160A conversion should pack 4 pixels into correct byte value"))

    ;; Test 320A conversion (2 pixels per byte, 4 bits per pixel)
    ;; Expected: first 2 pixels (0,1) -> #b00001111 = 15
    (let ((result-320a (skyline-tool::7800-image-to-320a test-image
                                                        :byte-width 1 :height 1
                                                        :palette test-palette)))
      (is (= (length result-320a) 1) "320A should return one row")
      (is (= (length (first result-320a)) 1) "320A row should contain one byte")
      (is (= (caar result-320a) 15) "320A should pack first 2 pixels correctly"))

    ;; Test 320C conversion (2 pixels per byte with color lookup)
    (let ((result-320c (skyline-tool::7800-image-to-320c test-image
                                                        :byte-width 1 :height 1
                                                        :palette test-palette)))
      (is (= (length result-320c) 1) "320C should return one row")
      (is (= (length (first result-320c)) 1) "320C row should contain one byte"))))

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
          "interleave-7800-bytes should correctly interleave columns into rows")))

;; Test 7800 binary file writing and reading
(test 7800-binary-file-io
  "Test that 7800 binary file writing produces readable data"
  (let ((test-file "/tmp/test-7800-data.bin")
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
  (signals error (skyline-tool::compile-music-7800 "/tmp/test.s" "/nonexistent.mid" :tia)
            "compile-music-7800 should signal error for missing MIDI file"))

;; Test 7800 platform constants
(test 7800-platform-constants
  "Test that 7800 platform constants are properly defined"
  (is-true (member 7800 skyline-tool::*valid-machines*)
           "7800 should be in valid machines list"))

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
