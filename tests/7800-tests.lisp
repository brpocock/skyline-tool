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

;; Test 7800 music functions existence
(test 7800-music-functions-existence
  "Test that 7800 music compilation functions exist"
  (is-true (fboundp 'skyline-tool::compile-music-7800)
           "compile-music-7800 should exist")
  (is-true (fboundp 'skyline-tool::midi->7800-tia)
           "midi->7800-tia should exist")
  (is-true (fboundp 'skyline-tool::array<-7800-tia-notes-list)
           "array<-7800-tia-notes-list should exist"))

;; Test 7800 graphics conversion with mock data
(test 7800-graphics-conversion-basic
  "Test basic 7800 graphics conversion functionality"
  (let ((test-image (make-array '(8 8) :element-type '(unsigned-byte 32) :initial-element #xFF000000)))
    ;; Test 160A conversion
    (finishes (skyline-tool::7800-image-to-160a test-image :byte-width 8 :height 8)
              "7800-image-to-160a should handle basic conversion")

    ;; Test 320A conversion
    (finishes (skyline-tool::7800-image-to-320a test-image :byte-width 8 :height 8)
              "7800-image-to-320a should handle basic conversion")

    ;; Test 320C conversion
    (finishes (skyline-tool::7800-image-to-320c test-image :byte-width 8 :height 8)
              "7800-image-to-320c should handle basic conversion")))

;; Test 7800 parse-object method
(test 7800-parse-object-method
  "Test 7800 object parsing"
  (let ((test-pixels (make-array '(8 8) :element-type '(unsigned-byte 32) :initial-element 0)))
    (finishes (skyline-tool::parse-7800-object :160a test-pixels :width 8 :height 8)
              "parse-7800-object should handle 160A mode")

    ;; Test with invalid mode should signal error
    (signals error (skyline-tool::parse-7800-object :invalid-mode test-pixels :width 8 :height 8)
             "parse-7800-object should reject invalid modes")))

;; Test 7800 binary writing functions
(test 7800-binary-writing
  "Test 7800 binary data writing functions"
  ;; Test interleave-7800-bytes
  (let ((test-data '((1 2 3) (4 5 6) (7 8 9))))
    (finishes (skyline-tool::interleave-7800-bytes test-data)
              "interleave-7800-bytes should process data"))

  ;; Test write-7800-binary with mock data
  (finishes (skyline-tool::write-7800-binary "/tmp/test-7800.bin" '((1 2 3) (4 5 6)))
            "write-7800-binary should handle basic data"))

;; Test 7800 music compilation
(test 7800-music-compilation-basic
  "Test basic 7800 music compilation functionality"
  ;; Test array<-7800-tia-notes-list
  (finishes (skyline-tool::array<-7800-tia-notes-list '((60 100 480) (62 100 480)) :ntsc)
            "array<-7800-tia-notes-list should process note data")

  ;; Test midi->7800-tia with mock data
  (finishes (skyline-tool::midi->7800-tia '((60 100 480)) :ntsc)
            "midi->7800-tia should process MIDI-like data")

  ;; Test compile-music-7800 (will fail due to missing files but should not crash)
  (signals error (skyline-tool::compile-music-7800 "/tmp/test.s" "/nonexistent.mid" :tia)
            "compile-music-7800 should signal error for missing MIDI file"))

;; Test 7800 platform constants
(test 7800-platform-constants
  "Test that 7800 platform constants are properly defined"
  (is (= skyline-tool::*machine* 7800)
      "*machine* should default to 7800 for this platform")
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

(defun run-7800-tests ()
  "Run all 7800 tests and return results"
  (fiveam:run! '7800-tests))
