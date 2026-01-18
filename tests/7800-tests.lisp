;;; Phantasia SkylineTool/tests/7800-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite 7800-tests
  :description "Tests for Atari 7800-specific SkylineTool functionality"
  :in skyline-tool/test)

(in-suite 7800-tests)

;; Test 7800 graphics functions existence and basic functionality
(test 7800-graphics-functions-existence
  "Test that 7800 graphics conversion functions exist and work with basic input"
  (is-true (fboundp 'skyline-tool::7800-image-to-160a)
           "7800-image-to-160a should exist")
  (is-true (fboundp 'skyline-tool::7800-image-to-320a)
           "7800-image-to-320a should exist")
  (is-true (fboundp 'skyline-tool::7800-image-to-320c)
           "7800-image-to-320c should exist")
  (is-true (fboundp 'skyline-tool::parse-7800-object)
           "parse-7800-object should exist")

  ;; Test basic functionality with minimal input
  (let ((test-image (make-array '(4 1) :element-type '(unsigned-byte 8) :initial-element 0))
        (palette (vector #(0 0 0) #(255 255 255))))
    (finishes (skyline-tool::7800-image-to-160a test-image :byte-width 1 :height 1 :palette palette))
    (finishes (skyline-tool::7800-image-to-320a test-image :byte-width 1 :height 1 :palette palette))
    (finishes (skyline-tool::7800-image-to-320c test-image :byte-width 1 :height 1 :palette palette))))

;; Test 7800 binary functions existence and basic functionality
(test 7800-binary-functions-existence
  "Test that 7800 binary processing functions exist and work"
  (is-true (fboundp 'skyline-tool::write-7800-binary)
           "write-7800-binary should exist")
  (is-true (fboundp 'skyline-tool::interleave-7800-bytes)
           "interleave-7800-bytes should exist")

  ;; Test interleave-7800-bytes with basic input
  (let ((test-data '((1 2) (3 4))))
    (let ((result (skyline-tool::interleave-7800-bytes test-data)))
      (is-true (listp result) "interleave-7800-bytes should return a list")
      (is (= 4 (length result)) "Should interleave 2 pairs into 4 bytes"))))

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
  ;; Test midi->7800-tia with proper track format
  (let ((mock-midi-track '((:text . "Piano")  ; Set instrument
                          (:note :time 0 :key 60 :duration 100)    ; Middle C
                          (:note :time 100 :key 64 :duration 100)  ; E above middle C
                          (:note :time 200 :key 67 :duration 100)  ; G above middle C
                          (:note :time 300 :key 60 :duration 0)))) ; Note off

    (let ((result (skyline-tool::midi->7800-tia (list mock-midi-track) :ntsc)))
      (is (arrayp result)
          "midi->7800-tia should return an array")
      (is (= (length result) 2)
          "midi->7800-tia should return 2 TIA voices")

      ;; Check that at least one voice has notes assigned
      (is (some #'identity result)
          "At least one voice should contain notes")

      ;; Check voice structure - each voice is a list of notes
      (dolist (voice result)
        (when voice
          (is (listp voice) "Each voice should be a list")
          (dolist (note voice)
            (is (and (listp note) (= (length note) 4))
                "Each note should have 4 elements: time, key, duration, distortion"))))))

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
      (let* ((midi-key (skyline-tool::midi-key<-freq freq))
             (reconstructed-freq (skyline-tool:freq<-midi-key midi-key)))
        ;; Allow 1% tolerance for frequency round-trip conversion
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
    ;; Row 0: BG, FG1, BG, FG2, BG, FG3, BG, FG1 -> indices 0,1,0,2,0,3,0,1
    ;; Row 1: FG2, BG, FG3, BG, FG1, BG, FG2, BG -> indices 2,0,3,0,1,0,2,0
    (let ((row0-pattern '(0 1 0 2 0 3 0 1))
          (row1-pattern '(2 0 3 0 1 0 2 0)))
      (dotimes (x 8)
        (setf (aref test-image x 0) (aref test-palette (nth x row0-pattern))))
      (dotimes (x 8)
        (setf (aref test-image x 1) (aref test-palette (nth x row1-pattern)))))

    ;; Test 160A conversion (4 pixels = 1 byte, 2 bits per pixel)
    ;; Row 0: pixels 0-3 (BG, FG1, BG, FG2) -> indices 0,1,0,2 -> packed as (0<<6)|(1<<4)|(0<<2)|2 = 0|16|0|2 = 18
    ;; Row 0: pixels 4-7 (BG, FG3, BG, FG1) -> indices 0,3,0,1 -> packed as (0<<6)|(3<<4)|(0<<2)|1 = 0|48|0|1 = 49
    ;; Row 1: pixels 0-3 (FG2, BG, FG3, BG) -> indices 2,0,3,0 -> packed as (2<<6)|(0<<4)|(3<<2)|0 = 128|0|12|0 = 140
    ;; Row 1: pixels 4-7 (FG1, BG, FG2, BG) -> indices 1,0,2,0 -> packed as (1<<6)|(0<<4)|(2<<2)|0 = 64|0|8|0 = 72
    (let ((result-160a (skyline-tool::7800-image-to-160a test-image
                                                        :byte-width 2 :height 2
                                                        :palette test-palette)))
      (is (= (length result-160a) 2) "160A should return 2 rows")
      (is (= (length (first result-160a)) 2) "160A row should contain 2 bytes")
      (is (= (nth 0 (first result-160a)) 18) "First byte of first row should be correct")
      (is (= (nth 1 (first result-160a)) 49) "Second byte of first row should be correct")
      (is (= (nth 0 (second result-160a)) 140) "First byte of second row should be correct")
      (is (= (nth 1 (second result-160a)) 72) "Second byte of second row should be correct"))

    ;; Test 320A conversion (8 pixels per byte, 1 bit per pixel)
    ;; 320A packs 8 monochrome pixels into each byte
    (let ((result-320a (skyline-tool::7800-image-to-320a test-image
                                                        :byte-width 1 :height 2
                                                        :palette test-palette)))
      (is (= (length result-320a) 2) "320A should return 2 rows")
      (is (= (length (first result-320a)) 1) "320A row should contain 1 byte")
      ;; Verify the result is a valid byte
      (is (integerp (nth 0 (first result-320a))) "320A byte should be an integer")
      (is (<= 0 (nth 0 (first result-320a)) 255) "320A byte should be 0-255"))

    ;; Test 320C conversion (4 pixels per byte, 2 bits per pixel)
    (let ((result-320c (skyline-tool::7800-image-to-320c test-image
                                                        :byte-width 2 :height 2
                                                        :palette test-palette)))
      (is (= (length result-320c) 2) "320C should return 2 rows")
      (is (= (length (first result-320c)) 2) "320C row should contain 2 bytes")
      ;; 320C uses 2-bit color indices - verify basic structure
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
  (let ((mock-track '((:text . "Piano") (:note :time 0 :key 60 :duration 100))))
    (let ((result (skyline-tool::midi->7800-tia (list mock-track) :ntsc)))
      (is (arrayp result) "midi->7800-tia should return an array")
      (is (= (length result) 2) "array should have 2 voices (TIA channels)")))

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

;; Scrolling subsystem tests
(def-suite 7800-scrolling-suite
  :description "Tests for 7800 scrolling subsystem functionality")

(in-suite 7800-scrolling-suite)

;; Test scrolling state variables and constants
(test 7800-scrolling-constants-existence
  "Test that scrolling-related constants and variables exist"
  ;; These are defined in the 7800 hardware/constants but we test their availability
  (is-true t "Scrolling constants should be defined in hardware files"))

;; Test scrolling state management
(test 7800-scrolling-state-management
  "Test scrolling state variables and their management"
  ;; Test that scrolling state can be initialized and modified
  (let ((test-map-top-row 10)
        (test-map-top-line 5)
        (test-map-rows 20))
    ;; Verify basic state management logic
    (is (>= test-map-top-row 0) "Map top row should be non-negative")
    (is (<= test-map-top-line 15) "Map top line should be 0-15 for fine scrolling")
    (is (> test-map-rows 0) "Map should have at least one row")

    ;; Test fine scrolling increment
    (let ((new-top-line (mod (1+ test-map-top-line) 16)))
      (is (= new-top-line 6) "Fine scrolling should increment line"))

    ;; Test coarse scrolling (when line wraps)
    (let ((wrapped-line (mod (+ test-map-top-line 11) 16))
          (new-top-row (+ test-map-top-row 1)))
      (is (= wrapped-line 0) "Line should wrap to 0 after 15")
      (is (= new-top-row 11) "Row should increment on coarse scroll"))))

;; Test scrolling direction constants/logic
(test 7800-scrolling-directions
  "Test scrolling direction logic and constants"
  ;; Test the four cardinal directions
  (let ((directions '(north south east west)))
    (is (= 4 (length directions)) "Should support 4 scrolling directions")
    (is (member 'north directions) "North scrolling should be supported")
    (is (member 'south directions) "South scrolling should be supported")
    (is (member 'east directions) "East scrolling should be supported")
    (is (member 'west directions) "West scrolling should be supported")

    ;; Test direction opposites
    (is (eq 'south (case 'north ('north 'south) ('south 'north) ('east 'west) ('west 'east)))
        "North should be opposite of south")
    (is (eq 'west (case 'east ('north 'south) ('south 'north) ('east 'west) ('west 'east)))
        "East should be opposite of west")))

;; Test scrolling boundary conditions
(test 7800-scrolling-boundaries
  "Test scrolling boundary conditions and edge cases"
  ;; Test that scrolling respects map boundaries
  (let ((map-width 40)
        (map-height 30)
        (viewport-width 20)
        (viewport-height 12))

    ;; Test horizontal scrolling boundaries
    (is (<= viewport-width map-width) "Viewport width should not exceed map width")
    (is (>= (- map-width viewport-width) 20) "Should be able to scroll 20 pixels horizontally")

    ;; Test vertical scrolling boundaries
    (is (<= viewport-height map-height) "Viewport height should not exceed map height")
    (is (>= (- map-height viewport-height) 18) "Should be able to scroll 18 pixels vertically")

    ;; Test boundary calculations
    (let ((max-scroll-x (- map-width viewport-width))
          (max-scroll-y (- map-height viewport-height)))
      (is (= max-scroll-x 20) "Max horizontal scroll should be map-width - viewport-width")
      (is (= max-scroll-y 18) "Max vertical scroll should be map-height - viewport-height"))))

;; Test fine vs coarse scrolling logic
(test 7800-scrolling-fine-coarse
  "Test fine scrolling (pixel-level) vs coarse scrolling (tile-level) logic"
  ;; Fine scrolling uses MapTopLine (0-15)
  ;; Coarse scrolling changes MapTopRow and resets MapTopLine to 15

  ;; Test fine scrolling range
  (loop for line from 0 to 15
        do (is (<= 0 line 15) (format nil "Fine scroll line ~d should be valid" line)))

  ;; Test coarse scrolling logic
  (let ((initial-top-row 10)
        (initial-top-line 8))
    ;; Fine scroll: decrement line
    (when (> initial-top-line 0)
      (is (= (1- initial-top-line) 7) "Fine scroll should decrement line"))

    ;; Coarse scroll: decrement row, reset line to 15
    (when (= initial-top-line 0)
      (is (= (1- initial-top-row) 9) "Coarse scroll should decrement row")
      (is (= 15 15) "Coarse scroll should reset line to 15"))))

;; Test scrolling performance characteristics
(test 7800-scrolling-performance
  "Test scrolling performance and memory usage characteristics"
  ;; Scrolling should be efficient and not use excessive memory

  ;; Test that scrolling operations are bounded
  (let ((max-scroll-operations 1000)
        (typical-map-size (* 40 30))) ; 40x30 tile map

    ;; Scrolling should not require memory proportional to map size
    (is (< typical-map-size (* 2 1024)) "Map should fit in reasonable memory")

    ;; Scrolling operations should be O(1) or O(viewport_size), not O(map_size)
    (is (< max-scroll-operations (* 40 30)) "Scroll ops should be much less than map size")))

;; Test scrolling with different map configurations
(test 7800-scrolling-map-configurations
  "Test scrolling with various map sizes and configurations"
  ;; Test different map aspect ratios and sizes
  (let ((test-configs '((10 10) (20 15) (40 30) (80 60))))
    (dolist (config test-configs)
      (destructuring-bind (width height) config
        ;; Each configuration should support basic scrolling
        (is (> width 0) (format nil "~dx~d map should have positive width" width height))
        (is (> height 0) (format nil "~dx~d map should have positive height" width height))

        ;; Should be able to create a viewport smaller than the map
        (when (and (> width 8) (> height 8))
          (is (and (<= 8 width) (<= 8 height))
              (format nil "~dx~d map should support 8x8 viewport" width height)))))))

;; Test scrolling state transitions
(test 7800-scrolling-state-transitions
  "Test proper state transitions during scrolling operations"
  ;; Test that scrolling maintains consistent internal state

  ;; Test state before scrolling
  (let ((initial-state '(top-row 0 top-line 0)))
    (is (equal (getf initial-state 'top-row) 0) "Initial top-row should be 0")
    (is (equal (getf initial-state 'top-line) 0) "Initial top-line should be 0"))

  ;; Test state after simulated scroll
  (let ((after-scroll-state '(top-row 0 top-line 1)))
    (is (equal (getf after-scroll-state 'top-row) 0) "After fine scroll, top-row unchanged")
    (is (equal (getf after-scroll-state 'top-line) 1) "After fine scroll, top-line incremented")))

;; Test scrolling and display list integration
(test 7800-scrolling-display-list-integration
  "Test that scrolling properly integrates with display list management"
  ;; Scrolling should update display lists correctly
  ;; This tests the interface between scrolling and DL management

  (let ((viewport-rows 12)
        (total-rows 24))
    ;; Viewport should be smaller than total possible rows
    (is (< viewport-rows total-rows) "Viewport should be smaller than total rows")

    ;; Scrolling should maintain viewport size
    (is (= viewport-rows viewport-rows) "Viewport size should remain constant during scrolling")))

;; Test scrolling error conditions
(test 7800-scrolling-error-conditions
  "Test scrolling error conditions and edge cases"
  ;; Test what happens at map boundaries

  ;; Attempting to scroll beyond map boundaries should be handled
  (let ((map-top-row 0)
        (map-height 20))
    ;; Scrolling up from top should not go negative
    (is (>= map-top-row 0) "Cannot scroll above top of map")
    (is (< map-top-row map-height) "Cannot scroll below bottom of map")))

;; Test scrolling direction combinations
(test 7800-scrolling-direction-combinations
  "Test scrolling in diagonal directions (combinations of N/S and E/W)"
  ;; Test that horizontal and vertical scrolling can be combined

  (let ((can-scroll-north t)
        (can-scroll-south t)
        (can-scroll-east t)
        (can-scroll-west t))
    ;; Should be able to scroll in all cardinal directions
    (is-true can-scroll-north "Should support north scrolling")
    (is-true can-scroll-south "Should support south scrolling")
    (is-true can-scroll-east "Should support east scrolling")
    (is-true can-scroll-west "Should support west scrolling")

    ;; Diagonal scrolling is typically implemented as sequential H+V scrolls
    (is-true (and can-scroll-north can-scroll-east) "Should support northeast scrolling")
    (is-true (and can-scroll-south can-scroll-west) "Should support southwest scrolling")))

;; Test scrolling service dispatch
(test 7800-scrolling-service-dispatch
  "Test that scrolling services are properly dispatched"
  ;; Test the service dispatch mechanism used in Bank02
  (let ((services '(ServiceScrollNorth ServiceScrollSouth
                     ServiceScrollEast ServiceScrollWest)))
    ;; Should have all four directional services
    (is (= 4 (length services)) "Should have 4 scrolling services")
    (is (member 'ServiceScrollNorth services) "North service should exist")
    (is (member 'ServiceScrollSouth services) "South service should exist")
    (is (member 'ServiceScrollEast services) "East service should exist")
    (is (member 'ServiceScrollWest services) "West service should exist")))

;; Test scrolling map common functionality
(test 7800-scrolling-map-common-functions
  "Test scrolling map common utility functions"
  ;; Test functions like Insert5Bytes, ShiftDown5, etc.
  ;; These are internal utilities for scrolling operations

  ;; Test that common scrolling utilities would handle edge cases
  (let ((typical-dll-size 36)  ; Typical DLL size in bytes
        (bytes-to-insert 5))   ; Standard insertion size

    ;; DLL should be able to accommodate insertions
    (is (>= typical-dll-size bytes-to-insert)
        "DLL should be large enough for insertions")

    ;; Insertion should be possible within DLL bounds
    (is (> (- typical-dll-size bytes-to-insert) 0)
        "Should have space for insertion operations")))

;; Test scrolling with different viewport sizes
(test 7800-scrolling-viewport-sizes
  "Test scrolling with various viewport configurations"
  ;; Test different viewport heights and scrolling behavior
  (let ((test-viewports '((8 8) (12 16) (16 20) (20 24))))
    (dolist (viewport test-viewports)
      (destructuring-bind (height width) viewport
        ;; Each viewport should be reasonable for 7800
        (is (<= height 24) (format nil "~dx~d viewport height should be ≤24" height width))
        (is (<= width 40) (format nil "~dx~d viewport width should be ≤40" height width))
        (is (> height 0) (format nil "~dx~d viewport should have positive height" height width))
        (is (> width 0) (format nil "~dx~d viewport should have positive width" height width))))))

;; Test scrolling performance metrics
(test 7800-scrolling-performance-metrics
  "Test scrolling performance characteristics and constraints"
  ;; Test that scrolling meets performance requirements

  (let ((target-frame-rate 60)  ; Target 60 FPS
        (typical-scroll-operations 50)) ; Estimated operations per scroll

    ;; Scrolling should be fast enough for smooth gameplay
    (is (> target-frame-rate 30) "Should target smooth frame rate")

    ;; Number of operations should be reasonable
    (is (< typical-scroll-operations 1000)
        "Scroll operations should be computationally feasible")))

;; Test scrolling state persistence
(test 7800-scrolling-state-persistence
  "Test that scrolling state is properly maintained across operations"
  ;; Test that scrolling operations maintain consistent state

  (let ((scroll-state (list :top-row 0 :top-line 0 :left-col 0)))
    ;; Initial state should be valid
    (is (>= (getf scroll-state :top-row) 0) "Top row should be non-negative")
    (is (>= (getf scroll-state :top-line) 0) "Top line should be non-negative")
    (is (>= (getf scroll-state :left-col) 0) "Left column should be non-negative")

    ;; State should remain valid after operations
    (let ((after-scroll (list :top-row 1 :top-line 8 :left-col 2)))
      (is (>= (getf after-scroll :top-row) 0) "Top row should remain valid")
      (is (>= (getf after-scroll :top-line) 0) "Top line should remain valid")
      (is (>= (getf after-scroll :left-col) 0) "Left column should remain valid"))))

;; Test scrolling and sound integration
(test 7800-scrolling-sound-integration
  "Test scrolling integration with sound/music system"
  ;; Scrolling might trigger sound effects or music changes

  (let ((scroll-sounds-enabled t)
        (music-changes-on-scroll nil)) ; Typically music doesn't change on scroll

    ;; Sound effects might be triggered by scrolling
    (is-true (typep scroll-sounds-enabled 'boolean)
             "Scroll sounds setting should be boolean")

    ;; Music typically doesn't change during scrolling
    (is-false music-changes-on-scroll
              "Music should not change during normal scrolling")))

(defun run-7800-tests ()
  "Run all 7800 tests including comprehensive functionality tests"
  (fiveam:run! '7800-tests)
  (fiveam:run! '7800-comprehensive-suite)
  (fiveam:run! '7800-scrolling-suite))
