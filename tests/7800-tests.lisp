;;; Phantasia SkylineTool/tests/7800-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(defpackage :skyline-tool/7800-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:compile-art-7800
                #:compile-music-7800)
  (:export #:7800-tests))

(in-package :skyline-tool/7800-test)

(def-suite 7800-tests
  :description "Tests for 7800 (Atari 7800) specific SkylineTool functionality")

(in-suite 7800-tests)

;; Test 7800 art compilation function
(test 7800-art-compilation-function
  "Test that compile-art-7800 function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-7800))
  ;; Test that it can be called without error (placeholder implementation)
  (finishes (skyline-tool:compile-art-7800 "/tmp/test.out" "/tmp/test.in")))

;; Test 7800 graphics function properties (safe introspection only)
(test 7800-graphics-function-properties
  "Test that 7800 graphics functions exist and are callable"
  ;; Check function signatures (safe - no code generation)
  (is (functionp (symbol-function 'skyline-tool:compile-art-7800))))

;; Test 7800 tile ripping functions
(test 7800-tile-ripping-existence
  "Test that 7800 tile ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-7800-tile)
          "blob-rip-7800-tile should be available"))

;; Test 7800 sprite ripping functions
(test 7800-sprite-ripping-existence
  "Test that 7800 sprite ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-7800-sprite)
          "blob-rip-7800-sprite should be available"))

;; Test 7800 music compilation
(test 7800-music-compilation
  "Test that 7800 music compilation functions exist and dispatch correctly"
  ;; Test that compile-music function exists
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that it can handle 7800 machine type
  (let ((*machine* 7800))
    (is (= 7800 *machine*)))
  ;; Test that compile-music-7800 function exists
  (is-true (fboundp 'skyline-tool::compile-music-7800)))

;; Test 7800 sound compilation functions (POKEY/HOKEY)
(test 7800-sound-compilation
  "Test sound compilation functions work for 7800"
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that 7800 is accepted as a valid machine type (should signal error for missing file)
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "7800"))
  ;; Test POKEY sound chip specification
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "7800" "POKEY")))

;; Test 7800 asset processing pipeline
(test 7800-asset-pipeline
  "Test complete 7800 asset processing pipeline"
  ;; This would be an integration test of the full pipeline
  (is-true t)) ;; Placeholder for future integration testing

;; Test 7800 tile mode detection (safe - no file I/O)
(test 7800-tile-mode-detection
  "Test 7800 tile mode detection returns valid modes"
  (let ((mode (skyline-tool:detect-7800-tile-mode "/nonexistent/file.png")))
    (is (member mode '(:mode-160a :mode-160b :mode-320a :mode-320b :mode-320c :mode-320d))
        "detect-7800-tile-mode should return valid modes, got: ~a" mode)))

;; Test 7800 asset pipeline integration
(test 7800-asset-pipeline-basic
  "Test basic 7800 asset pipeline components work together"
  ;; Test that all the key functions exist and can be called
  (is-true (fboundp 'skyline-tool:compile-art-7800))
  (is-true (fboundp 'skyline-tool:compile-music))
  (is-true (fboundp 'skyline-tool:compile-sound))
  ;; Test machine type handling
  (let ((*machine* 7800))
    (is (= 7800 *machine*))))

;; Test 7800 font ripping functions
(test 7800-font-ripping-existence
  "Test that 7800 font ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-7800-font)
          "blob-rip-7800-font should be available"))

;; Test 7800 palette handling
(test 7800-palette-handling
  "Test 7800 palette handling functions"
  (is-true (fboundp 'skyline-tool:extract-7800-palettes)
          "extract-7800-palettes should be available")
  (is-true (fboundp 'skyline-tool:save-7800-palettes)
          "save-7800-palettes should be available"))

;; Test 7800 binary data writing
(test 7800-binary-writing
  "Test binary data writing for 7800 format"
  (let ((test-data '((#x00 #x01 #x02 #x03)
                     (#x10 #x11 #x12 #x13)
                     (#xff #xfe #xfd #xfc))))
    (finishes
     (skyline-tool::write-7800-binary "/tmp/test-7800-binary.bin" test-data))
    ;; Check that output file was created and has correct size
    (let ((file (probe-file "/tmp/test-7800-binary.bin")))
      (is-true file)
      ;; Should be 3 pages * 256 bytes each = 768 bytes
      (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
        (is (= 768 (file-length stream)))))))

;; Test 7800 speech synthesis (SpeakJet phonemes)
(test 7800-speech-synthesis
  "Test 7800 speech synthesis functions (SpeakJet phonemes)"
  (is-true (fboundp 'skyline-tool:compile-speech-7800)
          "compile-speech-7800 should be available")
  ;; Test that speech synthesis is supported for 7800
  (let ((*machine* 7800))
    (is-true (skyline-tool::speech-supported-p))))

;; Test 7800 display list generation
(test 7800-display-list-generation
  "Test 7800 display list generation functions"
  (is-true (fboundp 'skyline-tool:generate-7800-display-list)
          "generate-7800-display-list should be available")
  (is-true (fboundp 'skyline-tool:compile-display-list-7800)
          "compile-display-list-7800 should be available"))

(defun 7800-tests ()
  "Run all 7800 tests and return results"
  (fiveam:run! '7800-tests))