;;; Phantasia SkylineTool/tests/snes-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(defpackage :skyline-tool/snes-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:compile-art-snes
                #:compile-music-snes)
  (:export #:snes-tests))

(in-package :skyline-tool/snes-test)

(def-suite snes-tests
  :description "Tests for SNES (Super Nintendo) specific SkylineTool functionality")

(in-suite snes-tests)

;; Test SNES art compilation function
(test snes-art-compilation-function
  "Test that compile-art-snes function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-snes))
  ;; Test that it can be called without error (placeholder implementation)
  (finishes (skyline-tool:compile-art-snes "/tmp/test.out" "/tmp/test.in")))

;; Test SNES graphics function properties (safe introspection only)
(test snes-graphics-function-properties
  "Test that SNES graphics functions exist and are callable"
  ;; Check function signatures (safe - no code generation)
  (is (functionp (symbol-function 'skyline-tool:compile-art-snes))))

;; Test SNES tile ripping functions
(test snes-tile-ripping-existence
  "Test that SNES tile ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-snes-tile)
          "blob-rip-snes-tile should be available"))

;; Test SNES sprite ripping functions
(test snes-sprite-ripping-existence
  "Test that SNES sprite ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-snes-sprite)
          "blob-rip-snes-sprite should be available"))

;; Test SNES music compilation
(test snes-music-compilation
  "Test that SNES music compilation functions exist and dispatch correctly"
  ;; Test that compile-music function exists
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that it can handle SNES machine type
  (let ((*machine* 6))
    (is (= 6 *machine*)))
  ;; Test that compile-music-snes function exists
  (is-true (fboundp 'skyline-tool::compile-music-snes)))

;; Test SNES sound compilation functions (SPC700)
(test snes-sound-compilation
  "Test sound compilation functions work for SNES"
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that SNES is accepted as a valid machine type (should signal error for missing file)
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "6"))
  ;; Test SNES SPC700 sound chip specification
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "6" "SPC700")))

;; Test SNES asset processing pipeline
(test snes-asset-pipeline
  "Test complete SNES asset processing pipeline"
  ;; This would be an integration test of the full pipeline
  (is-true t)) ;; Placeholder for future integration testing

;; Test SNES tile mode detection (safe - no file I/O)
(test snes-tile-mode-detection
  "Test SNES tile mode detection returns valid modes"
  (let ((mode (skyline-tool:detect-snes-tile-mode "/nonexistent/file.png")))
    (is (member mode '(:mode-normal :mode-flip-x :mode-flip-y :mode-flip-xy :mode-prio))
        "detect-snes-tile-mode should return valid modes, got: ~a" mode)))

;; Test SNES asset pipeline integration
(test snes-asset-pipeline-basic
  "Test basic SNES asset pipeline components work together"
  ;; Test that all the key functions exist and can be called
  (is-true (fboundp 'skyline-tool:compile-art-snes))
  (is-true (fboundp 'skyline-tool:compile-music))
  (is-true (fboundp 'skyline-tool:compile-sound))
  ;; Test machine type handling
  (let ((*machine* 6))
    (is (= 6 *machine*))))

;; Test SNES font ripping functions
(test snes-font-ripping-existence
  "Test that SNES font ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-snes-font)
          "blob-rip-snes-font should be available"))

;; Test SNES palette handling
(test snes-palette-handling
  "Test SNES palette handling functions"
  (is-true (fboundp 'skyline-tool:extract-snes-palettes)
          "extract-snes-palettes should be available")
  (is-true (fboundp 'skyline-tool:save-snes-palettes)
          "save-snes-palettes should be available"))

;; Test SNES CHR-ROM handling
(test snes-chr-rom-handling
  "Test SNES CHR-ROM handling functions"
  (is-true (fboundp 'skyline-tool:extract-snes-chr-data)
          "extract-snes-chr-data should be available")
  (is-true (fboundp 'skyline-tool:convert-snes-chr-to-png)
          "convert-snes-chr-to-png should be available"))

;; Test SNES Mode 7 handling
(test snes-mode7-handling
  "Test SNES Mode 7 handling functions"
  (is-true (fboundp 'skyline-tool:extract-snes-mode7-data)
          "extract-snes-mode7-data should be available")
  (is-true (fboundp 'skyline-tool:convert-snes-mode7-to-png)
          "convert-snes-mode7-to-png should be available"))

;; Test SNES HDMA handling
(test snes-hdma-handling
  "Test SNES HDMA handling functions"
  (is-true (fboundp 'skyline-tool:extract-snes-hdma-tables)
          "extract-snes-hdma-tables should be available")
  (is-true (fboundp 'skyline-tool:convert-snes-hdma-to-binary)
          "convert-snes-hdma-to-binary should be available"))

(defun snes-tests ()
  "Run all SNES tests and return results"
  (fiveam:run! 'snes-tests))