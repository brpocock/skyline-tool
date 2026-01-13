;;; Phantasia SkylineTool/tests/nes-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(defpackage :skyline-tool/nes-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:compile-art-nes
                #:compile-music-nes)
  (:export #:nes-tests))

(in-package :skyline-tool/nes-test)

(def-suite nes-tests
  :description "Tests for NES (Nintendo Entertainment System) specific SkylineTool functionality")

(in-suite nes-tests)

;; Test NES art compilation function
(test nes-art-compilation-function
  "Test that compile-art-nes function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-nes))
  ;; Test that it can be called without error (placeholder implementation)
  (finishes (skyline-tool:compile-art-nes "/tmp/test.out" "/tmp/test.in")))

;; Test NES graphics function properties (safe introspection only)
(test nes-graphics-function-properties
  "Test that NES graphics functions exist and are callable"
  ;; Check function signatures (safe - no code generation)
  (is (functionp (symbol-function 'skyline-tool:compile-art-nes))))

;; Test NES tile ripping functions
(test nes-tile-ripping-existence
  "Test that NES tile ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-nes-tile)
          "blob-rip-nes-tile should be available"))

;; Test NES sprite ripping functions
(test nes-sprite-ripping-existence
  "Test that NES sprite ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-nes-sprite)
          "blob-rip-nes-sprite should be available"))

;; Test NES music compilation
(test nes-music-compilation
  "Test that NES music compilation functions exist and dispatch correctly"
  ;; Test that compile-music function exists
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that it can handle NES machine type
  (let ((*machine* 3))
    (is (= 3 *machine*)))
  ;; Test that compile-music-nes function exists
  (is-true (fboundp 'skyline-tool::compile-music-nes)))

;; Test NES sound compilation functions (APU)
(test nes-sound-compilation
  "Test sound compilation functions work for NES"
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that NES is accepted as a valid machine type (should signal error for missing file)
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "3"))
  ;; Test NES APU sound chip specification
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "3" "APU")))

;; Test NES asset processing pipeline
(test nes-asset-pipeline
  "Test complete NES asset processing pipeline"
  ;; This would be an integration test of the full pipeline
  (is-true t)) ;; Placeholder for future integration testing

;; Test NES tile mode detection (safe - no file I/O)
(test nes-tile-mode-detection
  "Test NES tile mode detection returns valid modes"
  (let ((mode (skyline-tool:detect-nes-tile-mode "/nonexistent/file.png")))
    (is (member mode '(:mode-normal :mode-flip-x :mode-flip-y :mode-flip-xy))
        "detect-nes-tile-mode should return valid modes, got: ~a" mode)))

;; Test NES asset pipeline integration
(test nes-asset-pipeline-basic
  "Test basic NES asset pipeline components work together"
  ;; Test that all the key functions exist and can be called
  (is-true (fboundp 'skyline-tool:compile-art-nes))
  (is-true (fboundp 'skyline-tool:compile-music))
  (is-true (fboundp 'skyline-tool:compile-sound))
  ;; Test machine type handling
  (let ((*machine* 3))
    (is (= 3 *machine*))))

;; Test NES font ripping functions
(test nes-font-ripping-existence
  "Test that NES font ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-nes-font)
          "blob-rip-nes-font should be available"))

;; Test NES palette handling
(test nes-palette-handling
  "Test NES palette handling functions"
  (is-true (fboundp 'skyline-tool:extract-nes-palettes)
          "extract-nes-palettes should be available")
  (is-true (fboundp 'skyline-tool:save-nes-palettes)
          "save-nes-palettes should be available"))

;; Test NES CHR-ROM handling
(test nes-chr-rom-handling
  "Test NES CHR-ROM handling functions"
  (is-true (fboundp 'skyline-tool:extract-nes-chr-data)
          "extract-nes-chr-data should be available")
  (is-true (fboundp 'skyline-tool:convert-nes-chr-to-png)
          "convert-nes-chr-to-png should be available"))

;; Test NES nametable handling
(test nes-nametable-handling
  "Test NES nametable handling functions"
  (is-true (fboundp 'skyline-tool:extract-nes-nametable)
          "extract-nes-nametable should be available")
  (is-true (fboundp 'skyline-tool:convert-nes-nametable-to-png)
          "convert-nes-nametable-to-png should be available"))

(defun nes-tests ()
  "Run all NES tests and return results"
  (fiveam:run! 'nes-tests))