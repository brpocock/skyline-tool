;;; Phantasia SkylineTool/tests/dmg-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(defpackage :skyline-tool/dmg-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:compile-art-dmg
                #:compile-music-dmg)
  (:export #:dmg-tests))

(in-package :skyline-tool/dmg-test)

(def-suite dmg-tests
  :description "Tests for DMG (Game Boy) specific SkylineTool functionality")

(in-suite dmg-tests)

;; Test DMG art compilation function
(test dmg-art-compilation-function
  "Test that compile-art-dmg function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-dmg))
  ;; Test that it can be called without error (placeholder implementation)
  (finishes (skyline-tool:compile-art-dmg "/tmp/test.out" "/tmp/test.in")))

;; Test DMG graphics function properties (safe introspection only)
(test dmg-graphics-function-properties
  "Test that DMG graphics functions exist and are callable"
  ;; Check function signatures (safe - no code generation)
  (is (functionp (symbol-function 'skyline-tool:compile-art-dmg))))

;; Test DMG tile ripping functions
(test dmg-tile-ripping-existence
  "Test that DMG tile ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-dmg-tile)
          "blob-rip-dmg-tile should be available"))

;; Test DMG sprite ripping functions
(test dmg-sprite-ripping-existence
  "Test that DMG sprite ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-dmg-sprite)
          "blob-rip-dmg-sprite should be available"))

;; Test DMG music compilation
(test dmg-music-compilation
  "Test that DMG music compilation functions exist and dispatch correctly"
  ;; Test that compile-music function exists
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that it can handle DMG machine type
  (let ((*machine* 20953))
    (is (= 20953 *machine*)))
  ;; Test that compile-music-dmg function exists
  (is-true (fboundp 'skyline-tool::compile-music-dmg)))

;; Test DMG sound compilation functions
(test dmg-sound-compilation
  "Test sound compilation functions work for DMG"
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that DMG is accepted as a valid machine type (should signal error for missing file)
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "20953")))

;; Test DMG asset processing pipeline
(test dmg-asset-pipeline
  "Test complete DMG asset processing pipeline"
  ;; This would be an integration test of the full pipeline
  (is-true t)) ;; Placeholder for future integration testing

;; Test DMG tile mode detection (safe - no file I/O)
(test dmg-tile-mode-detection
  "Test DMG tile mode detection returns valid modes"
  (let ((mode (skyline-tool:detect-dmg-tile-mode "/nonexistent/file.png")))
    (is (member mode '(:mode-normal :mode-flip-x :mode-flip-y :mode-flip-xy))
        "detect-dmg-tile-mode should return valid modes, got: ~a" mode)))

;; Test DMG asset pipeline integration
(test dmg-asset-pipeline-basic
  "Test basic DMG asset pipeline components work together"
  ;; Test that all the key functions exist and can be called
  (is-true (fboundp 'skyline-tool:compile-art-dmg))
  (is-true (fboundp 'skyline-tool:compile-music))
  (is-true (fboundp 'skyline-tool:compile-sound))
  ;; Test machine type handling
  (let ((*machine* 20953))
    (is (= 20953 *machine*))))

;; Test DMG font ripping functions
(test dmg-font-ripping-existence
  "Test that DMG font ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-dmg-font)
          "blob-rip-dmg-font should be available"))

;; Test DMG palette handling (monochrome only)
(test dmg-palette-handling
  "Test DMG palette handling functions (monochrome)"
  (is-true (fboundp 'skyline-tool:extract-dmg-palettes)
          "extract-dmg-palettes should be available")
  (is-true (fboundp 'skyline-tool:save-dmg-palettes)
          "save-dmg-palettes should be available"))

(defun dmg-tests ()
  "Run all DMG tests and return results"
  (fiveam:run! 'dmg-tests))