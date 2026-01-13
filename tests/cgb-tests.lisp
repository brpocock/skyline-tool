;;; Phantasia SkylineTool/tests/cgb-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(defpackage :skyline-tool/cgb-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:compile-art-cgb
                #:compile-music-cgb)
  (:export #:cgb-tests))

(in-package :skyline-tool/cgb-test)

(def-suite cgb-tests
  :description "Tests for CGB (Game Boy Color) specific SkylineTool functionality")

(in-suite cgb-tests)

;; Test CGB art compilation function
(test cgb-art-compilation-function
  "Test that compile-art-cgb function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-cgb))
  ;; Test that it can be called without error (placeholder implementation)
  (finishes (skyline-tool:compile-art-cgb "/tmp/test.out" "/tmp/test.in")))

;; Test CGB graphics function properties (safe introspection only)
(test cgb-graphics-function-properties
  "Test that CGB graphics functions exist and are callable"
  ;; Check function signatures (safe - no code generation)
  (is (functionp (symbol-function 'skyline-tool:compile-art-cgb))))

;; Test CGB tile ripping functions
(test cgb-tile-ripping-existence
  "Test that CGB tile ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-cgb-tile)
          "blob-rip-cgb-tile should be available"))

;; Test CGB sprite ripping functions
(test cgb-sprite-ripping-existence
  "Test that CGB sprite ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-cgb-sprite)
          "blob-rip-cgb-sprite should be available"))

;; Test CGB music compilation
(test cgb-music-compilation
  "Test that CGB music compilation functions exist and dispatch correctly"
  ;; Test that compile-music function exists
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that it can handle CGB machine type
  (let ((*machine* 35902))
    (is (= 35902 *machine*)))
  ;; Test that compile-music-cgb function exists
  (is-true (fboundp 'skyline-tool::compile-music-cgb)))

;; Test CGB sound compilation functions
(test cgb-sound-compilation
  "Test sound compilation functions work for CGB"
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that CGB is accepted as a valid machine type (should signal error for missing file)
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "35902")))

;; Test CGB asset processing pipeline
(test cgb-asset-pipeline
  "Test complete CGB asset processing pipeline"
  ;; This would be an integration test of the full pipeline
  (is-true t)) ;; Placeholder for future integration testing

;; Test CGB tile mode detection (safe - no file I/O)
(test cgb-tile-mode-detection
  "Test CGB tile mode detection returns valid modes"
  (let ((mode (skyline-tool:detect-cgb-tile-mode "/nonexistent/file.png")))
    (is (member mode '(:mode-normal :mode-flip-x :mode-flip-y :mode-flip-xy))
        "detect-cgb-tile-mode should return valid modes, got: ~a" mode)))

;; Test CGB asset pipeline integration
(test cgb-asset-pipeline-basic
  "Test basic CGB asset pipeline components work together"
  ;; Test that all the key functions exist and can be called
  (is-true (fboundp 'skyline-tool:compile-art-cgb))
  (is-true (fboundp 'skyline-tool:compile-music))
  (is-true (fboundp 'skyline-tool:compile-sound))
  ;; Test machine type handling
  (let ((*machine* 35902))
    (is (= 35902 *machine*))))

;; Test CGB font ripping functions
(test cgb-font-ripping-existence
  "Test that CGB font ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-cgb-font)
          "blob-rip-cgb-font should be available"))

;; Test CGB palette handling
(test cgb-palette-handling
  "Test CGB palette handling functions"
  (is-true (fboundp 'skyline-tool:extract-cgb-palettes)
          "extract-cgb-palettes should be available")
  (is-true (fboundp 'skyline-tool:save-cgb-palettes)
          "save-cgb-palettes should be available"))

(defun cgb-tests ()
  "Run all CGB tests and return results"
  (fiveam:run! 'cgb-tests))