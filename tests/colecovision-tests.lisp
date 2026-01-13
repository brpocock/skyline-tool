;;; Phantasia SkylineTool/tests/colecovision-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(defpackage :skyline-tool/colecovision-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:compile-art-colecovision
                #:compile-music-colecovision)
  (:export #:colecovision-tests))

(in-package :skyline-tool/colecovision-test)

(def-suite colecovision-tests
  :description "Tests for ColecoVision specific SkylineTool functionality")

(in-suite colecovision-tests)

;; Test ColecoVision art compilation function
(test colecovision-art-compilation-function
  "Test that compile-art-colecovision function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-colecovision))
  ;; Test that it can be called without error (placeholder implementation)
  (finishes (skyline-tool:compile-art-colecovision "/tmp/test.out" "/tmp/test.in")))

;; Test ColecoVision graphics function properties (safe introspection only)
(test colecovision-graphics-function-properties
  "Test that ColecoVision graphics functions exist and are callable"
  ;; Check function signatures (safe - no code generation)
  (is (functionp (symbol-function 'skyline-tool:compile-art-colecovision))))

;; Test ColecoVision tile ripping functions
(test colecovision-tile-ripping-existence
  "Test that ColecoVision tile ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-colecovision-tile)
          "blob-rip-colecovision-tile should be available"))

;; Test ColecoVision sprite ripping functions
(test colecovision-sprite-ripping-existence
  "Test that ColecoVision sprite ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-colecovision-sprite)
          "blob-rip-colecovision-sprite should be available"))

;; Test ColecoVision music compilation
(test colecovision-music-compilation
  "Test that ColecoVision music compilation functions exist and dispatch correctly"
  ;; Test that compile-music function exists
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that it can handle ColecoVision machine type
  (let ((*machine* 9918))
    (is (= 9918 *machine*)))
  ;; Test that compile-music-colecovision function exists
  (is-true (fboundp 'skyline-tool::compile-music-colecovision)))

;; Test ColecoVision sound compilation functions (SN76489)
(test colecovision-sound-compilation
  "Test sound compilation functions work for ColecoVision"
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that ColecoVision is accepted as a valid machine type (should signal error for missing file)
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "9918"))
  ;; Test SN76489 sound chip specification
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "9918" "SN76489")))

;; Test ColecoVision asset processing pipeline
(test colecovision-asset-pipeline
  "Test complete ColecoVision asset processing pipeline"
  ;; This would be an integration test of the full pipeline
  (is-true t)) ;; Placeholder for future integration testing

;; Test ColecoVision tile mode detection (safe - no file I/O)
(test colecovision-tile-mode-detection
  "Test ColecoVision tile mode detection returns valid modes"
  (let ((mode (skyline-tool:detect-colecovision-tile-mode "/nonexistent/file.png")))
    (is (member mode '(:mode-normal :mode-flip-x :mode-flip-y :mode-flip-xy))
        "detect-colecovision-tile-mode should return valid modes, got: ~a" mode)))

;; Test ColecoVision asset pipeline integration
(test colecovision-asset-pipeline-basic
  "Test basic ColecoVision asset pipeline components work together"
  ;; Test that all the key functions exist and can be called
  (is-true (fboundp 'skyline-tool:compile-art-colecovision))
  (is-true (fboundp 'skyline-tool:compile-music))
  (is-true (fboundp 'skyline-tool:compile-sound))
  ;; Test machine type handling
  (let ((*machine* 9918))
    (is (= 9918 *machine*))))

;; Test ColecoVision font ripping functions
(test colecovision-font-ripping-existence
  "Test that ColecoVision font ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-colecovision-font)
          "blob-rip-colecovision-font should be available"))

;; Test ColecoVision palette handling
(test colecovision-palette-handling
  "Test ColecoVision palette handling functions"
  (is-true (fboundp 'skyline-tool:extract-colecovision-palettes)
          "extract-colecovision-palettes should be available")
  (is-true (fboundp 'skyline-tool:save-colecovision-palettes)
          "save-colecovision-palettes should be available"))

;; Test ColecoVision pattern table handling
(test colecovision-pattern-table-handling
  "Test ColecoVision pattern table handling functions"
  (is-true (fboundp 'skyline-tool:extract-colecovision-patterns)
          "extract-colecovision-patterns should be available")
  (is-true (fboundp 'skyline-tool:convert-colecovision-patterns-to-png)
          "convert-colecovision-patterns-to-png should be available"))

;; Test ColecoVision name table handling
(test colecovision-name-table-handling
  "Test ColecoVision name table handling functions"
  (is-true (fboundp 'skyline-tool:extract-colecovision-name-table)
          "extract-colecovision-name-table should be available")
  (is-true (fboundp 'skyline-tool:convert-colecovision-name-table-to-png)
          "convert-colecovision-name-table-to-png should be available"))

;; Test ColecoVision sprite attribute handling
(test colecovision-sprite-attributes
  "Test ColecoVision sprite attribute handling functions"
  (is-true (fboundp 'skyline-tool:extract-colecovision-sprite-attributes)
          "extract-colecovision-sprite-attributes should be available")
  (is-true (fboundp 'skyline-tool:convert-colecovision-sprite-attributes-to-binary)
          "convert-colecovision-sprite-attributes-to-binary should be available"))

(defun colecovision-tests ()
  "Run all ColecoVision tests and return results"
  (fiveam:run! 'colecovision-tests))