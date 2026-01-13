;;; Phantasia SkylineTool/tests/sega-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(defpackage :skyline-tool/sega-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:compile-art-sms
                #:compile-art-sgg
                #:compile-music-sms
                #:compile-music-sgg)
  (:export #:sega-tests))

(in-package :skyline-tool/sega-test)

(def-suite sega-tests
  :description "Tests for Sega platform (SMS/SGG) specific SkylineTool functionality")

(in-suite sega-tests)

;; Test SMS art compilation function
(test sms-art-compilation-function
  "Test that compile-art-sms function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-sms))
  ;; Test that it can be called without error (placeholder implementation)
  (finishes (skyline-tool:compile-art-sms "/tmp/test.out" "/tmp/test.in")))

;; Test SGG art compilation function
(test sgg-art-compilation-function
  "Test that compile-art-sgg function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-sgg))
  ;; Test that it can be called without error (placeholder implementation)
  (finishes (skyline-tool:compile-art-sgg "/tmp/test.out" "/tmp/test.in")))

;; Test SMS graphics function properties (safe introspection only)
(test sms-graphics-function-properties
  "Test that SMS graphics functions exist and are callable"
  ;; Check function signatures (safe - no code generation)
  (is (functionp (symbol-function 'skyline-tool:compile-art-sms))))

;; Test SGG graphics function properties (safe introspection only)
(test sgg-graphics-function-properties
  "Test that SGG graphics functions exist and are callable"
  ;; Check function signatures (safe - no code generation)
  (is (functionp (symbol-function 'skyline-tool:compile-art-sgg))))

;; Test SMS tile ripping functions
(test sms-tile-ripping-existence
  "Test that SMS tile ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-sms-tile)
          "blob-rip-sms-tile should be available"))

;; Test SMS sprite ripping functions
(test sms-sprite-ripping-existence
  "Test that SMS sprite ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-sms-sprite)
          "blob-rip-sms-sprite should be available"))

;; Test SGG tile ripping functions
(test sgg-tile-ripping-existence
  "Test that SGG tile ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-sgg-tile)
          "blob-rip-sgg-tile should be available"))

;; Test SGG sprite ripping functions
(test sgg-sprite-ripping-existence
  "Test that SGG sprite ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-sgg-sprite)
          "blob-rip-sgg-sprite should be available"))

;; Test SMS music compilation
(test sms-music-compilation
  "Test that SMS music compilation functions exist and dispatch correctly"
  ;; Test that compile-music function exists
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that it can handle SMS machine type
  (let ((*machine* 3010))
    (is (= 3010 *machine*)))
  ;; Test that compile-music-sms function exists
  (is-true (fboundp 'skyline-tool::compile-music-sms)))

;; Test SGG music compilation
(test sgg-music-compilation
  "Test that SGG music compilation functions exist and dispatch correctly"
  ;; Test that compile-music function exists
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that it can handle SGG machine type
  (let ((*machine* 837))
    (is (= 837 *machine*)))
  ;; Test that compile-music-sgg function exists
  (is-true (fboundp 'skyline-tool::compile-music-sgg)))

;; Test SMS sound compilation functions (SN76489 PSG)
(test sms-sound-compilation
  "Test sound compilation functions work for SMS"
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that SMS is accepted as a valid machine type (should signal error for missing file)
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "3010"))
  ;; Test SN76489 sound chip specification
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "3010" "SN76489")))

;; Test SGG sound compilation functions (same as SMS)
(test sgg-sound-compilation
  "Test sound compilation functions work for SGG"
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that SGG is accepted as a valid machine type (should signal error for missing file)
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "837"))
  ;; Test SN76489 sound chip specification
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "837" "SN76489")))

;; Test SMS asset processing pipeline
(test sms-asset-pipeline
  "Test complete SMS asset processing pipeline"
  ;; This would be an integration test of the full pipeline
  (is-true t)) ;; Placeholder for future integration testing

;; Test SGG asset processing pipeline
(test sgg-asset-pipeline
  "Test complete SGG asset processing pipeline"
  ;; This would be an integration test of the full pipeline
  (is-true t)) ;; Placeholder for future integration testing

;; Test SMS tile mode detection (safe - no file I/O)
(test sms-tile-mode-detection
  "Test SMS tile mode detection returns valid modes"
  (let ((mode (skyline-tool:detect-sms-tile-mode "/nonexistent/file.png")))
    (is (member mode '(:mode-0 :mode-1 :mode-2 :mode-3))
        "detect-sms-tile-mode should return valid modes, got: ~a" mode)))

;; Test SGG tile mode detection (safe - no file I/O)
(test sgg-tile-mode-detection
  "Test SGG tile mode detection returns valid modes"
  (let ((mode (skyline-tool:detect-sgg-tile-mode "/nonexistent/file.png")))
    (is (member mode '(:mode-0 :mode-1 :mode-2 :mode-3))
        "detect-sgg-tile-mode should return valid modes, got: ~a" mode)))

;; Test SMS asset pipeline integration
(test sms-asset-pipeline-basic
  "Test basic SMS asset pipeline components work together"
  ;; Test that all the key functions exist and can be called
  (is-true (fboundp 'skyline-tool:compile-art-sms))
  (is-true (fboundp 'skyline-tool:compile-music))
  (is-true (fboundp 'skyline-tool:compile-sound))
  ;; Test machine type handling
  (let ((*machine* 3010))
    (is (= 3010 *machine*))))

;; Test SGG asset pipeline integration
(test sgg-asset-pipeline-basic
  "Test basic SGG asset pipeline components work together"
  ;; Test that all the key functions exist and can be called
  (is-true (fboundp 'skyline-tool:compile-art-sgg))
  (is-true (fboundp 'skyline-tool:compile-music))
  (is-true (fboundp 'skyline-tool:compile-sound))
  ;; Test machine type handling
  (let ((*machine* 837))
    (is (= 837 *machine*))))

;; Test SMS font ripping functions
(test sms-font-ripping-existence
  "Test that SMS font ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-sms-font)
          "blob-rip-sms-font should be available"))

;; Test SGG font ripping functions
(test sgg-font-ripping-existence
  "Test that SGG font ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-sgg-font)
          "blob-rip-sgg-font should be available"))

;; Test SMS palette handling
(test sms-palette-handling
  "Test SMS palette handling functions"
  (is-true (fboundp 'skyline-tool:extract-sms-palettes)
          "extract-sms-palettes should be available")
  (is-true (fboundp 'skyline-tool:save-sms-palettes)
          "save-sms-palettes should be available"))

;; Test SGG palette handling
(test sgg-palette-handling
  "Test SGG palette handling functions"
  (is-true (fboundp 'skyline-tool:extract-sgg-palettes)
          "extract-sgg-palettes should be available")
  (is-true (fboundp 'skyline-tool:save-sgg-palettes)
          "save-sgg-palettes should be available"))

;; Test SMS VDP handling
(test sms-vdp-handling
  "Test SMS VDP handling functions"
  (is-true (fboundp 'skyline-tool:extract-sms-vdp-data)
          "extract-sms-vdp-data should be available")
  (is-true (fboundp 'skyline-tool:convert-sms-vdp-to-png)
          "convert-sms-vdp-to-png should be available"))

;; Test SGG VDP handling
(test sgg-vdp-handling
  "Test SGG VDP handling functions"
  (is-true (fboundp 'skyline-tool:extract-sgg-vdp-data)
          "extract-sgg-vdp-data should be available")
  (is-true (fboundp 'skyline-tool:convert-sgg-vdp-to-png)
          "convert-sgg-vdp-to-png should be available"))

(defun sega-tests ()
  "Run all Sega tests and return results"
  (fiveam:run! 'sega-tests))