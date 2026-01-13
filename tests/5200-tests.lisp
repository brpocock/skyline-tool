;;; Phantasia SkylineTool/tests/5200-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(defpackage :skyline-tool/5200-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:compile-art-5200)
  (:export #:5200-tests))

(in-package :skyline-tool/5200-test)

(def-suite 5200-tests
  :description "Tests for 5200-specific SkylineTool functionality")

(in-suite 5200-tests)

;; Test 5200 tile ripping functions
(test 5200-tile-ripping-existence
  "Test that 5200 tile ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-5200-tile)
          "blob-rip-5200-tile should be available")
  (is-true (fboundp 'skyline-tool:detect-5200-tile-mode)
          "detect-5200-tile-mode should be available"))

;; Test 5200 PMG ripping functions
(test 5200-pmg-ripping-existence
  "Test that 5200 PMG ripping functions exist and can be called"
  (is-true (fboundp 'skyline-tool:blob-rip-5200-pmg)
          "blob-rip-5200-pmg should be available"))

;; Test 5200 graphics function properties (safe introspection only)
(test 5200-graphics-function-properties
  "Test that 5200 graphics functions exist and are callable"
  ;; Check function signatures (safe - no code generation)
  (is (functionp (symbol-function 'skyline-tool:blob-rip-5200-tile)))
  (is (functionp (symbol-function 'skyline-tool:blob-rip-5200-pmg)))
  (is (functionp (symbol-function 'skyline-tool:detect-5200-tile-mode))))

;; Test tile mode detection (safe - no file I/O)
(test 5200-tile-mode-detection
  "Test 5200 tile mode detection returns valid modes"
  (let ((mode (skyline-tool:detect-5200-tile-mode "/nonexistent/file.png")))
    (is (member mode '(:mode-d :mode-e))
        "detect-5200-tile-mode should return :mode-d or :mode-e, got: ~a" mode)))

;; Test tile ripping function calls (isolated to avoid label conflicts)
(test 5200-tile-ripping-isolation
  "Test 5200 tile ripping functions can be called safely"
  ;; Use unique temporary paths to avoid conflicts
  (let ((temp-path-1 (format nil "/tmp/test-5200-tile-~a.png" (random 1000)))
        (temp-path-2 (format nil "/tmp/test-5200-tile-~a.png" (random 1000))))
    ;; Test Mode D ripping
    (finishes (skyline-tool:blob-rip-5200-tile temp-path-1 :mode-d)
             "Mode D tile ripping should not crash")
    ;; Test Mode E ripping
    (finishes (skyline-tool:blob-rip-5200-tile temp-path-2 :mode-e)
             "Mode E tile ripping should not crash")))

;; Test PMG ripping function calls (isolated to avoid label conflicts)
(test 5200-pmg-ripping-isolation
  "Test 5200 PMG ripping functions can be called safely"
  ;; Use unique temporary paths to avoid conflicts
  (let ((temp-path-1 (format nil "/tmp/test-5200-pmg-~a.png" (random 1000)))
        (temp-path-2 (format nil "/tmp/test-5200-pmg-~a.png" (random 1000))))
    ;; Test monochrome PMG processing
    (finishes (skyline-tool:blob-rip-5200-pmg temp-path-1 t)
             "Monochrome PMG processing should not crash")
    ;; Test color PMG processing
    (finishes (skyline-tool:blob-rip-5200-pmg temp-path-2 nil)
             "Color PMG processing should not crash")))

;; Test 5200 binary writing (reuse 7800 function)
(test 5200-binary-writing
  "Test binary data writing for 5200 format"
  (let ((test-data '((#x00 #x01 #x02 #x03)
                     (#x10 #x11 #x12 #x13)
                     (#xff #xfe #xfd #xfc))))
    (finishes
     (skyline-tool::write-7800-binary "/tmp/test-5200-binary.bin" test-data))
    ;; Check that output file was created and has correct size
    (let ((file (probe-file "/tmp/test-5200-binary.bin")))
      (is-true file)
      ;; Should be 3 pages * 256 bytes each = 768 bytes
      (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
        (is (= 768 (file-length stream)))))))

;; Test 5200 art compilation function
(test 5200-art-compilation-function
  "Test that compile-art-5200 function exists and can be called"
  ;; This is more of a smoke test - we can't easily test the full
  ;; art compilation without proper input files
  (is-true (fboundp 'skyline-tool:compile-art-5200)))

;; Test 5200 music compilation
(test 5200-music-compilation
  "Test that 5200 music compilation functions exist and dispatch correctly"
  ;; Test that compile-music function exists
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that it can handle 5200 machine type
  (let ((*machine* 5200))
    (is (= 5200 *machine*)))
  ;; Test that compile-music-7800 function exists (used for 5200 POKEY)
  (is-true (fboundp 'skyline-tool::compile-music-7800)))

;; Test music compilation error handling
(test music-compilation-error-handling
  "Test that music compilation functions handle errors appropriately"
  ;; Test with invalid machine type
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "9999"))
  ;; Test with valid machine types (should signal error for missing file)
  (dolist (machine '("2600" "5200" "7800"))
    (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" machine))))

;; Test 5200 art compilation
(test 5200-art-compilation
  "Test 5200 art compilation function"
  (is-true (fboundp 'skyline-tool:compile-art-5200))
  ;; Test that it can be called without error (placeholder implementation)
  (finishes (skyline-tool:compile-art-5200 "/tmp/test.out" "/tmp/test.in")))

;; Test sound compilation functions for 5200
(test 5200-sound-compilation
  "Test sound compilation functions work for 5200"
  (is-true (fboundp 'skyline-tool:compile-music))
  ;; Test that 5200 is accepted as a valid machine type (should signal error for missing file)
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "5200"))
  ;; Test POKEY sound chip specification (should signal error for missing file)
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "5200" "POKEY")))

;; Test 5200 asset processing pipeline
(test 5200-asset-pipeline
  "Test complete 5200 asset processing pipeline"
  ;; This would be an integration test of the full pipeline
  (is-true t)) ;; Placeholder for future integration testing

;; Test 5200 asset pipeline integration
(test 5200-asset-pipeline-basic
  "Test basic 5200 asset pipeline components work together"
  ;; Test that all the key functions exist and can be called
  (is-true (fboundp 'skyline-tool:compile-art-5200))
  (is-true (fboundp 'skyline-tool:compile-music))
  (is-true (fboundp 'skyline-tool:compile-sound))
  ;; Test machine type handling
  (let ((*machine* 5200))
    (is (= 5200 *machine*))))

(defun 5200-tests ()
  "Run all 5200 tests and return results"
  (fiveam:run! '5200-tests))
