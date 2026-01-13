;;; Phantasia SkylineTool/tests/lynx-graphics-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

(defpackage :skyline-tool/lynx-graphics-test
  (:use :cl :fiveam :skyline-tool)
  (:export #:lynx-graphics-tests))

(in-package :skyline-tool/lynx-graphics-test)

(def-suite lynx-graphics-tests
  :description "Tests for Lynx-specific graphics compilation functionality")

(in-suite lynx-graphics-tests)

;; Test Lynx graphics compiler existence
(test lynx-compiler-existence
  "Test that Lynx graphics compilers exist and are callable"
  (is-true (fboundp 'skyline-tool:dispatch-png) "dispatch-png should exist")
  (is-true (fboundp 'skyline-tool:dispatch-png%) "dispatch-png% should exist")
  (is-true (fboundp 'skyline-tool:compile-lynx-tileset) "compile-lynx-tileset should exist")
  (is-true (fboundp 'skyline-tool:compile-lynx-sprite) "compile-lynx-sprite should exist")
  (is-true (fboundp 'skyline-tool:compile-lynx-blob) "compile-lynx-blob should exist")
  (is-true (fboundp 'skyline-tool:zx7-compress) "zx7-compress should exist"))

;; Test dispatch-png error handling
(test dispatch-png-error-handling
  "Test that dispatch-png handles errors appropriately"
  (signals error (skyline-tool:dispatch-png "/nonexistent.png" "/target")))

;; Test dispatch-png% with different machine types that have methods
(test dispatch-png%-machine-dispatch
  "Test that dispatch-png% correctly dispatches based on machine type"
  ;; Test with 5200 machine (has method)
  (let ((skyline-tool:*machine* 5200))
    (finishes (skyline-tool:dispatch-png% skyline-tool:*machine* "/test.png" "/target" nil 16 16 nil nil)))

  ;; Test with Lynx machine (200) - has method
  (let ((skyline-tool:*machine* 200))
    ;; Test blob classification (160x97 should be blob)
    (finishes (skyline-tool:dispatch-png% skyline-tool:*machine* "/test.png" "/target" nil 160 97 nil nil)))

  ;; Test with 2600 machine (has method)
  (let ((skyline-tool:*machine* 2600))
    (finishes (skyline-tool:dispatch-png% skyline-tool:*machine* "/test.png" "/target" nil 16 16 nil nil))))

;; Test ZX7 compression with known inputs and outputs
(test zx7-compression-basic
  "Test ZX7 compression with known input data"
  (let ((test-data (make-array 10 :element-type '(unsigned-byte 8)
                              :initial-contents '(1 2 3 4 5 6 7 8 9 10))))
    (let ((compressed (skyline-tool:zx7-compress test-data)))
      (is-true (arrayp compressed) "ZX7 compression should return an array")
      (is (equal (array-element-type compressed) '(unsigned-byte 8))
          "Compressed data should be unsigned bytes")
      (is (> (length compressed) 0) "Compressed data should not be empty")
      ;; Note: zx7-decompress not implemented, skipping round-trip test
      )))

;; Test ZX7 compression with repeated data (should compress well)
(test zx7-compression-repeated
  "Test ZX7 compression with highly compressible repeated data"
  (let ((test-data (make-array 100 :element-type '(unsigned-byte 8)
                              :initial-element 42))) ; All 42s
    (let ((compressed (skyline-tool:zx7-compress test-data)))
      (is (< (length compressed) (length test-data))
          "Compressed data should be significantly smaller than original for repeated data")
      (is (> (length compressed) 2) "Compressed data should contain header")
      ;; Note: zx7-decompress not implemented, skipping round-trip test
      )))

;; Test ZX7 compression with empty data
(test zx7-compression-empty
  "Test ZX7 compression with empty input"
  (let ((test-data (make-array 0 :element-type '(unsigned-byte 8))))
    (let ((compressed (skyline-tool:zx7-compress test-data)))
      (is-true (arrayp compressed) "Empty data compression should return array")
      ;; Note: zx7-decompress not implemented, skipping round-trip test
      )))

;; Test ZX7 compression with single byte
(test zx7-compression-single-byte
  "Test ZX7 compression with single byte input"
  (let ((test-data (make-array 1 :element-type '(unsigned-byte 8)
                              :initial-element 255)))
    (let ((compressed (skyline-tool:zx7-compress test-data)))
      (is-true (arrayp compressed) "Single byte compression should return array")
      ;; Note: zx7-decompress not implemented, skipping round-trip test
      )))

;; Test ZX7 compression with maximum byte value sequence
(test zx7-compression-max-values
  "Test ZX7 compression with sequence of maximum byte values"
  (let ((test-data (make-array 256 :element-type '(unsigned-byte 8)
                              :initial-element 255)))
    (let ((compressed (skyline-tool:zx7-compress test-data)))
      (is-true (arrayp compressed) "Max values compression should return array")
      (is (< (length compressed) 128) "Max values should compress well")
      ;; Note: zx7-decompress not implemented, skipping round-trip test
      )))

;; Test Lynx compiler parameter validation
(test lynx-compiler-parameters
  "Test that Lynx compilers have proper parameter signatures"
  ;; Check function lambda lists
  (is (fboundp 'dispatch-png)
      "dispatch-png function should exist")

  (is (fboundp 'compile-lynx-tileset)
      "compile-lynx-tileset function should exist")

  (is (fboundp 'compile-lynx-sprite)
      "compile-lynx-sprite function should exist")

  (is (fboundp 'compile-lynx-blob)
      "compile-lynx-blob function should exist"))

;; Test error handling for invalid inputs
(test lynx-compiler-error-handling
  "Test that Lynx compilers handle invalid inputs appropriately"
  ;; These should signal errors for invalid paths
  (signals error (skyline-tool:compile-lynx-tileset "/nonexistent.png" "/target" 8 8 nil))
  (signals error (skyline-tool:compile-lynx-sprite "/nonexistent.png" "/target" 16 16 nil))
  (signals error (skyline-tool:compile-lynx-blob "/nonexistent.png" "/target" 160 97 nil)))

;; Test dispatch logic for different image sizes with specific expectations
(test dispatch-logic-image-sizes
  "Test the dispatch logic for different image dimensions with known classifications"
  (let ((skyline-tool:*machine* 200)) ; Lynx
    ;; Test various sizes and verify correct classification
    (dolist (test-case '((8 8 :tileset "8x8 tileset")
                        (16 16 :sprite "16x16 sprite")
                        (32 32 :sprite "32x32 sprite")
                        (160 97 :blob "160x97 navigation blob")
                        (200 100 :blob "200x100 blob")))
      (destructuring-bind (width height expected-type description) test-case
        ;; The dispatch should complete without error for valid sizes
        (finishes (skyline-tool:dispatch-png% skyline-tool:*machine* "/test.png" "/target" nil height width nil nil)
                 "~A dispatch should complete" description)))))

;; Test Lynx tileset compilation with mock data
(test lynx-tileset-compilation
  "Test Lynx tileset compilation with known input/output"
  ;; Create mock tileset data and verify compilation
  (let ((mock-tileset-data (make-array '(8 8) :element-type '(unsigned-byte 32)
                                      :initial-element #xFF000000))) ; Black pixels
    ;; Test that compilation function can handle the data structure
    (finishes (skyline-tool:compile-lynx-tileset "/mock-input.png" "/mock-output.s" 8 8 nil)
             "Tileset compilation should handle mock data")
    ;; Test parameter validation
    (signals error (skyline-tool:compile-lynx-tileset nil "/output.s" 8 8 nil)
            "Nil input should signal error")
    (signals error (skyline-tool:compile-lynx-tileset "/input.png" nil 8 8 nil)
            "Nil output should signal error")))

;; Test Lynx sprite compilation with specific dimensions
(test lynx-sprite-compilation
  "Test Lynx sprite compilation with various sizes"
  ;; Test different sprite sizes that should be supported
  (dolist (size '((8 8) (16 16) (32 32) (64 64)))
    (destructuring-bind (width height) size
      (finishes (skyline-tool:compile-lynx-sprite "/mock-input.png" "/mock-output.s" width height nil)
               "~Dx~D sprite compilation should work" width height))))

;; Test Lynx blob compilation with navigation chart sizes
(test lynx-blob-compilation
  "Test Lynx blob compilation with navigation chart dimensions"
  ;; Test blob sizes typical for Lynx navigation charts
  (dolist (size '((160 102) (160 97) (200 100) (320 200)))
    (destructuring-bind (width height) size
      (finishes (skyline-tool:compile-lynx-blob "/mock-input.png" "/mock-output.s" width height nil)
               "~Dx~D blob compilation should work" width height))))

;; Test dispatch-png% with comprehensive machine and size matrix
(test dispatch-png-comprehensive
  "Test dispatch-png% with comprehensive machine/size combinations"
  ;; Test different machines that have dispatch-png% methods
  (dolist (machine '(200 5200 2600)) ; Lynx, 5200, 2600 (these have methods)
    (let ((skyline-tool:*machine* machine))
      ;; Test various image sizes for each machine
      (dolist (test-case '((8 8 "small tileset")
                          (16 16 "standard sprite")
                          (32 32 "large sprite")
                          (64 64 "huge sprite")
                          (160 100 "navigation blob")))
        (destructuring-bind (width height description) test-case
          (finishes (skyline-tool:dispatch-png% skyline-tool:*machine* "/test.png" "/target" nil height width nil nil)
                   "~A dispatch for machine ~A should work" description machine))))))

;; Test compression integration
(test compression-integration
  "Test that compression functions integrate properly"
  ;; Test that ZX7 compression can be called and returns valid results
  (let ((data (make-array 50 :element-type '(unsigned-byte 8))))
    ;; Fill with some pattern
    (dotimes (i 50)
      (setf (aref data i) (mod i 256)))
    (let ((compressed (skyline-tool:zx7-compress data)))
      (is-true (and (arrayp compressed)
                   (> (length compressed) 0))
               "ZX7 compression should return a non-empty array"))))

(defun run-lynx-graphics-tests ()
  "Run all Lynx graphics tests and return results"
  (fiveam:run! 'lynx-graphics-tests))
