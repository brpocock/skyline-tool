;;; Phantasia SkylineTool/tests/nes-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite nes-tests
  :description "Tests for NES-specific SkylineTool functionality"
  :in skyline-tool/test)

(in-suite nes-tests)

;; Test NES palette constants
(test nes-palette-constants
  "Test that NES palette constants are properly defined"
  (is-true (boundp 'skyline-tool::+nes-palette-ntsc+)
           "+nes-palette-ntsc+ should be defined")
  (is-true (boundp 'skyline-tool::+nes-palette-pal+)
           "+nes-palette-pal+ should be defined")
  (is-true (arrayp skyline-tool::+nes-palette-ntsc+)
           "+nes-palette-ntsc+ should be an array")
  (is-true (arrayp skyline-tool::+nes-palette-pal+)
           "+nes-palette-pal+ should be an array"))

;; Test NES music compilation functions
(test nes-music-compilation
  "Test NES music compilation functions"
  (is-true (fboundp 'skyline-tool::compile-music-nes)
           "compile-music-nes should exist")
  ;; Currently just signals error, but shouldn't crash
  (signals error (skyline-tool::compile-music-nes
                   (format nil "Object/~a/test-~a.s" (skyline-tool::machine-directory-name) (skyline-tool::generate-secure-random-id 8))
                   (format nil "Object/~a/test-~a.mid" (skyline-tool::machine-directory-name) (skyline-tool::generate-secure-random-id 8)))
           "compile-music-nes should signal error (not yet implemented)"))

;; Test NES monochrome detection
(test nes-monochrome-detection
  "Test NES monochrome line detection"
  (is-true (fboundp 'skyline-tool::monochrome-lines-p)
           "monochrome-lines-p should exist")

  ;; Test with mock palette data
  (let ((test-palette (make-array '(3 18) :element-type '(unsigned-byte 8))))
    ;; Fill with test data - multiple colors per row to test non-monochrome
    (dotimes (row 18)
      (dotimes (col 3)
        (setf (aref test-palette col row) (mod (+ col (* row 3)) 256))))
    (is-false (skyline-tool::monochrome-lines-p test-palette 18 3)
               "monochrome-lines-p should return false for multi-color palette")))

;; Test NES platform in dispatch system
(test nes-platform-dispatch
  "Test NES platform integration in dispatch system"
  ;; NES should be in valid machines
  (is-true (skyline-tool::check-machine-valid 8)
           "NES (machine 8) should be a valid machine"))

;; Test NES dispatch-png% method exists and works
(test nes-dispatch-method
  "Test that NES has a dispatch-png% method"
  (is-true (find-method #'skyline-tool::dispatch-png% '() (list (list 'eql 8) t t t t t t t) nil)
           "NES should have a dispatch-png% method specialized for machine 8"))

;; Test NES CHR tile compilation
(test nes-chr-tile-compilation
  "Test NES CHR tile compilation with mock data"
  ;; Test that the compilation function exists
  (is-true (fboundp 'skyline-tool::compile-nes-chr-tiles)
           "compile-nes-chr-tiles function should exist")

  ;; Test with mock 8x8 pixel data
  (let ((mock-pixels (make-array '(8 8) :element-type '(unsigned-byte 32) :initial-element 0))
        (temp-dir (uiop:ensure-directory-pathname "/tmp/nes-test/")))
    (ensure-directories-exist temp-dir)
    ;; This test verifies the function can be called without errors
    (finishes (skyline-tool::compile-nes-chr-tiles "mock.png" temp-dir 8 8 mock-pixels)
              "compile-nes-chr-tiles should complete without errors")))

;; Test NES nametable compilation
(test nes-nametable-compilation
  "Test NES nametable compilation"
  (is-true (fboundp 'skyline-tool::compile-nes-nametable)
           "compile-nes-nametable function should exist")

  (let ((mock-pixels (make-array '(240 256) :element-type '(unsigned-byte 32) :initial-element 0))
        (temp-dir (uiop:ensure-directory-pathname "/tmp/nes-test/")))
    (ensure-directories-exist temp-dir)
    (finishes (skyline-tool::compile-nes-nametable "mock.png" temp-dir 240 256 mock-pixels)
              "compile-nes-nametable should complete without errors")))

;; Test NES sprite compilation
(test nes-sprite-compilation
  "Test NES sprite compilation"
  (is-true (fboundp 'skyline-tool::compile-nes-sprite)
           "compile-nes-sprite function should exist")

  (let ((mock-pixels (make-array '(8 8) :element-type '(unsigned-byte 32) :initial-element 0))
        (temp-dir (uiop:ensure-directory-pathname "/tmp/nes-test/")))
    (ensure-directories-exist temp-dir)
    (finishes (skyline-tool::compile-nes-sprite "mock.png" temp-dir 8 8 mock-pixels)
              "compile-nes-sprite should complete without errors")))

;; Test NES palette usage in graphics
(test nes-palette-integration
  "Test NES palette integration in graphics system"
  ;; Test that NES palettes are used in the region-based palette selection
  (let ((ntsc-palette (skyline-tool::machine-palette 8 :ntsc))
        (pal-palette (skyline-tool::machine-palette 8 :pal)))
    (is-true (arrayp ntsc-palette) "NES NTSC palette should be available")
    (is-true (arrayp pal-palette) "NES PAL palette should be available")
    (is (= (length ntsc-palette) (length pal-palette))
        "NES NTSC and PAL palettes should have same length")))

;; Test NES error conditions
(test nes-error-conditions
  "Test error handling for NES-specific functions"
  ;; Test compile-music-nes with invalid inputs
  (signals error (skyline-tool::compile-music-nes nil nil)
           "compile-music-nes should handle nil inputs")

  ;; Test monochrome-lines-p with invalid data
  (signals error (skyline-tool::monochrome-lines-p nil 0 0)
           "monochrome-lines-p should handle nil palette data"))

(defun run-nes-tests ()
  "Run all NES tests and return results"
  (fiveam:run! 'nes-tests))
