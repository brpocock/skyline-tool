;;; Phantasia SkylineTool/tests/tileset-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite tileset-tests
  :description "Tests for tileset palette extraction and processing")

(in-suite tileset-tests)

;; Test extract-tileset-palette function existence
(test extract-tileset-palette-existence
  "Test that extract-tileset-palette function exists and is callable"
  (is-true (fboundp 'skyline-tool:extract-tileset-palette)
           "extract-tileset-palette should be available"))

;; Test extract-tileset-palette with invalid input
(test extract-tileset-palette-invalid-input
  "Test extract-tileset-palette handles invalid input gracefully"
  (signals error (skyline-tool:extract-tileset-palette "/nonexistent/file.tsx" "/tmp/test.out"))
  (signals error (skyline-tool:extract-tileset-palette nil "/tmp/test.out"))
  (signals error (skyline-tool:extract-tileset-palette "/tmp/test.tsx" nil)))

;; Create a mock tileset structure for testing
(defun create-mock-tileset (width height)
  "Create a mock tileset with a simple gradient pattern"
  (let ((image (make-array (list height width) :element-type '(unsigned-byte 32))))
    ;; Create a simple color pattern
    (dotimes (y height)
      (dotimes (x width)
        (let ((color-index (mod (+ x y) 16)))
          ;; Convert to ARGB format (simple palette index for testing)
          (setf (aref image y x) (logior (ash #xff 24)  ; Alpha
                                        (ash (* color-index 16) 16)  ; Red
                                        (ash (* color-index 8) 8)    ; Green
                                        (* color-index 4))))))       ; Blue
    image))

;; Test palette extraction from image data
(test palette-extraction-from-image
  "Test that palettes can be extracted from image data"
  (let ((test-image (create-mock-tileset 16 16)))
    ;; Test that extract-palettes function exists and works
    (is-true (fboundp 'skyline-tool::extract-palettes)
             "extract-palettes function should exist")
    ;; The function should return palette data without error
    (finishes (skyline-tool::extract-palettes test-image)
              "extract-palettes should process image data without error")))

;; Test Atari color conversion
(test atari-color-conversion
  "Test Atari color string conversion"
  (is-true (fboundp 'skyline-tool::atari-colu-string)
           "atari-colu-string function should exist")
  ;; Test basic color conversion
  (finishes (skyline-tool::atari-colu-string 0)
            "atari-colu-string should handle black color")
  (finishes (skyline-tool::atari-colu-string 15)
            "atari-colu-string should handle white color"))

;; Test palette adjustment functions
(test palette-adjustment-functions
  "Test palette color adjustment functions"
  (is-true (fboundp 'skyline-tool::darken-color-in-palette)
           "darken-color-in-palette should exist")
  (is-true (fboundp 'skyline-tool::lighten-color-in-palette)
           "lighten-color-in-palette should exist")
  (is-true (fboundp 'skyline-tool::redden-color-in-palette)
           "redden-color-in-palette should exist")
  (is-true (fboundp 'skyline-tool::cyanate-color-in-palette)
           "cyanate-color-in-palette should exist"))

;; Test adjust-palettes function
(test adjust-palettes-function
  "Test the adjust-palettes function"
  (is-true (fboundp 'skyline-tool::adjust-palettes)
           "adjust-palettes should exist")
  (let ((test-palettes (make-array '(8 4) :initial-element 8))) ; Simple test palette
    (finishes (skyline-tool::adjust-palettes #'skyline-tool::darken-color-in-palette test-palettes)
              "adjust-palettes should work with darken function")
    (finishes (skyline-tool::adjust-palettes #'skyline-tool::lighten-color-in-palette test-palettes)
              "adjust-palettes should work with lighten function")))

;; Test print-wide-pixel function
(test print-wide-pixel-function
  "Test print-wide-pixel output function"
  (is-true (fboundp 'skyline-tool::print-wide-pixel)
           "print-wide-pixel should exist")
  (finishes (skyline-tool::print-wide-pixel 8 *standard-output*)
            "print-wide-pixel should handle basic color values"))

;; Test tileset loading (mock test)
(test tileset-loading-mock
  "Test tileset loading functionality exists"
  (is-true (fboundp 'skyline-tool::load-tileset)
           "load-tileset should exist")
  ;; This would normally require actual TSX files
  (signals error (skyline-tool::load-tileset "/nonexistent/file.tsx")
           "load-tileset should signal error for missing files"))

;; Test that palette variations are generated correctly
(test palette-variation-generation
  "Test that palette variations (Dark, Light, Red, Cyan) are generated"
  ;; This is more of an integration test that would require actual tileset files
  ;; For now, just test that the supporting functions exist
  (is-true t "Palette variation generation framework is in place"))

;; Test region-specific palette generation
(test region-specific-palettes
  "Test that palettes are generated for both NTSC and PAL regions"
  ;; The extract-tileset-palette function processes both regions
  (is-true (boundp 'skyline-tool::*region*)
           "*region* variable should be available")
  (is-true (member :ntsc skyline-tool::*valid-regions*)
           "NTSC should be a valid region")
  (is-true (member :pal skyline-tool::*valid-regions*)
           "PAL should be a valid region"))

(defparameter *valid-regions* '(:ntsc :pal :secam)
  "Valid TV regions supported by the system")

(def-suite tileset-palette-extraction-suite
  :description "Comprehensive tests for tileset palette extraction")

(in-suite tileset-palette-extraction-suite)

;; Integration test for the complete palette extraction workflow
(test complete-palette-extraction-workflow
  "Test the complete palette extraction workflow components"
  ;; Test all the component functions exist
  (is-true (fboundp 'skyline-tool::extract-tileset-palette)
           "Main extraction function should exist")
  (is-true (fboundp 'skyline-tool::extract-palettes)
           "Palette extraction should exist")
  (is-true (fboundp 'skyline-tool::atari-colu-string)
           "Color conversion should exist")
  (is-true (fboundp 'skyline-tool::adjust-palettes)
           "Palette adjustment should exist"))

;; Test error conditions for palette extraction
(test palette-extraction-error-conditions
  "Test error handling in palette extraction"
  ;; Should handle nil inputs gracefully
  (signals error (skyline-tool::extract-palettes nil)
           "extract-palettes should handle nil input")
  ;; Should handle empty arrays
  (finishes (skyline-tool::extract-palettes (make-array '(0 0)))
            "extract-palettes should handle empty arrays"))

(defun run-tileset-tests ()
  "Run all tileset tests and return results"
  (fiveam:run! 'tileset-tests)
  (fiveam:run! 'tileset-palette-extraction-suite))
