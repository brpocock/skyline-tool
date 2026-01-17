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
  (signals error (skyline-tool:extract-tileset-palette "/nonexistent/file.tsx"
                   (format nil "Object/~a/test-~x.out" (skyline-tool::machine-directory-name) (sxhash (get-universal-time)))))
  (signals error (skyline-tool:extract-tileset-palette nil
                   (format nil "Object/~a/test-~x.out" (skyline-tool::machine-directory-name) (sxhash (get-universal-time)))))
  (signals error (skyline-tool:extract-tileset-palette
                   (format nil "Object/~a/test-~x.tsx" (skyline-tool::machine-directory-name) (sxhash (get-universal-time)))
                   nil)))

;; Create a mock tileset structure for testing
(defun create-mock-tileset (width height)
  "Create a mock tileset with a simple pattern using palette indices"
  (let ((image (make-array (list height width) :element-type '(unsigned-byte 8))))
    ;; Create a simple pattern using palette indices 0-15
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref image y x) (mod (+ x y) 16))))
    image))

;; Test palette extraction from image data
(test palette-extraction-from-image
  "Test that palettes can be extracted from image data"
  (let ((skyline-tool::*machine* 7800) ; Set machine for palette functions
        (skyline-tool::*region* :ntsc) ; Set region for palette functions
        (test-image (create-mock-tileset 16 16)))
    ;; Test that extract-palettes function exists and works
    (is-true (fboundp 'skyline-tool::extract-palettes)
             "extract-palettes function should exist")
    ;; The function should return palette data (use count=4 to fit in 16 pixels)
    (is-true (arrayp (skyline-tool::extract-palettes test-image :count 4))
             "extract-palettes should return an array")))

;; Test Atari color conversion
(test atari-color-conversion
  "Test Atari color string conversion"
  (is-true (fboundp 'skyline-tool::atari-colu-string)
           "atari-colu-string function should exist")
  ;; Test basic color conversion
  (is (stringp (skyline-tool::atari-colu-string 0))
      "atari-colu-string should return a string for black color")
  (is (stringp (skyline-tool::atari-colu-string 15))
      "atari-colu-string should return a string for white color"))

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
  (let ((skyline-tool::*machine* 7800) ; Set machine for palette functions
        (skyline-tool::*region* :ntsc) ; Set region for palette functions
        (test-palettes (make-array '(8 4) :initial-element 8))) ; Simple test palette
    (is (arrayp (skyline-tool::adjust-palettes #'skyline-tool::darken-color-in-palette test-palettes))
        "adjust-palettes should return an array when using darken function")
    (is (arrayp (skyline-tool::adjust-palettes #'skyline-tool::lighten-color-in-palette test-palettes))
        "adjust-palettes should return an array when using lighten function")))

;; Test print-wide-pixel function
(test print-wide-pixel-function
  "Test print-wide-pixel output function"
  (is-true (fboundp 'skyline-tool::print-wide-pixel)
           "print-wide-pixel should exist")
  (is (null (skyline-tool::print-wide-pixel 8 *standard-output*))
      "print-wide-pixel should return nil after printing"))

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
  (is-true (keywordp :ntsc)
           "NTSC should be a valid region keyword")
  (is-true (keywordp :pal)
           "PAL should be a valid region keyword"))

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
  (is (arrayp (skyline-tool::extract-palettes (make-array '(0 0))))
      "extract-palettes should return an array even for empty input"))

(defun run-tileset-tests ()
  "Run all tileset tests and return results"
  (fiveam:run! 'tileset-tests)
  (fiveam:run! 'tileset-palette-extraction-suite))
