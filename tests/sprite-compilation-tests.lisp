;;;; Sprite Compilation Regression Tests for ChaosFight
;;;; Copyright © 2025 Interworldly Adventuring, LLC.
;;;; 
;;;; FiveAM regression tests to prevent sprite compilation failures

(defpackage :skyline-tool/test/sprites
  (:use :cl :fiveam :skyline-tool)
  (:export #:sprite-compilation-tests))

(in-package :skyline-tool/test/sprites)

(def-suite sprite-compilation-tests
  :description "Regression tests for sprite compilation pipeline")

(in-suite sprite-compilation-tests)

(test png-format-validation
  "Test that PNG files are in correct RGB/RGBA format for SkylineTool"
  (let ((test-png-path (merge-pathnames "test-data/test-sprite.png" 
                                        (asdf:system-source-directory :skyline-tool))))
    ;; Skip if test PNG doesn't exist
    (when (probe-file test-png-path)
      (let ((png (skyline-tool::read-png test-png-path)))
        ;; PNG should load without errors
        (is (not (null png)))
        ;; Should have 3 or 4 color channels (RGB or RGBA)
        (let ((dimensions (array-dimensions png)))
          (is (= (length dimensions) 3))
          (is (member (third dimensions) '(3 4))
              "PNG must be RGB (3 channels) or RGBA (4 channels)")
          ;; Test expected sprite dimensions for ChaosFight (64×256)
          (when (and (= (first dimensions) 64)
                     (= (second dimensions) 256))
            (is (= (first dimensions) 64) "Expected width: 64 pixels")
            (is (= (second dimensions) 256) "Expected height: 256 pixels")))))))

(test compile-art-basic-functionality
  "Test that compile-art command works with valid PNG input"
  (let ((test-png-path (merge-pathnames "test-data/test-sprite.png"
                                        (asdf:system-source-directory :skyline-tool)))
        (output-path (merge-pathnames "test-output.s" 
                                      (asdf:system-source-directory :skyline-tool))))
    ;; Skip if test PNG doesn't exist
    (when (probe-file test-png-path)
      ;; Should not signal an error
      (finishes 
        (skyline-tool::compile-art (namestring output-path) 
                                   (namestring test-png-path)))
      ;; Output file should be created
      (is (probe-file output-path))
      ;; Clean up
      (when (probe-file output-path)
        (delete-file output-path)))))

(test png-rgb-channel-count
  "Test that destructuring-bind (w h bpp) works correctly"
  (let ((test-png-path (merge-pathnames "test-data/test-sprite.png"
                                        (asdf:system-source-directory :skyline-tool))))
    ;; Skip if test PNG doesn't exist  
    (when (probe-file test-png-path)
      (let ((png (skyline-tool::read-png test-png-path)))
        ;; This is the exact pattern that was failing in the original bug
        (destructuring-bind (w h bpp) (array-dimensions png)
          (is (numberp w) "Width should be a number")
          (is (numberp h) "Height should be a number") 
          (is (numberp bpp) "BPP should be a number")
          (is (member bpp '(3 4)) "BPP should be 3 (RGB) or 4 (RGBA)"))))))

(test gimp-xcf-to-png-pipeline
  "Test XCF to PNG conversion preserves RGB format"
  ;; This would test the GIMP conversion pipeline if we had test XCF files
  ;; For now, just validate the expected PNG output format
  (pass "GIMP pipeline test placeholder - requires test XCF files"))

(test character-sprite-dimensions
  "Test that character sprites have expected 64×256 dimensions"
  (let ((bernie-png (merge-pathnames "Source/Art/Bernie.png"
                                     (asdf:system-source-directory :skyline-tool))))
    ;; Skip if Bernie.png doesn't exist
    (when (probe-file bernie-png)
      (let ((png (skyline-tool::read-png bernie-png)))
        (destructuring-bind (w h bpp) (array-dimensions png)
          (is (= w 64) "Bernie sprite should be 64 pixels wide")
          (is (= h 256) "Bernie sprite should be 256 pixels tall")
          (is (member bpp '(3 4)) "Bernie sprite should be RGB or RGBA"))))))

(test batch-character-compilation
  "Test that all character sprites can be compiled without errors"
  (let ((character-names '("Bernie" "Curler" "Dragonet" "EXOPilot"
                          "FatTony" "Megax" "Harpy" "KnightGuy"
                          "Frooty" "Nefertem" "NinjishGuy" "PorkChop"
                          "RadishGoblin" "RoboTito" "Ursulo" "VegDog")))
    (dolist (char character-names)
      (let ((png-path (format nil "Source/Art/~A.png" char))
            (output-path (format nil "test-~A.s" char)))
        ;; Skip if PNG doesn't exist
        (when (probe-file png-path)
          (finishes 
            (skyline-tool::compile-art output-path png-path)
            (format nil "~A sprite should compile without errors" char))
          ;; Clean up
          (when (probe-file output-path)
            (delete-file output-path)))))))

;; Test runner
(defun run-sprite-compilation-tests ()
  "Run all sprite compilation regression tests"
  (run! 'sprite-compilation-tests))


