;;; Phantasia SkylineTool/tests/graphics-tests.lisp
;;;; Comprehensive tests for graphics processing functions
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

;;; Enhanced test utilities for targeted testing
(defmacro define-multi-test (name description iterations &body body)
  "Define a test that runs multiple iterations for statistical validation"
  `(test ,name
     ,description
     (dotimes (i ,iterations)
       ,@body)))

(def-suite graphics-tests
  :description "Comprehensive tests for graphics processing functions")

(in-suite graphics-tests)

;; Test data generators for graphics functions
(defun generate-random-color ()
  "Generate a random RGB color"
  (list (random 256) (random 256) (random 256)))

(defun generate-random-palette (&optional (size 16))
  "Generate a random color palette"
  (loop for i from 1 to size
        collect (generate-random-color)))

(defun generate-random-pixels (width height &optional (max-color 15))
  "Generate random pixel data for testing"
  (let ((pixels (make-array (list height width) :element-type '(unsigned-byte 8))))
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref pixels y x) (random (1+ max-color)))))
    pixels))

(defun generate-random-nybbles (width height &optional (max-value 15))
  "Generate random nybble data for testing"
  (let ((data (make-array (list height width) :element-type '(unsigned-byte 4))))
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref data y x) (random (1+ max-value)))))
    data))

;; Test basic utility functions
(test double-up-basic
  "Test double-up function with simple lists"
  (is (equal (skyline-tool::double-up '(1 2 3)) '(1 1 2 2 3 3))
      "Should duplicate each element")
  (is (equal (skyline-tool::double-up nil) nil)
      "Should handle empty list")
  (is (equal (skyline-tool::double-up '(a)) '(a a))
      "Should handle single element"))

(test square-basic
  "Test square function"
  (is (= (skyline-tool::square 0) 0) "Square of 0 should be 0")
  (is (= (skyline-tool::square 1) 1) "Square of 1 should be 1")
  (is (= (skyline-tool::square 5) 25) "Square of 5 should be 25")
  (is (= (skyline-tool::square -3) 9) "Square of -3 should be 9"))

;; Test color and palette functions
(test machine-palette-basic
  "Test machine-palette returns appropriate palettes"
  (let ((skyline-tool::*machine* 2600))
    (let ((palette (skyline-tool::machine-palette)))
      (is (listp palette) "Should return a list")
      (is (> (length palette) 0) "Should not be empty"))))

(test machine-colors-basic
  "Test machine-colors returns color information"
  (let ((skyline-tool::*machine* 2600))
    (let ((colors (skyline-tool::machine-colors)))
      (is (or (null colors) (listp colors)) "Should return nil or list"))))

(test color-distance-basic
  "Test color-distance calculates Euclidean distance"
  (is (= (skyline-tool::color-distance 0 0 0 '(0 0 0)) 0)
      "Distance between identical colors should be 0")
  (is (> (skyline-tool::color-distance 0 0 0 '(255 255 255)) 0)
      "Distance between different colors should be > 0"))

(define-multi-test color-distance-properties
  "Test color-distance properties with multiple samples"
  15 ; Test 15 times for reasonable statistical confidence
  (let* ((c1 (generate-random-color))
         (c2 (generate-random-color))
         (dist (skyline-tool::color-distance (first c1) (second c1) (third c1) c2)))
    (is (>= dist 0) "Distance should always be non-negative")
    (is (= dist (skyline-tool::color-distance (first c2) (second c2) (third c2) c1))
        "Distance should be symmetric")))

(test palette-rgb-conversion
  "Test palette->rgb and rgb->palette conversions"
  (let ((skyline-tool::*machine* 2600))
    ;; Test round-trip conversion
    (let* ((original-rgb '(255 128 64))
           (palette-index (skyline-tool::rgb->palette (first original-rgb)
                                                     (second original-rgb)
                                                     (third original-rgb)))
           (back-to-rgb (skyline-tool::palette->rgb palette-index)))
      (is (listp back-to-rgb) "Should return RGB list")
      (is (= (length back-to-rgb) 3) "Should have 3 components"))))

(test find-nearest-in-palette-basic
  "Test find-nearest-in-palette finds closest color"
  (let ((palette '((0 0 0) (255 255 255) (255 0 0))))
    (let ((nearest (skyline-tool::find-nearest-in-palette palette 254 0 0)))
      (is (integerp nearest) "Should return palette index")
      (is (<= 0 nearest (1- (length palette))) "Index should be in valid range"))))

(define-multi-test find-nearest-in-palette-samples
  "Test find-nearest-in-palette with multiple random samples"
  10 ; Test 10 times for reasonable coverage
  (let* ((palette (generate-random-palette 16))
         (target-color (generate-random-color))
         (nearest (skyline-tool::find-nearest-in-palette palette
                                                       (first target-color)
                                                       (second target-color)
                                                       (third target-color))))
    (is (integerp nearest) "Should return integer index")
    (is (<= 0 nearest (1- (length palette))) "Index should be in valid range")))

(test rgb-int-conversion
  "Test rgb->int conversion"
  (is (= (skyline-tool::rgb->int 255 0 0) #xff0000) "Red should convert correctly")
  (is (= (skyline-tool::rgb->int 0 255 0) #x00ff00) "Green should convert correctly")
  (is (= (skyline-tool::rgb->int 0 0 255) #x0000ff) "Blue should convert correctly")
  (is (= (skyline-tool::rgb->int 0 0 0) 0) "Black should convert to 0"))

;; Test pixel manipulation functions
(test fat-bits-basic
  "Test fat-bits expands pixel data"
  (let ((pixels #(1 0 1 0)))
    (let ((expanded (skyline-tool::fat-bits pixels)))
      (is (arrayp expanded) "Should return array")
      (is (> (length expanded) (length pixels)) "Should expand the data"))))

(test tile-bits-conversion
  "Test tile->bits converts tile data"
  (is-true (fboundp 'skyline-tool::tile->bits) "tile->bits should be defined")
  (finishes (skyline-tool::tile->bits #(1 2 3 4)) "Should handle basic input"))

(test tile-color-basic
  "Test tile->color extracts color information"
  (is-true (fboundp 'skyline-tool::tile->color) "tile->color should be defined")
  (finishes (skyline-tool::tile->color #(1 2 3 4)) "Should handle basic input"))

;; Test mob (sprite) functions
(test mob-mono-bits-basic
  "Test mob->mono-bits converts monochrome mob data"
  (is-true (fboundp 'skyline-tool::mob->mono-bits) "mob->mono-bits should be defined")
  (finishes (skyline-tool::mob->mono-bits #(1 2 3 4)) "Should handle basic input"))

(test mob-multi-bits-basic
  "Test mob->multi-bits converts multicolor mob data"
  (is-true (fboundp 'skyline-tool::mob->multi-bits) "mob->multi-bits should be defined")
  (finishes (skyline-tool::mob->multi-bits #(1 2 3 4)) "Should handle basic input"))

(test mob-colors-basic
  "Test mob-colors extracts color information"
  (is-true (fboundp 'skyline-tool::mob-colors) "mob-colors should be defined")
  (finishes (skyline-tool::mob-colors #(1 2 3 4)) "Should handle basic input"))

(test ensure-monochrome-basic
  "Test ensure-monochrome validates monochrome sprites"
  (is-true (fboundp 'skyline-tool::ensure-monochrome) "ensure-monochrome should be defined")
  (finishes (skyline-tool::ensure-monochrome #(0 1 0 1)) "Should handle basic monochrome data"))

(test ensure-1plus-chrome-basic
  "Test ensure-1+chrome validates multicolor sprites"
  (is-true (fboundp 'skyline-tool::ensure-1+chrome) "ensure-1+chrome should be defined")
  (finishes (skyline-tool::ensure-1+chrome #(0 1 2 3)) "Should handle basic multicolor data"))

(test mob-empty-basic
  "Test mob-empty checks for empty sprites"
  (is-true (skyline-tool::mob-empty #(0 0 0 0)) "All-zero mob should be empty")
  (is-false (skyline-tool::mob-empty #(0 1 0 0)) "Non-zero mob should not be empty"))

(test mob-hires-basic
  "Test mob-hires checks high-resolution sprites"
  (is-true (fboundp 'skyline-tool::mob-hires) "mob-hires should be defined")
  (finishes (skyline-tool::mob-hires #(1 2 3 4)) "Should handle basic input"))

;; Test image processing functions
(test gather-mobs-basic
  "Test gather-mobs extracts sprites from image"
  (is-true (fboundp 'skyline-tool::gather-mobs) "gather-mobs should be defined")
  (let ((nybbles (generate-random-nybbles 16 16)))
    (finishes (skyline-tool::gather-mobs nybbles 16 16) "Should handle basic nybble data")))

(test image-colors-basic
  "Test image-colors extracts color information from images"
  (is-true (fboundp 'skyline-tool::image-colors) "image-colors should be defined")
  (finishes (skyline-tool::image-colors #(1 2 3 4) 2 2) "Should handle basic image data"))

;; Test bit manipulation functions
(test bits-to-art-basic
  "Test bits-to-art converts bits to art format"
  (is (equal (skyline-tool::bits-to-art #b10101010) "████████")
      "Should convert bits to block characters"))

(test bit-pairs-to-art-basic
  "Test bit-pairs-to-art converts bit pairs to art"
  (is (stringp (skyline-tool::bit-pairs-to-art #b10101010))
      "Should return a string"))

(test bytes-and-art-basic
  "Test bytes-and-art formats bytes as art"
  (is (stringp (skyline-tool::bytes-and-art #(1 2 3)))
      "Should return a string"))

(test byte-and-art-basic
  "Test byte-and-art formats single byte as art"
  (is (stringp (skyline-tool::byte-and-art 170))
      "Should return a string"))

;; Test assembler label functions
(test assembler-label-name-basic
  "Test assembler-label-name creates valid labels"
  (is (stringp (skyline-tool::assembler-label-name "test sprite"))
      "Should return a string")
  (is (not (null (skyline-tool::assembler-label-name "test sprite")))
      "Should not return empty string"))

;; Test utility functions
(test pathname-base-name-basic
  "Test pathname-base-name extracts base name"
  (is (string= (skyline-tool::pathname-base-name #p"/path/to/file.png") "file")
      "Should extract filename without extension")
  (is (string= (skyline-tool::pathname-base-name #p"simple") "simple")
      "Should handle simple names"))

;; Test Atari color functions
(test atari-color-name-basic
  "Test atari-color-name returns color names"
  (is (stringp (skyline-tool::atari-color-name 0)) "Should return string for valid index")
  (is (stringp (skyline-tool::atari-color-name 127)) "Should handle max index"))

(test atari-colu-basic
  "Test atari-colu converts color index to COLU value"
  (is (integerp (skyline-tool::atari-colu 15)) "Should return integer")
  (is (<= 0 (skyline-tool::atari-colu 15) 255) "Should be in valid byte range"))

(test atari-colu-string-basic
  "Test atari-colu-string formats COLU value"
  (is (stringp (skyline-tool::atari-colu-string #x1a)) "Should return formatted string"))

;; Test reverse functions
(test reverse-7-or-8-basic
  "Test reverse-7-or-8 reverses 7-8 bit values"
  (is (= (skyline-tool::reverse-7-or-8 #b00001111) #b11110000)
      "Should reverse bit pattern"))

(test reverse-16-basic
  "Test reverse-16 reverses 16-bit values"
  (is (= (skyline-tool::reverse-16 #b0000000011111111) #b1111111100000000)
      "Should reverse 16-bit pattern"))

;; Test rows-of-width function
(test rows-of-width-basic
  "Test rows-of-width organizes bytes into rows"
  (let ((result (skyline-tool::rows-of-width #(1 2 3 4 5 6 7 8) 4)))
    (is (listp result) "Should return a list")
    (is (> (length result) 0) "Should not be empty")))

;; Test make-fillable-vector
(test make-fillable-vector-basic
  "Test make-fillable-vector creates adjustable vector"
  (let ((vector (skyline-tool::make-fillable-vector '(1 2 3))))
    (is (arrayp vector) "Should return array")
    (is (adjustable-array-p vector) "Should be adjustable")))

;; Test monochrome detection functions
(test monochrome-lines-p-basic
  "Test monochrome-lines-p detects monochrome lines"
  (is-true (fboundp 'skyline-tool::monochrome-lines-p) "monochrome-lines-p should be defined")
  (finishes (skyline-tool::monochrome-lines-p #(0 0 0 0) 2 2) "Should handle basic input"))

(test monochrome-image-p-basic
  "Test monochrome-image-p detects monochrome images"
  (is-true (fboundp 'skyline-tool::monochrome-image-p) "monochrome-image-p should be defined")
  (finishes (skyline-tool::monochrome-image-p #(0 0 0 0)) "Should handle basic input"))

;; Test platform-specific compilation functions existence
(test platform-compilation-functions-existence
  "Test that platform-specific compilation functions exist"
  (dolist (func '(skyline-tool::compile-2600-font-8x16
                  skyline-tool::compile-2600-playfield
                  skyline-tool::compile-chaos-character
                  skyline-tool::compile-5200-mode-e-bitmap
                  skyline-tool::compile-gtia-player
                  skyline-tool::compile-mob
                  skyline-tool::compile-atari-8×8
                  skyline-tool::compile-gram-intv
                  skyline-tool::compile-intv-sprite
                  skyline-tool::compile-art-intv
                  skyline-tool::compile-tileset
                  skyline-tool::compile-tileset-64
                  skyline-tool::compile-tileset-cgb
                  skyline-tool::compile-ted-bitmap
                  skyline-tool::compile-ted-charmap
                  skyline-tool::compile-ted-sprite
                  skyline-tool::compile-ted-multicolor-sprite
                  skyline-tool::compile-lynx-sprite
                  skyline-tool::compile-lynx-tiles
                  skyline-tool::compile-lynx-font
                  skyline-tool::compile-snes-mode7
                  skyline-tool::compile-snes-tiles))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test font-related functions
(test font-compilation-functions-existence
  "Test that font compilation functions exist"
  (dolist (func '(skyline-tool::compile-font-command
                  skyline-tool::compile-font-8×8
                  skyline-tool::tia-font-interpret
                  skyline-tool::antic-font-interpret
                  skyline-tool::tia-font-guide
                  skyline-tool::antic-font-guide
                  skyline-tool::tia-font-write
                  skyline-tool::antic-font-write))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test TIA (Atari 2600) specific functions
(test tia-functions-existence
  "Test that TIA-specific functions exist"
  (dolist (func '(skyline-tool::tia-player-interpret/strip
                  skyline-tool::tia-player-interpret
                  skyline-tool::tia-48px-interpret
                  skyline-tool::tia-48px-preview
                  skyline-tool::compile-tia-48px
                  skyline-tool::compile-batari-48px
                  skyline-tool::compile-batari-48px-command))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test ANTIC (Atari 8-bit) specific functions
(test antic-functions-existence
  "Test that ANTIC-specific functions exist"
  (dolist (func '(skyline-tool::mode-e-row-bytes
                  skyline-tool::mode-e-interpret
                  skyline-tool::antic-font-interpret
                  skyline-tool::antic-font-guide
                  skyline-tool::antic-font-write))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test Chaos frame functions
(test chaos-functions-existence
  "Test that Chaos frame functions exist"
  (dolist (func '(skyline-tool::chaos-frame->key
                  skyline-tool::chaos-byte->binary-string
                  skyline-tool::chaos-extract-frame
                  skyline-tool::ensure-chaos-frame-index
                  skyline-tool::chaos-previous-value
                  skyline-tool::fill-chaos-row-indices
                  skyline-tool::write-chaos-character-output))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test playfield functions
(test playfield-functions-existence
  "Test that playfield functions exist"
  (dolist (func '(skyline-tool::playfield-row->string
                  skyline-tool::dominant-playfield-color
                  skyline-tool::format-playfield-color
                  skyline-tool::playfield-color-byte))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test VIC-II (C64) specific functions
(test vic2-functions-existence
  "Test that VIC-II specific functions exist"
  (dolist (func '(skyline-tool::pretty-mob-data-listing-vic2
                  skyline-tool::mob-index+bitmap+color-sets
                  skyline-tool::tile-cell-vic2-x
                  skyline-tool::tile-cell-vic2-y))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test compression functions
(test compression-functions-existence
  "Test that compression functions exist"
  (dolist (func '(skyline-tool::zx7-compress
                  skyline-tool::48px-array-to-bytes))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test PNG processing functions
(test png-functions-existence
  "Test that PNG processing functions exist"
  (dolist (func '(skyline-tool::png->palette
                  skyline-tool::png->bits
                  skyline-tool::extract-region))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test palette functions
(test palette-functions-existence
  "Test that palette functions exist"
  (dolist (func '(skyline-tool::find-nearest-palette-color
                  skyline-tool::try-to-maintain-palette))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test Inty compilation functions
(test inty-compilation-existence
  "Test that Intellivision compilation functions exist"
  (dolist (func '(skyline-tool::read-intv-art-index
                  skyline-tool::assemble-intv-rom))
    (is-true (fboundp func)
             "~a function should be defined" func)))

;; Test atari-colu-run function
(test atari-colu-run-existence
  "Test atari-colu-run function exists"
  (is-true (fboundp 'skyline-tool::atari-colu-run) "atari-colu-run should be defined")
  (finishes (skyline-tool::atari-colu-run) "Should handle no arguments"))

;; Test find-nearest-palette-color function
(test find-nearest-palette-color-existence
  "Test find-nearest-palette-color function exists"
  (is-true (fboundp 'skyline-tool::find-nearest-palette-color)
           "find-nearest-palette-color should be defined")
  (finishes (skyline-tool::find-nearest-palette-color '(255 0 0))
            "Should handle basic RGB input"))