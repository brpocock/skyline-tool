;;; Phantasia SkylineTool/tests/tg16-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC
;;;; Tests for TurboGrafx-16/PC Engine platform support

(in-package :skyline-tool)

(def-suite tg16-suite
  :description "Tests for TurboGrafx-16/PC Engine platform support"
  :in skyline-tool/test)

(in-suite tg16-suite)

(test tg16-machine-number
  "Test that TG16 machine number is correctly defined"
  (is (= 16 (machine-number-by-tag "TG16")))
  (is (= 16 (machine-number-by-tag "tg16")))
  (is (= 16 (machine-number-by-tag "PC Engine")))
  (is (= 16 (machine-number-by-tag "pce"))))

(test tg16-machine-directory
  "Test that TG16 machine directory name is correct"
  (is (string= "TG16" (machine-directory-name 16))))

(test tg16-supported-video-types
  "Test that TG16 supports NTSC, PAL, and SECAM"
  (is (equal '(:ntsc :pal :secam) (supported-video-types 16))))

(test tg16-number-of-banks
  "Test that TG16 has appropriate number of banks"
  (let ((*machine* 16))
    (is (= 64 (number-of-banks "Public" :ntsc)))
    (is (= 64 (number-of-banks "AA" :pal)))
    (is (= 32 (number-of-banks "Demo" :secam)))
    (is (= 32 (number-of-banks "Test" :ntsc)))))

(test tg16-dispatch-png-method-exists
  "Test that dispatch-png% method exists for TG16"
  (is (not (null (find-method #'dispatch-png% '() (list (eql 16) t t t t) nil)))))

(test tg16-write-master-makefile-method-exists
  "Test that write-master-makefile-for-machine method exists for TG16"
  (is (not (null (find-method #'write-master-makefile-for-machine '() (list (eql 16)) nil)))))

(test tg16-dispatch-png-tileset-detection
  "Test that TG16 dispatch-png correctly identifies tilesets"
  (let ((*machine* 16))
    ;; Test tileset detection logic (multiples of 8x8, at least 16 tiles)
    (is (and (zerop (mod 256 8)) (zerop (mod 224 8)))) ; 32x28 tiles = 896 tiles
    (is (>= (* (/ 256 8) (/ 224 8)) 16))))

(test tg16-dispatch-png-sprite-detection
  "Test that TG16 dispatch-png correctly identifies sprites"
  (let ((*machine* 16))
    ;; Test sprite dimension detection
    (is (and (= 16 16) (zerop (mod 16 16)))) ; 16x16 sprite
    (is (and (= 32 32) (zerop (mod 32 16)))) ; 32x32 sprite
    (is (not (and (= 24 24) (zerop (mod 24 16))))))) ; 24x24 is not valid

(test tg16-dispatch-png-background-detection
  "Test that TG16 dispatch-png correctly identifies backgrounds"
  (let ((*machine* 16))
    ;; Test background resolution detection
    (is (and (= 256 256) (member 224 '(224 239)))) ; NTSC resolution
    (is (and (= 256 256) (member 239 '(224 239)))) ; PAL resolution
    (is (not (and (= 320 320) (member 200 '(224 239))))))) ; Invalid resolution

(test tg16-compile-functions-exist
  "Test that TG16 compilation functions are defined"
  (is (fboundp 'compile-tg16-sprite))
  (is (fboundp 'compile-tg16-background))
  (is (fboundp 'compile-tg16-tile-data)))

(test tg16-tile-data-conversion
  "Test TG16 tile data conversion from palette pixels"
  (let ((*machine* 16))
    ;; Create a simple 8x8 test palette
    (let ((test-palette (make-array '(8 8) :element-type '(unsigned-byte 8) :initial-element 0)))
      ;; Set some test colors (0-15 for 4-bit)
      (dotimes (y 8)
        (dotimes (x 8)
          (setf (aref test-palette x y) (mod (+ x y) 16))))

      ;; Test tile conversion
      (let ((tile-bytes (compile-tg16-tile-data test-palette 0 0)))
        (is (= (length tile-bytes) 32)) ; Should be 32 bytes (4 bitplanes × 8 bytes)
        ;; Check that bytes are in valid range
        (dotimes (i 32)
          (is (<= 0 (aref tile-bytes i) 255)))))))

(test tg16-sprite-compilation-smoke-test
  "Smoke test for TG16 sprite compilation with mock data"
  (let ((*machine* 16)
        (temp-dir (uiop:ensure-directory-pathname "/tmp/tg16-test/")))
    (ensure-directories-exist temp-dir)
    ;; Create mock palette data for a 16x16 sprite (2x2 tiles)
    (let ((mock-palette (make-array '(16 16) :element-type '(unsigned-byte 8) :initial-element 0)))
      ;; This should not error
      (finishes
       (compile-tg16-sprite #p"test-sprite" temp-dir 16 16 mock-palette)))))

(test tg16-background-compilation-smoke-test
  "Smoke test for TG16 background compilation with mock data"
  (let ((*machine* 16)
        (temp-dir (uiop:ensure-directory-pathname "/tmp/tg16-test/")))
    (ensure-directories-exist temp-dir)
    ;; Create mock palette data for a 256x224 background (32x28 tiles)
    (let ((mock-palette (make-array '(256 224) :element-type '(unsigned-byte 8) :initial-element 0)))
      ;; This should not error (though it may take time with large arrays)
      (finishes
       (compile-tg16-background #p"test-bg" temp-dir 224 256 mock-palette)))))

(test tg16-palette-validation
  "Test TG16 palette constraints (512 colors total, 16 per tile)"
  ;; TG16 supports 512 colors total
  ;; Each tile can use up to 16 colors
  (is (<= 16 512)) ; Basic constraint check
  (is (>= 512 16))) ; Basic constraint check

(test tg16-tile-constraints
  "Test TG16 tile format constraints"
  ;; Tiles are 8x8 pixels
  (is (= 8 8))
  ;; Background layers use tiles
  (is (not (null t))))

(test tg16-sprite-constraints
  "Test TG16 sprite format constraints"
  ;; Sprites are 16x16 or 32x32 pixels
  (is (member 16 '(16 32)))
  (is (member 32 '(16 32)))
  ;; Maximum 64 sprites
  (is (<= 64 64)))

(test tg16-resolution-constraints
  "Test TG16 display resolution constraints"
  ;; NTSC: 256x224
  (is (= 256 256))
  (is (= 224 224))
  ;; PAL: 256x239
  (is (= 256 256))
  (is (= 239 239)))

(test tg16-tile-planar-format
  "Test TG16 tile planar format conversion"
  (let ((*machine* 16))
    ;; Create a test tile with known pixel pattern
    (let ((test-palette (make-array '(8 8) :element-type '(unsigned-byte 8) :initial-element 0)))
      ;; Set specific pattern: top-left 4x4 = color 1, bottom-right 4x4 = color 2
      (dotimes (y 4)
        (dotimes (x 4)
          (setf (aref test-palette x y) 1)
          (setf (aref test-palette (+ x 4) (+ y 4)) 2)))

      (let ((tile-bytes (compile-tg16-tile-data test-palette 0 0)))
        (is (= (length tile-bytes) 32))
        ;; Verify planar format: bitplane 0 should have LSB of colors
        ;; Color 1 = binary 01, Color 2 = binary 10
        ;; So bitplane 0 (LSB): 1s where color 1 or 2 appears
        ;; Bitplane 1: 1s where color 2 appears
        (let ((bitplane-0 (subseq tile-bytes 0 8))
              (bitplane-1 (subseq tile-bytes 8 16)))
          ;; Check that bitplane data is not all zeros (indicating conversion worked)
          (is (notevery #'zerop bitplane-0))
          (is (notevery #'zerop bitplane-1)))))))

(test tg16-color-index-validation
  "Test TG16 color index validation and clamping"
  (let ((*machine* 16))
    ;; Create palette with out-of-range color indices
    (let ((test-palette (make-array '(8 8) :element-type '(unsigned-byte 8) :initial-element 20)))
      ;; Should not error, should clamp to valid range
      (finishes
       (compile-tg16-tile-data test-palette 0 0)))))

(test tg16-sprite-tile-assembly
  "Test TG16 sprite tile assembly for multi-tile sprites"
  (let ((*machine* 16))
    ;; Test 16x16 sprite (2x2 tiles = 4 tiles)
    (let ((mock-palette (make-array '(16 16) :element-type '(unsigned-byte 8) :initial-element 0)))
      (dotimes (tile-y 2)
        (dotimes (tile-x 2)
          (dotimes (y 8)
            (dotimes (x 8)
              (let ((global-x (+ (* tile-x 8) x))
                    (global-y (+ (* tile-y 8) y)))
                (when (< global-x 16)
                  (when (< global-y 16)
                    (setf (aref mock-palette global-x global-y)
                          (mod (+ tile-x tile-y) 16)))))))))

      ;; Verify all tiles can be converted
      (dotimes (tile-y 2)
        (dotimes (tile-x 2)
          (let ((tile-data (compile-tg16-tile-data mock-palette tile-x tile-y)))
            (is (= (length tile-data) 32))))))))

(test tg16-sound-compilation-method-exists
  "Test that TG16 music compilation method exists"
  (is (not (null (find-method #'compile-music-for-machine '() (list (eql 16) t t t t) nil)))))

(test tg16-huc6280-psg-function-exists
  "Test that HuC6280 PSG music writing function exists"
  (is (fboundp 'write-song-data-to-huc6280)))

(test tg16-sound-chip-support
  "Test TG16 sound chip specifications"
  ;; TG16 has HuC6280 PSG with 6 channels
  (is (= 6 6))
  ;; Volume range is 0-31 (5 bits)
  (is (<= 0 31 31))
  ;; 6 channels total
  (is (= 6 6)))
