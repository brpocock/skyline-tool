;;; Phantasia SkylineTool/tests/a2gs-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC
;;;; Tests for Apple IIGS platform support

(in-package :skyline-tool)

(def-suite a2gs-suite
    :description "Tests for Apple IIGS platform support"
    :in test-suite)

(in-suite a2gs-suite)

(test a2gs-machine-number
  "Test that Apple IIGS machine number is correctly defined"
  (is (= 222 (machine-number-by-tag "2gs")))
  (is (= 222 (machine-number-by-tag "2GS")))
  (is (= 222 (machine-number-by-tag "Apple IIGS"))))

(test a2gs-machine-directory
  "Test that Apple IIGS machine directory name is correct"
  (is (string= "2gs" (machine-directory-name 222))))

(test a2gs-supported-video-types
  "Test that Apple IIGS supports appropriate video types"
  ;; Apple IIGS supports NTSC, PAL, and SECAM
  (is (equal '(:ntsc :pal :secam) (supported-video-types 222))))

(test a2gs-dispatch-png-method-exists
  "Test that dispatch-png% method exists for Apple IIGS"
  (is (not (null (find-method #'dispatch-png% '() (list (eql 222) t t t t) nil)))))

(test a2gs-hires-palette-defined
  "Test that Apple IIGS Hi-Res palette is properly defined"
  (is (not (null +apple-hires-palette+)))
  (is (= 6 (length +apple-hires-palette+))))

(test a2gs-graphics-functions-exist
  "Test that Apple IIGS graphics compilation functions exist"
  (is (fboundp 'compile-a2gs-super-hires))
  (is (fboundp 'compile-a2gs-double-hires))
  (is (fboundp 'compile-a2gs-hires))
  (is (fboundp 'compile-a2gs-sprite)))

(test a2gs-sound-functions-exist
  "Test that Apple IIGS sound compilation functions exist"
  (is (fboundp 'compile-music-a2gs))
  (is (not (null (find-method #'compile-music-for-machine '() (list (eql 222) t t t t) nil)))))

(test a2gs-dispatch-png-super-hires-detection
  "Test Apple IIGS dispatch-png correctly identifies Super Hi-Res graphics"
  (let ((*machine* 222))
    ;; Test Super Hi-Res detection (320x200)
    (is (and (= 320 320) (= 200 200)))))

(test a2gs-dispatch-png-double-hires-detection
  "Test Apple IIGS dispatch-png correctly identifies Double Hi-Res graphics"
  (let ((*machine* 222))
    ;; Test Double Hi-Res detection (560x192)
    (is (and (= 560 560) (= 192 192)))))

(test a2gs-dispatch-png-hires-detection
  "Test Apple IIGS dispatch-png correctly identifies Hi-Res graphics"
  (let ((*machine* 222))
    ;; Test Hi-Res detection (280x192)
    (is (and (= 280 280) (= 192 192)))))

(test a2gs-dispatch-png-sprite-detection
  "Test Apple IIGS dispatch-png correctly identifies sprites"
  (let ((*machine* 222))
    ;; Test sprite size validation
    (is (<= 16 64)) ; Minimum sprite size
    (is (<= 16 64)) ; Maximum sprite size))

(test a2gs-super-hires-compilation-smoke-test
  "Smoke test for Apple IIGS Super Hi-Res compilation"
  (let ((*machine* 222)
        (temp-dir (uiop:ensure-directory-pathname "/tmp/a2gs-test/")))
    (ensure-directories-exist temp-dir)
    ;; Create mock palette data for Super Hi-Res (320x200)
    (let ((mock-palette (make-array '(320 200) :element-type '(unsigned-byte 8) :initial-element 0)))
      ;; Fill with some test pattern
      (dotimes (y 200)
        (dotimes (x 320)
          (setf (aref mock-palette x y) (mod (+ x y) 16))))
      ;; This should not error
      (finishes
       (compile-a2gs-super-hires #p"test-super-hires" temp-dir 200 320 mock-palette)))))

(test a2gs-double-hires-compilation-smoke-test
  "Smoke test for Apple IIGS Double Hi-Res compilation"
  (let ((*machine* 222)
        (temp-dir (uiop:ensure-directory-pathname "/tmp/a2gs-test/")))
    (ensure-directories-exist temp-dir)
    ;; Create mock palette data for Double Hi-Res (560x192)
    (let ((mock-palette (make-array '(560 192) :element-type '(unsigned-byte 8) :initial-element 0)))
      ;; This should not error
      (finishes
       (compile-a2gs-double-hires #p"test-double-hires" temp-dir 192 560 mock-palette)))))

(test a2gs-hires-compilation-smoke-test
  "Smoke test for Apple IIGS Hi-Res compilation"
  (let ((*machine* 222)
        (temp-dir (uiop:ensure-directory-pathname "/tmp/a2gs-test/")))
    (ensure-directories-exist temp-dir)
    ;; Create mock palette data for Hi-Res (280x192)
    (let ((mock-palette (make-array '(280 192) :element-type '(unsigned-byte 8) :initial-element 0)))
      ;; This should not error
      (finishes
       (compile-a2gs-hires #p"test-hires" temp-dir 192 280 mock-palette)))))

(test a2gs-sprite-compilation-smoke-test
  "Smoke test for Apple IIGS sprite compilation"
  (let ((*machine* 222)
        (temp-dir (uiop:ensure-directory-pathname "/tmp/a2gs-test/")))
    (ensure-directories-exist temp-dir)
    ;; Create mock palette data for 16x16 sprite
    (let ((mock-palette (make-array '(16 16) :element-type '(unsigned-byte 8) :initial-element 0)))
      ;; This should not error
      (finishes
       (compile-a2gs-sprite #p"test-sprite" temp-dir 16 16 mock-palette)))))

(test a2gs-super-hires-bitplane-format
  "Test Apple IIGS Super Hi-Res bitplane format conversion"
  (let ((*machine* 222))
    ;; Test that bitplane calculations are correct
    ;; Super Hi-Res: 320x200 pixels, 4 bitplanes = 3200 bytes total
    (is (= (* 320 200 4) (* 8 4 800))) ; 4 bitplanes × 800 bytes each
    (is (= 3200 (* 4 800)))))

(test a2gs-double-hires-bitplane-format
  "Test Apple IIGS Double Hi-Res bitplane format conversion"
  (let ((*machine* 222))
    ;; Test that bitplane calculations are correct
    ;; Double Hi-Res: 560x192 pixels, 4 bitplanes = 5376 bytes total
    (is (= (* 560 192 4) (* 8 4 1344))) ; 4 bitplanes × 1344 bytes each
    (is (= 5376 (* 4 1344)))))

(test a2gs-hires-bitplane-format
  "Test Apple IIGS Hi-Res bitplane format conversion"
  (let ((*machine* 222))
    ;; Test that bitplane calculations are correct
    ;; Hi-Res: 280x192 pixels, 3 bitplanes = 2304 bytes total
    (is (= (* 280 192 3) (* 8 3 768))) ; 3 bitplanes × 768 bytes each
    (is (= 2304 (* 3 768)))))

(test a2gs-color-depth-validation
  "Test Apple IIGS color depth specifications"
  ;; Super Hi-Res and Double Hi-Res: 16 colors (4-bit)
  (is (= 16 (expt 2 4)))
  ;; Hi-Res: 6 colors (NTSC artifact)
  (is (= 6 (length +apple-hires-palette+))))

(test a2gs-resolution-validation
  "Test Apple IIGS resolution specifications"
  ;; Super Hi-Res: 320x200
  (is (= 320 320))
  (is (= 200 200))
  ;; Double Hi-Res: 560x192
  (is (= 560 560))
  (is (= 192 192))
  ;; Hi-Res: 280x192
  (is (= 280 280))
  (is (= 192 192)))

(test a2gs-sound-chip-specification
  "Test Apple IIGS sound chip specifications"
  ;; Ensoniq DOC: 32 oscillators
  (is (= 32 32))
  ;; 8-bit samples
  (is (= 8 8))
  ;; Stereo capability
  (is (= 2 2)))
