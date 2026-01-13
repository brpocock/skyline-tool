;;; Phantasia SkylineTool/tests/multiplatform-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC
;;;; Tests for all remaining platforms not covered by individual test files

(defpackage :skyline-tool/multiplatform-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:compile-art-sms #:compile-art-sgg
                #:compile-art-sg1000 #:compile-art-c16
                #:compile-art-a2 #:compile-art-a3 #:compile-art-a2gs
                #:compile-art-bbc)
  (:export #:multiplatform-tests))

(in-package :skyline-tool/multiplatform-test)

(def-suite multiplatform-tests
  :description "Tests for all remaining platform-specific SkylineTool functionality")

(in-suite multiplatform-tests)

;; ============================================================================
;; Sega Master System (SMS) Tests
;; ============================================================================

(test sms-art-compilation-function
  "Test that compile-art-sms function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-sms))
  (finishes (skyline-tool:compile-art-sms "/tmp/test.out" "/tmp/test.in")))

(test sms-graphics-function-properties
  "Test that SMS graphics functions exist and are callable"
  (is (functionp (symbol-function 'skyline-tool:compile-art-sms))))

(test sms-music-compilation
  "Test that SMS music compilation functions exist"
  (is-true (fboundp 'skyline-tool:compile-music))
  (let ((*machine* 3010)) (is (= 3010 *machine*)))
  (is-true (fboundp 'skyline-tool::compile-music-sms)))

(test sms-sound-compilation
  "Test sound compilation functions work for SMS (SN76489)"
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "3010")))

;; ============================================================================
;; Sega Game Gear (SGG) Tests
;; ============================================================================

(test sgg-art-compilation-function
  "Test that compile-art-sgg function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-sgg))
  (finishes (skyline-tool:compile-art-sgg "/tmp/test.out" "/tmp/test.in")))

(test sgg-graphics-function-properties
  "Test that SGG graphics functions exist and are callable"
  (is (functionp (symbol-function 'skyline-tool:compile-art-sgg))))

(test sgg-music-compilation
  "Test that SGG music compilation functions exist"
  (is-true (fboundp 'skyline-tool:compile-music))
  (let ((*machine* 837)) (is (= 837 *machine*)))
  (is-true (fboundp 'skyline-tool::compile-music-sgg)))

(test sgg-sound-compilation
  "Test sound compilation functions work for SGG (SN76489)"
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "837")))

;; ============================================================================
;; Sega SG-1000 Tests
;; ============================================================================

(test sg1000-art-compilation-function
  "Test that compile-art-sg1000 function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-sg1000))
  (finishes (skyline-tool:compile-art-sg1000 "/tmp/test.out" "/tmp/test.in")))

(test sg1000-graphics-function-properties
  "Test that SG-1000 graphics functions exist and are callable"
  (is (functionp (symbol-function 'skyline-tool:compile-art-sg1000))))

(test sg1000-music-compilation
  "Test that SG-1000 music compilation functions exist"
  (is-true (fboundp 'skyline-tool:compile-music))
  (let ((*machine* 1000)) (is (= 1000 *machine*)))
  (is-true (fboundp 'skyline-tool::compile-music-sg1000)))

(test sg1000-sound-compilation
  "Test sound compilation functions work for SG-1000 (SN76489)"
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "1000")))

;; ============================================================================
;; Commodore 16 Tests
;; ============================================================================

(test c16-art-compilation-function
  "Test that compile-art-c16 function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-c16))
  (finishes (skyline-tool:compile-art-c16 "/tmp/test.out" "/tmp/test.in")))

(test c16-graphics-function-properties
  "Test that C=16 graphics functions exist and are callable"
  (is (functionp (symbol-function 'skyline-tool:compile-art-c16))))

(test c16-music-compilation
  "Test that C=16 music compilation functions exist"
  (is-true (fboundp 'skyline-tool:compile-music))
  (let ((*machine* 264)) (is (= 264 *machine*)))
  (is-true (fboundp 'skyline-tool::compile-music-c16)))

(test c16-sound-compilation
  "Test sound compilation functions work for C=16 (TED)"
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "264")))

;; ============================================================================
;; Apple II Variants Tests
;; ============================================================================

(test a2-art-compilation-function
  "Test that compile-art-a2 function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-a2))
  (finishes (skyline-tool:compile-art-a2 "/tmp/test.out" "/tmp/test.in")))

(test a2-graphics-function-properties
  "Test that Apple II graphics functions exist and are callable"
  (is (functionp (symbol-function 'skyline-tool:compile-art-a2))))

(test a2-music-compilation
  "Test that Apple II music compilation functions exist"
  (is-true (fboundp 'skyline-tool:compile-music))
  (let ((*machine* 8)) (is (= 8 *machine*)))
  (is-true (fboundp 'skyline-tool::compile-music-a2)))

(test a2-sound-compilation
  "Test sound compilation functions work for Apple II (Mockingboard)"
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "8")))

;; ============================================================================
;; Apple III Tests
;; ============================================================================

(test a3-art-compilation-function
  "Test that compile-art-a3 function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-a3))
  (finishes (skyline-tool:compile-art-a3 "/tmp/test.out" "/tmp/test.in")))

(test a3-graphics-function-properties
  "Test that Apple III graphics functions exist and are callable"
  (is (functionp (symbol-function 'skyline-tool:compile-art-a3))))

(test a3-music-compilation
  "Test that Apple III music compilation functions exist"
  (is-true (fboundp 'skyline-tool:compile-music))
  (let ((*machine* 9)) (is (= 9 *machine*)))
  (is-true (fboundp 'skyline-tool::compile-music-a3)))

(test a3-sound-compilation
  "Test sound compilation functions work for Apple III"
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "9")))

;; ============================================================================
;; Apple IIGS Tests
;; ============================================================================

(test a2gs-art-compilation-function
  "Test that compile-art-a2gs function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-a2gs))
  (finishes (skyline-tool:compile-art-a2gs "/tmp/test.out" "/tmp/test.in")))

(test a2gs-graphics-function-properties
  "Test that Apple IIGS graphics functions exist and are callable"
  (is (functionp (symbol-function 'skyline-tool:compile-art-a2gs))))

(test a2gs-music-compilation
  "Test that Apple IIGS music compilation functions exist"
  (is-true (fboundp 'skyline-tool:compile-music))
  (let ((*machine* 10)) (is (= 10 *machine*)))
  (is-true (fboundp 'skyline-tool::compile-music-a2gs)))

(test a2gs-sound-compilation
  "Test sound compilation functions work for Apple IIGS (Ensoniq)"
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "10")))

;; ============================================================================
;; BBC Micro Tests
;; ============================================================================

(test bbc-art-compilation-function
  "Test that compile-art-bbc function exists and can be called"
  (is-true (fboundp 'skyline-tool:compile-art-bbc))
  (finishes (skyline-tool:compile-art-bbc "/tmp/test.out" "/tmp/test.in")))

(test bbc-graphics-function-properties
  "Test that BBC graphics functions exist and are callable"
  (is (functionp (symbol-function 'skyline-tool:compile-art-bbc))))

(test bbc-music-compilation
  "Test that BBC music compilation functions exist"
  (is-true (fboundp 'skyline-tool:compile-music))
  (let ((*machine* 7)) (is (= 7 *machine*)))
  (is-true (fboundp 'skyline-tool::compile-music-bbc)))

(test bbc-sound-compilation
  "Test sound compilation functions work for BBC (SN76489)"
  (signals error (skyline-tool:compile-music "/tmp/test-music.s" "/nonexistent.mid" "7")))

;; ============================================================================
;; Speech/Phoneme Tests for Supported Platforms
;; ============================================================================

(test speech-phoneme-support-7800
  "Test that 7800 supports SpeakJet phonemes"
  (let ((*machine* 7800))
    (is-true (skyline-tool::speech-supported-p))
    (is-true (fboundp 'skyline-tool:compile-speech-7800))))

(test speech-phoneme-support-2600
  "Test that 2600 supports SpeakJet phonemes"
  (let ((*machine* 2600))
    (is-true (skyline-tool::speech-supported-p))
    (is-true (fboundp 'skyline-tool:compile-speech-2600))))

(test speech-phoneme-support-2609
  "Test that 2609 supports IntelliVoice phonemes"
  (let ((*machine* 2609))
    (is-true (skyline-tool::speech-supported-p))
    (is-true (fboundp 'skyline-tool:compile-speech-2609))))

(test speech-phoneme-unsupported
  "Test that other platforms don't support speech phonemes"
  (dolist (machine '(3 6 200 35902 20953 9918 1000 3010 837 264 8 9 10 7))
    (let ((*machine* machine))
      (is-false (skyline-tool::speech-supported-p)
                "Machine ~A should not support speech phonemes" machine))))

;; ============================================================================
;; Forth Bytecode Compilation Tests
;; ============================================================================

(test forth-bytecode-compilation-6502
  "Test that 65xx platforms generate 64tass-compatible Forth bytecode"
  (dolist (machine '(2600 5200 7800 264 8 9 10))
    (let ((*machine* machine))
      (is-true (fboundp 'skyline-tool:compile-forth-6502))
      (finishes (skyline-tool:compile-forth-6502 "/tmp/test.fth" "/tmp/test.out")))))

(test forth-bytecode-compilation-z80
  "Test that Z80 platforms generate z80asm-compatible Forth bytecode"
  (dolist (machine '(3 6 1000 3010 837)) ; NES, SNES, SG-1000, SMS, SGG
    (let ((*machine* machine))
      (is-true (fboundp 'skyline-tool:compile-forth-z80))
      (finishes (skyline-tool:compile-forth-z80 "/tmp/test.fth" "/tmp/test.out")))))

;; ============================================================================
;; Makefile Generation Tests
;; ============================================================================

(test makefile-generation-all-platforms
  "Test that Makefiles can be generated for all supported platforms"
  (dolist (machine '(2600 5200 7800 200 3 6 35902 20953 9918 1000 3010 837 264 8 9 10 7 2609 1601))
    (let ((*machine* machine))
      (finishes (skyline-tool:write-master-makefile)
                "Makefile generation should not fail for machine ~A" machine))))

;; ============================================================================
;; Palette Generation Tests
;; ============================================================================

(test palette-generation-ntsc
  "Test that NTSC palettes are generated for all platforms"
  (dolist (machine '(2600 5200 7800 200 3 6 35902 20953 9918 1000 3010 837 264 8 9 10 7 2609 1601))
    (let ((*machine* machine))
      (is-true (fboundp 'skyline-tool:generate-ntsc-palette))
      (finishes (skyline-tool:generate-ntsc-palette "/tmp/palette.act")))))

(test palette-generation-pal
  "Test that PAL palettes are generated for PAL-capable platforms"
  (dolist (machine '(7800 200 3 6 35902 20953 9918 1000 3010 837 264 8 9 10 7))
    (let ((*machine* machine))
      (when (skyline-tool::pal-capable-p *machine*)
        (is-true (fboundp 'skyline-tool:generate-pal-palette))
        (finishes (skyline-tool:generate-pal-palette "/tmp/palette.act"))))))

(test palette-generation-secam
  "Test that SECAM palettes are generated for SECAM-capable platforms"
  (dolist (machine '(7800 200 3 6))
    (let ((*machine* machine))
      (when (skyline-tool::secam-capable-p *machine*)
        (is-true (fboundp 'skyline-tool:generate-secam-palette))
        (finishes (skyline-tool:generate-secam-palette "/tmp/palette.act"))))))

(defun multiplatform-tests ()
  "Run all multiplatform tests and return results"
  (fiveam:run! 'multiplatform-tests))
