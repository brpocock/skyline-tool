;;; Phantasia SkylineTool/tests/music-compilation-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite music-compilation-tests
  :description "Tests for music compilation across all supported platforms"
  :in skyline-tool/test)

(in-suite music-compilation-tests)

;; Define test data - a minimal MIDI-like structure for testing
(defparameter *test-midi-data*
  '((:note-on :channel 0 :key 60 :velocity 100 :time 0)
    (:note-off :channel 0 :key 60 :velocity 0 :time 100))
  "Minimal test MIDI data for compilation testing")

;; Test helper function to validate output files
(defun validate-music-output-file (filename machine-type expected-content-patterns)
  "Validate that a music output file contains expected content patterns"
  (is-true (probe-file filename)
           (format nil "~a output file should be created for machine ~a" filename machine-type))
  (when (probe-file filename)
    (with-open-file (stream filename :direction :input :if-does-not-exist nil)
      (when stream
        (let ((content (alexandria:read-stream-content-into-string stream)))
          (dolist (pattern expected-content-patterns)
            (is-true (search pattern content)
                     (format nil "~a output should contain pattern '~a' for machine ~a"
                             filename pattern machine-type))))))))

;; Test each supported machine's music compilation capabilities

(test 2600-music-compilation-validation
  "Test that Atari 2600 music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/2600/test-music-~a.s" (random 1000)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test compilation
        (finishes (skyline-tool::compile-music output-file input-file "2600" "TIA" "NTSC"))

        ;; Validate output
        (validate-music-output-file output-file 2600
                                  '(";;; Music compiled from"
                                    ".if TV == NTSC"
                                    ".else"
                                    ".fi"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

(test 5200-music-compilation-validation
  "Test that Atari 5200 music compilation produces correct binary output"
  (let ((output-file (format nil "Object/5200/test-music-~a.bin" (random 1000)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test compilation
        (finishes (skyline-tool::compile-music output-file input-file "5200" "POKEY" "NTSC"))

        ;; Validate output file exists and has reasonable size
        (is-true (probe-file output-file)
                 "5200 music compilation should create output file")
        (when (probe-file output-file)
        (is-true (> (with-open-file (s output-file :element-type '(unsigned-byte 8))
                       (file-length s)) 0)
                 "5200 music output file should have non-zero size"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

(test 7800-music-compilation-validation
  "Test that Atari 7800 music compilation functions work correctly"
  ;; Test that the 7800 music compilation functions are available and callable
  (is-true (fboundp 'skyline-tool::compile-music-for-machine)
           "compile-music-for-machine should be available")

  ;; Test that 7800 music processing functions work
  (let ((mock-track '((:text . "Piano") (:note :time 0 :key 60 :duration 100))))
    (let ((result (skyline-tool::midi->7800-tia (list mock-track) :ntsc)))
      (is (arrayp result) "midi->7800-tia should return an array for 7800")
      (is (= (length result) 2) "7800 TIA should have 2 voices")))

  ;; Test array conversion function
  (let ((result (skyline-tool::array<-7800-tia-notes-list '((60 100 480)) :ntsc)))
    (is (vectorp result) "array<-7800-tia-notes-list should return a vector")))

(test 2609-music-compilation-validation
  "Test that Intellivision music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/2609/test-music-~a.s" (random 1000)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test compilation
        (finishes (skyline-tool::compile-music output-file input-file "2609" "AY-3-8910" "NTSC"))

        ;; Validate output
        (validate-music-output-file output-file 2609
                                  '(";;; Music compiled from"
                                    "Intellivision AY-3-8910 PSG"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

(test 64-c64-music-compilation-validation
  "Test that Commodore 64 music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/64/test-music-~a.s" (random 1000)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test compilation
        (finishes (skyline-tool::compile-music output-file input-file "64" "SID" "NTSC"))

        ;; Validate output
        (validate-music-output-file output-file 64
                                  '(";;; SID Music compiled from"
                                    "Commodore 64 SID synthesizer"
                                    "sid_init:"
                                    "sta $d404"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

(test 128-c128-music-compilation-validation
  "Test that Commodore 128 music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/128/test-music-~a.s" (random 1000)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test compilation
        (finishes (skyline-tool::compile-music output-file input-file "128" "SID" "NTSC"))

        ;; Validate output (should be same as C64)
        (validate-music-output-file output-file 128
                                  '(";;; SID Music compiled from"
                                    "Commodore 128 SID synthesizer"
                                    "sid_init:"
                                    "sta $d404"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

(test 8-apple2-music-compilation-validation
  "Test that Apple II music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/8/test-music-~a.s" (random 1000)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test compilation
        (finishes (skyline-tool::compile-music output-file input-file "8" "Mockingboard" "NTSC"))

        ;; Validate output
        (validate-music-output-file output-file 8
                                  '(";;; Mockingboard Music compiled from"
                                    "Apple ][ Mockingboard"
                                    "mock_init:"
                                    "sta AY_REG"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

(test 10-apple2gs-music-compilation-validation
  "Test that Apple IIGS music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/10/test-music-~a.s" (random 1000)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test compilation
        (finishes (skyline-tool::compile-music output-file input-file "10" "Ensoniq" "NTSC"))

        ;; Validate output
        (validate-music-output-file output-file 10
                                  '(";;; Apple IIGS Enhanced Sound compiled from"
                                    "Apple IIGS has Ensoniq DOC"
                                    "sound_init:"
                                    "sta SOUNDCTL"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

(test 81-zx81-music-compilation-validation
  "Test that ZX81 music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/81/test-music-~a.s" (random 1000)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test compilation
        (finishes (skyline-tool::compile-music output-file input-file "81" "EAR" "NTSC"))

        ;; Validate output
        (validate-music-output-file output-file 81
                                  '(";;; ZX81 EAR Music compiled from"
                                    "ZX81 cassette EAR interface"
                                    "ear_init:"
                                    "out ($fe), a"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

(test 2068-spectrum-music-compilation-validation
  "Test that ZX Spectrum music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/2068/test-music-~a.s" (random 1000)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test compilation
        (finishes (skyline-tool::compile-music output-file input-file "2068" "BEEPER" "NTSC"))

        ;; Validate output
        (validate-music-output-file output-file 2068
                                  '(";;; ZX Spectrum Beeper Music compiled from"
                                    "ZX Spectrum internal speaker"
                                    "beeper_init:"
                                    "out ($fe), a"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

;; Test that unimplemented platforms properly signal errors

(test unimplemented-platform-error-signaling
  "Test that unimplemented platforms signal appropriate errors"
  (let ((output-file (format nil "Object/3/test-music-~a.s" (random 1000)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test that NES compilation signals error (currently unimplemented)
        (signals error (skyline-tool::compile-music output-file input-file "3" "APU" "NTSC"))

        ;; Test that SNES compilation signals error (currently unimplemented)
        (signals error (skyline-tool::compile-music output-file input-file "6" "SPC700" "NTSC"))

        ;; Test that ColecoVision compilation signals error (currently unimplemented)
        (signals error (skyline-tool::compile-music output-file input-file "9918" "SN76489" "NTSC"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

;; Test MIDI utility functions

(test midi-utility-functions
  "Test MIDI utility functions work correctly"
  ;; Test note->midi-note-number function
  (is (= 60 (skyline-tool::note->midi-note-number "C5"))
      "C5 should convert to MIDI note 60")
  (is (= 69 (skyline-tool::note->midi-note-number "A4"))
      "A4 should convert to MIDI note 69 (concert A)")
  (is (= 61 (skyline-tool::note->midi-note-number "C♯5"))
      "C♯5 should convert to MIDI note 61")
  (is (= 62 (skyline-tool::note->midi-note-number "D5"))
      "D5 should convert to MIDI note 62")

  ;; Test midi->note-name function
  (is (string= "C5" (skyline-tool::midi->note-name 60))
      "MIDI note 60 should convert to C5")
  (is (string= "A4" (skyline-tool::midi->note-name 69))
      "MIDI note 69 should convert to A4"))

;; Test platform-specific parameter validation

(test platform-parameter-validation
  "Test that platforms receive correct parameters and validate inputs"
  (let ((output-file (format nil "Object/2600/test-music-~a.s" (random 1000)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test that 2600 ignores sound-chip parameter (should still work)
        (finishes (skyline-tool::compile-music output-file input-file "2600" "INVALID" "INVALID"))
        (is-true (probe-file output-file)
                 "2600 compilation should ignore invalid sound-chip/output-coding parameters")

        ;; Test that binary platforms (5200, 7800) require valid output-coding
        (finishes (skyline-tool::compile-music output-file input-file "5200" "POKEY" "NTSC"))
        (is-true (probe-file output-file)
                 "5200 compilation should accept valid NTSC output-coding")
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))
