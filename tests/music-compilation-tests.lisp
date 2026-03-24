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
  (let ((output-file (format nil "Object/2600/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
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
  (let ((output-file (format nil "Object/5200/test-music-~a.bin" (skyline-tool::generate-secure-random-id 2)))
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
  (let ((output-file (format nil "Object/2609/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test compilation
        (let ((skyline-tool::*machine* 2609))
          (finishes (skyline-tool::compile-music output-file input-file "2609" "AY-3-8910" "NTSC")))

        ;; Validate output
        (validate-music-output-file output-file 2609
                                  '(";;; Music compiled from"
                                    "Intellivision AY-3-8910 PSG"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

(test intv-ay-3-8910-score-and-midi-compile-path
  "Intellivision PSG: score->song, write-song-binary, and compile-midi AY-3-8910 path"
  (let ((skyline-tool::*machine* 2609)
        (score (list (list :lyric nil :instrument :piano :time 0.0d0 :duration 0.1d0
                           :key 60 :velocity 100))))
    (let ((song (skyline-tool::score->song score :ay-3-8910 :ntsc)))
      (is (arrayp song) "score->song :ay-3-8910 should return an array")
      (is (plusp (array-dimension song 0)) "should have at least one note row"))
    (let ((bin (merge-pathnames (format nil "test-ay-~a.bin" (skyline-tool::generate-secure-random-id 2))
                                (uiop:temporary-directory))))
      (unwind-protect
          (let ((song (skyline-tool::score->song score :ay-3-8910 :ntsc)))
            (finishes (skyline-tool::write-song-binary song :ay-3-8910 bin))
            (is-true (probe-file bin))
            (is-true (> (with-open-file (s bin :element-type '(unsigned-byte 8))
                          (file-length s))
                        3)
                     "AY binary should have header plus ≥1 note (6 bytes)"))
        (when (probe-file bin) (delete-file bin))))))

(test 64-c64-music-compilation-validation
  "Test that Commodore 64 music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/64/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
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
  (let ((output-file (format nil "Object/128/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
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

(test 2-apple2-mockingboard-music-compilation-validation
  "Test that Apple II Mockingboard music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/2/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test Mockingboard compilation
        (finishes (skyline-tool::compile-music output-file input-file "2" "Mockingboard" "NTSC"))

        ;; Validate output
        (validate-music-output-file output-file 2
                                  '(";;; Apple II Mockingboard Music compiled from"
                                    "Apple II Mockingboard (AY-3-8910 PSG)"
                                    "mock_write_register:"
                                    "MOCK_REG_SELECT"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

(test 2-apple2-beeper-music-compilation-validation
  "Test that Apple II beeper music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/2/test-beeper-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test beeper compilation
        (finishes (skyline-tool::compile-music output-file input-file "2" "Beeper" "NTSC"))

        ;; Validate output
        (validate-music-output-file output-file 2
                                  '(";;; Apple II Beeper Music compiled from"
                                    "Apple II built-in speaker (1-bit audio)"
                                    "beeper_init:"
                                    "sta SPEAKER"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

(test 10-apple2gs-music-compilation-validation
  "Test that Apple IIGS music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/10/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
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
  (let ((output-file (format nil "Object/81/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
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
  (let ((output-file (format nil "Object/2068/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
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
  (let ((output-file (format nil "Object/3/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
        (input-file "test-input.mid"))
    (unwind-protect
        ;; Create a minimal test input file
        (with-open-file (out input-file :direction :output :if-exists :supersede)
          (write *test-midi-data* :stream out :readably t))

        ;; Test that NES compilation signals error (currently unimplemented)
        (signals error (skyline-tool::compile-music output-file input-file "3" "APU" "NTSC"))

        ;; Test that SNES compilation signals error (currently unimplemented)
        (signals error (skyline-tool::compile-music output-file input-file "6" "SPC700" "NTSC"))
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
  (let ((output-file (format nil "Object/2600/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
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

(test nes-music-compilation-validation
  "Test that NES music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/8/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
        (input-file "test-input.mid"))
    (unwind-protect
        (progn
           ;; Create a minimal test input file
           (with-open-file (out input-file :direction :output :if-exists :supersede)
             (write *test-midi-data* :stream out :readably t))

           ;; Test compilation
           (finishes (skyline-tool::compile-music-nes output-file input-file))

           ;; Validate output contains expected NES APU code
           (validate-music-output-file output-file 8
                                     '(";;; NES APU Music compiled from"
                                       "APU_PULSE1 = $4000"
                                       "APU_PULSE2 = $4004"
                                       "APU_TRIANGLE = $4008"
                                       "apu_init:"
                                       "note_periods:"
                                       "play_note_pulse1:"
                                       "apu_stop:"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file))))))

(test snes-music-compilation-validation
  "Test that SNES music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/88/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
        (input-file "test-input.mid"))
    (unwind-protect
        (progn
           ;; Create a minimal test input file
           (with-open-file (out input-file :direction :output :if-exists :supersede)
             (write *test-midi-data* :stream out :readably t))

           ;; Test compilation
           (finishes (skyline-tool::compile-music-snes output-file input-file))

           ;; Validate output contains expected SNES DSP code
           (validate-music-output-file output-file 88
                                     '(";;; SNES SPC700 Music compiled from"
                                       "DSP_VOL_L = $00"
                                       "DSP_PITCH_L = $02"
                                       "DSP_KON = $4C"
                                       "dsp_init:"
                                       "note_pitches:"
                                       "play_note:"
                                       "stop_channel:"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file))))))

(test sms-music-compilation-validation
  "Test that SMS music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/3010/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
        (input-file "test-input.mid"))
    (unwind-protect
        (progn
           ;; Create a minimal test input file
           (with-open-file (out input-file :direction :output :if-exists :supersede)
             (write *test-midi-data* :stream out :readably t))

           ;; Test compilation
           (finishes (skyline-tool::compile-music-sms output-file input-file))

           ;; Validate output contains expected SMS PSG code
           (validate-music-output-file output-file 3010
                                     '(";;; SN76489 PSG Music compiled from"
                                       "PSG_PORT = $7F"
                                       "psg_init:"
                                       "note_freqs:"
                                       "psg_write:"
                                       "psg_stop:"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file))))))

(test colecovision-music-compilation-validation
  "Test that ColecoVision music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/9918/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
        (input-file "test-input.mid"))
    (unwind-protect
        (progn
           (ensure-directories-exist output-file)
           ;; Create a minimal test input file
           (with-open-file (out input-file :direction :output :if-exists :supersede)
             (write *test-midi-data* :stream out :readably t))

           ;; Test compilation
           (finishes (skyline-tool::compile-music-colecovision output-file input-file))

           ;; Validate output contains expected ColecoVision PSG code
           (validate-music-output-file output-file 9918
                                     '(";;; ColecoVision SN76489 PSG Music compiled from"
                                       "PSG_PORT = $FF"
                                       "psg_init:"
                                       "note_freqs:"
                                       "play_tone:"
                                       "psg_stop:"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file))))))

(test sg1000-music-compilation-validation
  "Test that SG-1000 music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/1000/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
        (input-file "test-input.mid"))
    (unwind-protect
        (progn
           ;; Create a minimal test input file
           (with-open-file (out input-file :direction :output :if-exists :supersede)
             (write *test-midi-data* :stream out :readably t))

           ;; Test compilation
           (finishes (skyline-tool::compile-music-sg1000 output-file input-file))

           ;; Validate output contains expected SG-1000 PSG code
           (validate-music-output-file output-file 1000
                                     '(";;; SG-1000 SN76489 PSG Music compiled from"
                                       "PSG_PORT = $7F"
                                       "psg_init:"
                                       "note_freqs:"
                                       "play_tone:"
                                       "psg_stop:"))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

(test 200-lynx-music-compilation-validation
  "Test that Atari Lynx music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/200/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
        (input-file "test-input.mid"))
    (unwind-protect
        (progn
           ;; Create a minimal test input file
           (with-open-file (out input-file :direction :output :if-exists :supersede)
             (write *test-midi-data* :stream out :readably t))

           ;; Test compilation
           (finishes (skyline-tool::compile-music-lynx output-file input-file))

           ;; Validate output contains expected Lynx audio code
           (validate-music-output-file output-file 200
                                     '(";;; Atari Lynx Music compiled from"
                                       "AUD0_VOL"
                                       "AUD0_FEEDBACK"
                                       "AUD0_OUTPUT"
                                       "AUD0_SHIFT"
                                       "AUD0_BACKUP"
                                       "AUD0_CONTROL"
                                       "AUD0_COUNT"
                                       "AUD0_OTHER")))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file)))))

(test 264-c16-music-compilation-validation
  "Test that Commodore 16/Plus4 music compilation produces correct assembly output"
  (let ((output-file (format nil "Object/264/test-music-~a.s" (skyline-tool::generate-secure-random-id 2)))
        (input-file "test-input.mid"))
    (unwind-protect
        (progn
           ;; Create a minimal test input file
           (with-open-file (out input-file :direction :output :if-exists :supersede)
             (write *test-midi-data* :stream out :readably t))

           ;; Test compilation
           (finishes (skyline-tool::compile-music-c16 output-file input-file))

           ;; Validate output contains expected TED audio code
           (validate-music-output-file output-file 264
                                     '(";;; Commodore 16 Music compiled from"
                                       "TED_SOUND"
                                       "TED_VOLUME"
                                       "TED_ADSR"
                                       "TED_WAVEFORM")))
      ;; Cleanup
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file input-file))))))
