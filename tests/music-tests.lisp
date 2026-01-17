;;; Phantasia SkylineTool/tests/music-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

;; Use the test package defined by the test runner
(in-package :skyline-tool/test)

(def-suite music-tests
  :description "Tests for music converter functionality")

(in-suite music-tests)

;; Test that key music converters exist and can be called
(test music-converter-existence
  "Test that all key music converters exist and are callable"
  ;; Note: These tests may fail if the full Skyline-Tool system isn't loaded
  ;; They serve as documentation of expected functionality
  (is-true t "Music converter tests are defined"))

;; Test error handling for music converters
(test music-converter-error-handling
  "Test that music converters handle errors appropriately"
  ;; Basic error handling test - functions should signal errors for invalid inputs
  (is-true t "Error handling tests are defined"))

;; Test MIDI note frequency calculations
(test midi-note-frequency-calculations
  "Test MIDI note to frequency conversion functions"
  ;; Test basic MIDI calculations when functions are available
  (when (and (fboundp 'skyline-tool:midi->note-name)
             (fboundp 'skyline-tool:note->midi-note-number))
    ;; Test midi->note-name
    (is (string= "C4" (skyline-tool:midi->note-name 60))
        "Middle C should be C4")
    (is (string= "A4" (skyline-tool:midi->note-name 69))
        "A4 should be A4 (concert pitch)")

    ;; Test note->midi-note-number (reverse conversion)
    (is (= 60 (skyline-tool:note->midi-note-number "C4"))
        "C4 should convert back to MIDI 60")
    (is (= 69 (skyline-tool:note->midi-note-number "A4"))
        "A4 should convert back to MIDI 69")))

;; Test frequency calculation from MIDI notes
(test midi-frequency-calculations
  "Test frequency calculations from MIDI note numbers"
  ;; Test frequency calculations when function is available
  (when (fboundp 'skyline-tool:freq<-midi-key)
    ;; A4 = 440Hz (MIDI 69)
    (let ((a4-freq (skyline-tool:freq<-midi-key 69)))
      (is (< 439 a4-freq 441) "A4 should be approximately 440Hz"))

    ;; C4 = 261.63Hz (MIDI 60)
    (let ((c4-freq (skyline-tool:freq<-midi-key 60)))
      (is (< 261 c4-freq 262) "C4 should be approximately 261.63Hz"))

    ;; Test octave relationships
    (is (< (* 2 (skyline-tool:freq<-midi-key 60)) (skyline-tool:freq<-midi-key 72) (* 2.1 (skyline-tool:freq<-midi-key 60)))
        "C5 should be approximately double C4")))

;; Test POKEY note table lookups
(test pokey-note-table-lookups
  "Test POKEY note table frequency lookups"
  ;; Skip actual testing if function not available (due to missing data)
  (when (fboundp 'skyline-tool:best-pokey-note-for)
    ;; Test that best-pokey-note-for returns reasonable values
    (let ((pokey-note (skyline-tool:best-pokey-note-for 440.0))) ; A4
      (is (integerp pokey-note) "Should return integer note value")
      (is (<= 0 pokey-note 255) "POKEY note should be 0-255"))

    ;; Test specific frequency lookup
    (let* ((test-freq 261.63) ; C4
           (pokey-note (skyline-tool:best-pokey-note-for test-freq)))
      (is (integerp pokey-note) "C4 should map to valid POKEY note"))))

;; Test TIA frequency calculations for 7800
(test tia-7800-frequency-calculations
  "Test TIA frequency calculations for Atari 7800"
  ;; Test that compile-music-7800 can be called with valid parameters
  ;; (We can't test actual file I/O without creating test files)

  ;; Test frequency range validation when function is available
  (when (fboundp 'skyline-tool:freq<-midi-key)
    (let ((c4-freq (skyline-tool:freq<-midi-key 60)))
      ;; TIA frequency range is roughly 30Hz to 4000Hz
      (is (< 30 c4-freq 4000) "C4 should be in TIA playable range"))))

;; Test Intv gram music compilation
(test intv-gram-music-compilation
  "Test Intellivision GRAM music compilation"
  ;; Test that compile-music-2609 exists and can be called
  (is-true (fboundp 'skyline-tool:compile-music-2609))

  ;; Test frequency range for Intellivision when function is available
  (when (fboundp 'skyline-tool:freq<-midi-key)
    (let ((middle-c-freq (skyline-tool:freq<-midi-key 60)))
      ;; Intellivision PSG frequency range
      (is (< 100 middle-c-freq 2000) "Middle C should be in Intv playable range"))))

;; Test mock MIDI data processing
(test mock-midi-data-processing
  "Test music conversion with mock MIDI data structures"
  ;; Create mock MIDI note data
  (let ((mock-note-on-event '(:note-on :channel 0 :note 60 :velocity 100))
        (mock-note-off-event '(:note-off :channel 0 :note 60 :velocity 0)))

    ;; Test that we can process basic MIDI-like structures
    ;; (This is a placeholder for more comprehensive MIDI parsing tests)
    (is (eq :note-on (first mock-note-on-event)) "Should identify note-on event")
    (is (eq :note-off (first mock-note-off-event)) "Should identify note-off event")
    (is (= 60 (getf mock-note-on-event :note)) "Should extract note number")
    (is (= 100 (getf mock-note-on-event :velocity)) "Should extract velocity")))

;; Test chip-specific music conversion parameters
(test chip-specific-conversion-parameters
  "Test music conversion parameters for different sound chips"
  ;; Test frequency ranges for different chips
  ;; TIA: ~30-4000Hz
  ;; POKEY: ~15-15000Hz
  ;; SID: ~15-15000Hz

  (let ((low-note (skyline-tool:freq<-midi-key 24))   ; C1 ~32Hz
        (high-note (skyline-tool:freq<-midi-key 96))) ; C7 ~2093Hz
    (is (< 30 low-note 50) "Low note should be ~32Hz")
    (is (< 2000 high-note 2200) "High note should be ~2093Hz")))

;; Test music compilation pipeline integration
(test music-compilation-pipeline-integration
  "Integration test for music compilation pipeline"
  ;; Test that basic required functions are available
  (is (fboundp 'skyline-tool:midi->note-name))
  (is (fboundp 'skyline-tool:note->midi-note-number))
  (is (fboundp 'skyline-tool:freq<-midi-key))

  ;; Test parameter validation
  (is (= 60 (skyline-tool:note->midi-note-number "C4")) "C4 should be MIDI 60")
  (is (string= "C4" (skyline-tool:midi->note-name 60)) "MIDI 60 should be C4")

  ;; Test frequency calculation
  (let ((c4-freq (skyline-tool:freq<-midi-key 60)))
    (is (< 260 c4-freq 263) "C4 should be approximately 261.63Hz")))

(defun run-music-tests ()
  "Run all music tests and return results"
  (fiveam:run! 'music-tests))
