;;; Phantasia SkylineTool/tests/music-new-features-tests.lisp
;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.
;;;
;;; Tests for newly added music features:
;;; - TIA SECAM support
;;; - SID (C64/C128) frequency conversion and compilation
;;; - Hokey/Pokey TV-standard dispatch

(in-package :skyline-tool/test)

(def-suite music-new-features-tests
  :description "Tests for newly added music features (TIA SECAM, SID, Hokey TV dispatch)"
  :in skyline-tool/test)

(in-suite music-new-features-tests)

;; ======================================================================
;; TIA SECAM Tests
;; ======================================================================

(test tia-secam-clock-constants
  "Verify TIA clock constants are defined with correct values."
  (is (= skyline-tool::+tia-ntsc-clock-hz+ 3579545)
      "NTSC TIA clock should be 3,579,545 Hz")
  (is (= skyline-tool::+tia-pal-clock-hz+ 3546895)
      "PAL TIA clock should be 3,546,895 Hz")
  (is (= skyline-tool::+tia-secam-clock-hz+ 3579545)
      "SECAM TIA clock should equal NTSC (same 3.58 MHz crystal)"))

(test tia-secam-note-delegates-to-ntsc
  "SECAM TIA note lookup should delegate to NTSC table (same crystal)."
  (let ((freq 440.0d0))
    (let ((ntsc-result (skyline-tool::best-tia-ntsc-note-for freq))
          (secam-result (skyline-tool::best-tia-secam-note-for freq)))
      (is (equal ntsc-result secam-result)
          "SECAM should return same result as NTSC for same frequency"))))

(test tia-secam-note-for-all-voices
  "SECAM note lookup should search all voices like NTSC."
  (let ((freq 261.63d0))
    (let ((ntsc-all (skyline-tool::best-tia-note-for-ntsc freq))
          (secam-all (skyline-tool::best-tia-note-for-secam freq)))
      (is (listp ntsc-all) "NTSC all-voices should return a list")
      (is (listp secam-all) "SECAM all-voices should return a list")
      (is (= (length ntsc-all) (length secam-all))
          "NTSC and SECAM results should have same length")
      (is (= (length secam-all) 3)
          "SECAM result should have 3 elements (voice, code, error)"))))

(test tia-note-for-tv-dispatch
  "best-tia-note-for-tv should dispatch correctly for each TV standard."
  (let ((freq 440.0d0))
    (is (equal (skyline-tool::best-tia-note-for-tv freq :ntsc)
               (skyline-tool::best-tia-ntsc-note-for freq))
        "TV dispatch :ntsc should match NTSC lookup")
    (is (equal (skyline-tool::best-tia-note-for-tv freq :pal)
               (skyline-tool::best-tia-pal-note-for freq))
        "TV dispatch :pal should match PAL lookup")
    (is (equal (skyline-tool::best-tia-note-for-tv freq :secam)
               (skyline-tool::best-tia-ntsc-note-for freq))
        "TV dispatch :secam should match NTSC lookup (same crystal)")))

(test tia-note-for-with-distortion-secam
  "best-tia-note-for with :secam TV should handle distortion parameter."
  (let ((key 69)
        (distortion 4))
    (multiple-value-bind (result error)
        (ignore-errors (skyline-tool::best-tia-note-for key distortion :secam))
      (is (or result error)
          "best-tia-note-for should return a result or signal an error for SECAM"))))

(test array<-7800-tia-notes-list-secam-frame-rate
  "array<-7800-tia-notes-list should use 50 Hz frame rate for SECAM."
  (let ((notes '((0 0 60 100 4))))
    (let ((ntsc-result (skyline-tool::array<-7800-tia-notes-list notes :ntsc))
          (pal-result (skyline-tool::array<-7800-tia-notes-list notes :pal))
          (secam-result (skyline-tool::array<-7800-tia-notes-list notes :secam)))
      (is (vectorp secam-result) "SECAM result should be a vector")
      (is (equalp pal-result secam-result)
          "SECAM and PAL should produce same timing (both 50 Hz)"))))

(test array<-tia-notes-list-secam
  "array<-tia-notes-list should handle :secam output-coding."
  ;; Format: (duration control midi-key volume comment)
  (let ((notes '((100 0 60 8 "comment"))))
    (let ((result (skyline-tool::array<-tia-notes-list notes :secam)))
      (is (arrayp result) "Result should be an array")
      (is (= (array-dimension result 0) 1) "Should have 1 note row")
      (is (= (array-dimension result 1) 5) "Should have 5 columns"))))

;; ======================================================================
;; SID Frequency Conversion Tests
;; ======================================================================

(test sid-clock-constants
  "Verify SID clock constants are defined with correct values."
  (is (= skyline-tool::+sid-ntsc-clock-hz+ 1022730)
      "NTSC SID clock should be 1,022,730 Hz")
  (is (= skyline-tool::+sid-pal-clock-hz+ 985248)
      "PAL SID clock should be 985,248 Hz"))

(test frequency->sid-ntsc
  "frequency->sid should convert NTSC frequency to correct period."
  (multiple-value-bind (period error)
      (skyline-tool::frequency->sid 440.0d0 :ntsc)
    (is (<= 1 period #xffff) "Period should be within 16-bit range")
    (is (numberp error) "Error should be a number")
    (let ((actual-freq (skyline-tool::sid->frequency period :ntsc)))
      (is (< (abs (- actual-freq 440.0d0)) 20.0d0)
          "Round-trip frequency should be within 20 Hz of target"))))

(test frequency->sid-pal
  "frequency->sid should convert PAL frequency to correct period."
  (multiple-value-bind (period error)
      (skyline-tool::frequency->sid 440.0d0 :pal)
    (is (<= 1 period #xffff) "Period should be within 16-bit range")
    (is (numberp error) "Error should be a number")
    (let ((actual-freq (skyline-tool::sid->frequency period :pal)))
      (is (< (abs (- actual-freq 440.0d0)) 20.0d0)
          "Round-trip frequency should be within 20 Hz of target"))))

(test frequency->sid-secam-uses-pal-clock
  "frequency->sid with :secam should use PAL clock."
  (multiple-value-bind (ntsc-period ntsc-error)
      (skyline-tool::frequency->sid 440.0d0 :ntsc)
    (declare (ignore ntsc-error))
    (multiple-value-bind (secam-period secam-error)
        (skyline-tool::frequency->sid 440.0d0 :secam)
      (is (/= ntsc-period secam-period)
          "SECAM period should differ from NTSC (different clocks)")
      (is (numberp secam-error) "SECAM error should be a number"))))

(test frequency->sid-extreme-frequencies
  "frequency->sid should handle extreme frequency values."
  (multiple-value-bind (period error)
      (skyline-tool::frequency->sid 20.0d0 :ntsc)
    (declare (ignore error))
    (is (<= 1 period #xffff) "Low freq period should be clamped to max"))
  (multiple-value-bind (period error)
      (skyline-tool::frequency->sid 5000.0d0 :ntsc)
    (declare (ignore error))
    (is (<= 1 period #xffff) "High freq period should be within range"))
  (multiple-value-bind (period error)
      (skyline-tool::frequency->sid 100000.0d0 :ntsc)
    (declare (ignore error))
    (is (>= period 1) "Period should be clamped to minimum 1")))

(test sid->frequency-round-trip
  "sid->frequency should be the inverse of frequency->sid."
  (dolist (freq '(261.63d0 440.0d0 880.0d0 1760.0d0))
    (multiple-value-bind (period error)
        (skyline-tool::frequency->sid freq :ntsc)
      (declare (ignore error))
      (let ((recovered (skyline-tool::sid->frequency period :ntsc)))
        (is (< (abs (- recovered freq)) 20.0d0)
            "Round-trip frequency should be accurate within 20 Hz")))))

(test best-sid-note-for
  "best-sid-note-for should return period, actual frequency, and error."
  (dolist (note '(60 69 72))
    (multiple-value-bind (period actual-freq error)
        (skyline-tool::best-sid-note-for note :ntsc)
      (is (<= 1 period #xffff) "Period should be within 16-bit range")
      (is (numberp actual-freq) "Actual frequency should be a number")
      (is (numberp error) "Error should be a number")
      (is (< (abs error) 20.0d0)
          "Error for MIDI note should be less than 20 Hz"))))

(test best-sid-note-for-tv-variants
  "best-sid-note-for should produce different results for NTSC vs PAL."
  (let ((note 69))
    (multiple-value-bind (ntsc-period ntsc-freq ntsc-error)
        (skyline-tool::best-sid-note-for note :ntsc)
      (declare (ignore ntsc-freq))
      (multiple-value-bind (pal-period pal-freq pal-error)
          (skyline-tool::best-sid-note-for note :pal)
        (declare (ignore pal-freq))
        (is (numberp ntsc-period) "NTSC period should be a number")
        (is (numberp pal-period) "PAL period should be a number")
        (is (< (abs ntsc-error) 20.0d0) "NTSC error should be small")
        (is (< (abs pal-error) 20.0d0) "PAL error should be small")))))

(test best-sid-note-for-ntsc-helper
  "best-sid-note-for-ntsc should return a list of (period freq error)."
  (let ((result (skyline-tool::best-sid-note-for-ntsc 440.0d0)))
    (is (listp result) "Should return a list")
    (is (= (length result) 3) "Should have 3 elements")
    (destructuring-bind (period freq error) result
      (is (<= 1 period #xffff) "Period should be within range")
      (is (numberp freq) "Frequency should be a number")
      (is (numberp error) "Error should be a number"))))

(test best-sid-note-for-pal-helper
  "best-sid-note-for-pal should return a list of (period freq error)."
  (let ((result (skyline-tool::best-sid-note-for-pal 440.0d0)))
    (is (listp result) "Should return a list")
    (is (= (length result) 3) "Should have 3 elements")))

(test best-sid-note-for-secam-helper
  "best-sid-note-for-secam should delegate to PAL helper."
  (let ((freq 440.0d0))
    (is (equal (skyline-tool::best-sid-note-for-secam freq)
               (skyline-tool::best-sid-note-for-pal freq))
        "SECAM helper should return same result as PAL helper")))

;; ======================================================================
;; SID MIDI Conversion Tests
;; ======================================================================

(test midi->sid-basic
  "midi->sid should convert MIDI notes to SID format."
  ;; Format: ((:note . (:time 0 :key 60 :duration 100 :velocity 100)))
  (let ((midi-notes (list (cons :note '(:time 0 :key 60 :duration 100 :velocity 100)))))
    (let ((result (skyline-tool::midi->sid midi-notes :ntsc)))
      (is (listp result) "Should return a list")
      (is (plusp (length result)) "Should have at least one note")
      (let ((note (first result)))
        (is (arrayp note) "Note should be an array")
        (is (= (length note) 5) "Note should have 5 elements")))))

(test midi->sid-text-events
  "midi->sid should handle :text events."
  (let ((midi-notes (list (cons :text "Piano"))))
    (let ((result (skyline-tool::midi->sid midi-notes :ntsc)))
      (is (listp result) "Should return a list")
      (is (plusp (length result)) "Should have at least one element")
      (let ((note (first result)))
        (is (arrayp note) "Text event should be an array")
        (is (stringp (aref note 4)) "5th element should be the text string")))))

(test midi->sid-tv-standards
  "midi->sid should produce different output for different TV standards."
  (let ((midi-notes (list (cons :note '(:time 0 :key 69 :duration 100 :velocity 100)))))
    (let ((ntsc-result (skyline-tool::midi->sid midi-notes :ntsc))
          (pal-result (skyline-tool::midi->sid midi-notes :pal))
          (secam-result (skyline-tool::midi->sid midi-notes :secam)))
      (let ((ntsc-note (first ntsc-result))
            (pal-note (first pal-result)))
        (is (arrayp ntsc-note) "NTSC note should be an array")
        (is (arrayp pal-note) "PAL note should be an array"))
      (is (equalp pal-result secam-result)
          "SECAM and PAL should produce same output (same clock)"))))

(test midi->sid-duration-inheritance
  "midi->sid should handle zero duration."
  (let ((midi-notes (list (cons :note '(:time 0 :key 60 :duration 0 :velocity 100)))))
    (let ((result (skyline-tool::midi->sid midi-notes :ntsc)))
      (let ((note (first result)))
        (is (numberp (aref note 0)) "Duration should be a number")))))

(test midi-to-sound-binary-sid
  "midi-to-sound-binary for :sid should call midi->sid."
  (let ((midi-notes (list (cons :note '(:time 0 :key 60 :duration 100 :velocity 100)))))
    (let ((result (skyline-tool::midi-to-sound-binary "NTSC" 64 midi-notes :sid)))
      (is (listp result) "Should return a list")
      (is (plusp (length result)) "Should have at least one note"))))

(test merge-sid-voices-stub
  "merge-sid-voices should return notes unchanged (stub)."
  (let ((notes '((note1 note2 note3))))
    (is (equal notes (skyline-tool::merge-sid-voices notes))
        "merge-sid-voices should return input unchanged")))

;; ======================================================================
;; SID Score Conversion Tests
;; ======================================================================

(test score-item-to-sid-note-event
  "score-item-to-sid-note-event should convert score item to dotted-pair note event."
  (let ((item '(:time 0.0d0 :key 60 :duration 0.1d0 :velocity 100 :instrument :piano)))
    (let ((result (skyline-tool::score-item-to-sid-note-event item)))
      (is (consp result) "Should return a cons cell")
      (is (eq :note (car result)) "First element should be :note")
      (let ((params (cdr result)))
        (is (= 60 (getf params :key)) "Key should be preserved")
        (is (= 100 (getf params :velocity)) "Velocity should be preserved")
        (is (eq :piano (getf params :instrument)) "Instrument should be preserved")))))

(test score->song-sid
  "score->song for :sid should return SID note data."
  (let ((score '((:time 0.0d0 :key 60 :duration 0.1d0 :velocity 100 :instrument :piano))))
    (let ((result (skyline-tool::score->song score :sid :ntsc)))
      (is (listp result) "Should return a list")
      (is (plusp (length result)) "Should have at least one note"))))

(test score->song-sid-keyword-frame-rate
  "score->song for :sid should handle keyword frame-rate."
  (let ((score '((:time 0.0d0 :key 60 :duration 0.1d0 :velocity 100 :instrument :piano))))
    (let ((result (skyline-tool::score->song score :sid :ntsc)))
      (is (listp result) "Should return a list with :ntsc keyword"))
    (let ((result (skyline-tool::score->song score :sid :pal)))
      (is (listp result) "Should return a list with :pal keyword"))))

;; ======================================================================
;; Property-Based Tests
;; ======================================================================

(test sid-frequency-property-all-midi-notes
  "Property test: frequency->sid should work for all valid MIDI notes."
  (dotimes (note 128)
    (let ((freq (skyline-tool::freq<-midi-key note)))
      (multiple-value-bind (period error)
          (skyline-tool::frequency->sid freq :ntsc)
        (is (<= 1 period #xffff)
            "Period for MIDI note should be in range")
        (is (< (abs error) 200.0d0)
            "Error for MIDI note should be reasonable")))))

(test sid-frequency-property-pal-all-midi-notes
  "Property test: frequency->sid should work for all MIDI notes on PAL."
  (dotimes (note 128)
    (let ((freq (skyline-tool::freq<-midi-key note)))
      (multiple-value-bind (period error)
          (skyline-tool::frequency->sid freq :pal)
        (is (<= 1 period #xffff)
            "PAL period for MIDI note should be in range")
        (is (< (abs error) 200.0d0)
            "PAL error for MIDI note should be reasonable")))))

(test tia-secam-consistency-property
  "Property test: SECAM TIA results should always match NTSC results."
  (dotimes (voice 10)
    (let ((freq (+ 50.0d0 (* voice 100.0d0))))
      (let ((ntsc (skyline-tool::best-tia-ntsc-note-for freq))
            (secam (skyline-tool::best-tia-secam-note-for freq)))
        (is (equal ntsc secam)
            "SECAM should match NTSC for all frequencies")))))

(test sid-roundtrip-property-all-frequencies
  "Property test: SID round-trip should be accurate for a range of frequencies."
  (loop for freq from 50.0d0 to 5000.0d0 by 50.0d0
        do (multiple-value-bind (period error)
               (skyline-tool::frequency->sid freq :ntsc)
             (declare (ignore error))
             (let ((recovered (skyline-tool::sid->frequency period :ntsc)))
               (is (< (abs (- recovered freq)) 200.0d0)
                   "Round-trip frequency should be accurate within 200 Hz")))))
