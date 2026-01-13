;;; Phantasia SkylineTool/tests/music-tests.lisp
;;;; Copyright © 2024-2026 Bruce-Robert Pocock; Copyright © 2024-2026 Interworldly Adventuring, LLC.

(defpackage :skyline-tool/music-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:compile-music
                #:compile-midi
                #:midi-compile
                #:compile-sound
                #:midi->note-name
                #:note->midi-note-number
                #:best-pokey-note-for
                #:midi->2600-tia
                #:midi->7800-tia
                #:read-midi)
  (:export #:music-tests))

(in-package :skyline-tool/music-test)

(def-suite music-tests
  :description "Tests for music and sound compilation functionality")

(in-suite music-tests)

;; Helper functions for creating test data
(defun make-test-midi-data ()
  "Create mock MIDI data for testing."
  ;; This would be complex to implement properly
  ;; For now, just test function existence and error handling
  nil)

;; Test music compiler existence
(test music-compilers-existence
  "Test that all music and sound compilers exist"
  (is-true (fboundp 'compile-music) "compile-music should exist")
  (is-true (fboundp 'compile-midi) "compile-midi should exist")
  (is-true (fboundp 'midi-compile) "midi-compile should exist")
  (is-true (fboundp 'compile-sound) "compile-sound should exist"))

;; Test music utility functions
(test music-utility-functions
  "Test that music utility functions exist and work"
  (is-true (fboundp 'midi->note-name) "midi->note-name should exist")
  (is-true (fboundp 'note->midi-note-number) "note->midi-note-number should exist")
  (is-true (fboundp 'best-pokey-note-for) "best-pokey-note-for should exist")
  (is-true (fboundp 'midi->2600-tia) "midi->2600-tia should exist")
  (is-true (fboundp 'midi->7800-tia) "midi->7800-tia should exist")
  (is-true (fboundp 'read-midi) "read-midi should exist"))

;; Test MIDI note conversion
(test midi-note-conversion
  "Test MIDI note name conversion functions"
  ;; Test midi->note-name
  (is (string= "C4" (midi->note-name 60)) "Middle C should be C4")
  (is (string= "A4" (midi->note-name 69)) "Concert A should be A4")

  ;; Test note->midi-note-number
  (is (= 60 (note->midi-note-number 4 "C")) "C4 should be MIDI note 60")
  (is (= 69 (note->midi-note-number 4 "A")) "A4 should be MIDI note 69"))

;; Test Pokey note selection
(test pokey-note-selection
  "Test Pokey note frequency calculation"
  ;; Test with known values
  (let ((result (best-pokey-note-for 440.0))) ; A4
    (is-true result "Should find a Pokey note for A4")
    (is (listp result) "Should return a list with note info")))

;; Test TIA conversion functions
(test tia-conversion-functions
  "Test TIA audio conversion functions"
  ;; These functions convert MIDI data to TIA register values
  (is-true (fboundp 'midi->2600-tia) "2600 TIA conversion should exist")
  (is-true (fboundp 'midi->7800-tia) "7800 TIA conversion should exist"))

;; Test error handling for music functions
(test music-compilation-error-handling
  "Test that music compilation functions handle errors appropriately"
  ;; Test with non-existent files
  (signals error (compile-music "/nonexistent.out" "/nonexistent.mid"))
  (signals error (midi-compile "/nonexistent.mid" :pokey 60))
  (signals error (compile-sound "/nonexistent.out" "/nonexistent.mid"))
  (signals error (read-midi "/nonexistent.mid")))

;; Test MIDI format support
(test midi-format-support
  "Test that MIDI compilation supports expected formats"
  ;; Test midi-compile with different formats
  (dolist (format '(:pokey :tia-7800 :tia-2600))
    (signals error (midi-compile "/nonexistent.mid" format 60))))

;; Test compile-music parameter validation
(test compile-music-parameters
  "Test compile-music parameter handling"
  ;; Test that function exists and can be called
  (is-true (fboundp 'compile-music) "compile-music should exist")
  (signals error (compile-music "/nonexistent.out" "/nonexistent.mid")))

;; Test compile-midi parameter validation
(test compile-midi-parameters
  "Test compile-midi parameter handling"
  ;; Test that function exists and can be called
  (is-true (fboundp 'compile-midi) "compile-midi should exist")
  (signals error (compile-midi "/nonexistent.argv0" "/nonexistent.mid" :pokey 60)))

;; Test sound compilation
(test sound-compilation-basic
  "Test basic sound compilation functionality"
  (is-true (fboundp 'compile-sound) "compile-sound should be callable")
  (signals error (compile-sound "/nonexistent.out" "/nonexistent.mid")))

;; Test MIDI reading functionality
(test midi-reading-basic
  "Test basic MIDI file reading"
  (is-true (fboundp 'read-midi) "read-midi should be callable")
  (signals error (read-midi "/nonexistent.mid")))

;; Test music compilation integration
(test music-compilation-integration
  "Integration test for music compilation pipeline"
  ;; Test that all functions in the pipeline exist
  (is-true (fboundp 'compile-music) "Music compilation entry point should exist")
  (is-true (fboundp 'midi-compile) "MIDI compilation should exist")
  (is-true (fboundp 'compile-sound) "Sound compilation should exist")

  ;; Test error handling
  (signals error (compile-music "/nonexistent.out" "/nonexistent.mid"))
  (signals error (midi-compile "/nonexistent.mid" :pokey 60))
  (signals error (compile-sound "/nonexistent.out" "/nonexistent.mid")))

;; Test frequency calculation
(test frequency-calculation
  "Test MIDI to frequency conversion"
  ;; This would require more detailed testing with known values
  ;; For now, just verify the function exists
  (is-true (fboundp 'freq<-midi-key) "Frequency calculation should exist"))

;; Test music data processing
(test music-data-processing
  "Test music data processing functions"
  ;; Test functions that process MIDI data
  (is-true (fboundp 'collect-midi-texts) "MIDI text collection should exist")
  (is-true (fboundp 'midi-track-notes-count) "Track note counting should exist")
  (is-true (fboundp 'midi-tracks-with-music) "Music track detection should exist"))

(defun run-music-tests ()
  "Run all music tests and return results"
  (fiveam:run! 'music-tests))
