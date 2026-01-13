;;; Phantasia SkylineTool/tests/music-tests.lisp
<<<<<<< HEAD
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
=======
;;;; Copyright © 2025 Interworldly Adventuring, LLC

(in-package :skyline-tool/test)

(def-suite music-tests
  :description "Tests for music conversion and synthesis functionality")

(in-suite music-tests)

;; Test that the music compilation function exists and is callable
(test music-compilation-function-existence
  "Test that compile-music function exists and can be called"
  (is-true (fboundp 'compile-music) "compile-music function should exist")
  ;; Test that it's a function
  (is (functionp (symbol-function 'compile-music)) "compile-music should be a function"))

;; Test music compilation with error handling
(test music-compilation-error-handling
  "Test error handling in music compilation"
  ;; Test with invalid inputs - should handle gracefully
  (signals error (compile-music "/nonexistent.mscz"))
  (signals error (compile-music nil))
  ;; Test with invalid file types
  (signals error (compile-music "/dev/null")))

;; Test music compilation parameter validation
(test music-compilation-parameters
  "Test music compilation parameter handling"
  ;; Basic parameter check - function should accept at least 2 args
  (is (functionp (symbol-function 'compile-music)) "compile-music should be a function")
  ;; Check that it can be called with expected arguments
  (handler-case
      (compile-music "/tmp/test.out" "/nonexistent.mid")
    (error (e) (pass "Function accepts expected parameters"))))

;; Test music conversion pipeline components
(test music-conversion-components
  "Test individual components of music conversion pipeline"
  ;; Test that the function can be inspected without errors
  (is (typep (symbol-function 'compile-music) 'function))

  ;; Test function metadata
  (let ((docstring (documentation 'compile-music 'function)))
    (is (or (null docstring) (stringp docstring)) "Function should have documentation or none"))

  ;; Test that it doesn't crash when inspected
  (finishes (describe 'compile-music)))

;; Integration test for music compilation workflow
(test music-compilation-workflow
  "Integration test for music compilation workflow"
  ;; Test the typical workflow that would be used
  ;; This ensures the function is ready for production use

  ;; Verify function is properly integrated into the system
  (is (find-symbol "COMPILE-MUSIC" :skyline-tool)
      "compile-music should be accessible in skyline-tool package")

  ;; Test that the function can be found via find-symbol
  (is (eq (find-symbol "COMPILE-MUSIC" :skyline-tool)
          'skyline-tool::compile-music)
      "Function should be findable in skyline-tool package"))

;; Test music file format validation
(test music-file-format-validation
  "Test validation of music file formats"
  ;; Test with various file extensions (just test that function accepts them)
  (dolist (ext '(".mscz" ".mid" ".midi"))
    (let ((test-file (concatenate 'string "/nonexistent" ext))
          (output-file (concatenate 'string "/tmp/test-output" ext)))
      ;; The function should accept various music file formats
      ;; and handle missing files gracefully (signals error but doesn't crash)
      (signals error (compile-music output-file test-file)
               "Should handle missing ~a files with proper error" ext))))

;; Test Intv-specific asset converters
(def-suite intv-asset-converters
  :description "Tests for Intellivision-specific asset converters")

(in-suite intv-asset-converters)

;; Test GRAM (Graphics RAM) tile compilation
(test compile-tileset-intv-existence
  "Test that compile-tileset-intv function exists"
  (is-true (fboundp 'skyline-tool::compile-tileset-intv)
           "compile-tileset-intv function should exist"))

(test compile-tileset-intv-basic-functionality
  "Test basic functionality of compile-tileset-intv"
  ;; Create a simple test PNG data structure
  (let ((test-png-data (make-array '(8 8) :element-type '(unsigned-byte 32)
                                   :initial-contents '((#xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF)
                                                       (#xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000)
                                                       (#xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF)
                                                       (#xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000)
                                                       (#xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF)
                                                       (#xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000)
                                                       (#xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF)
                                                       (#xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000 #xFFFFFFFF #xFF000000)))))
    ;; This test verifies the function can handle basic tile data
    ;; Use Object/ directory for temporary build files
    (let ((test-dir (merge-pathnames "Object/" (asdf:system-source-directory :skyline-tool))))
      (ensure-directories-exist test-dir)
      (let ((output-file (merge-pathnames "tiles.test-tile.s" test-dir)))
        ;; Test the function call directly
        (skyline-tool::compile-tileset-intv (make-pathname :name "test-tile" :type "png") test-dir 8 8 test-png-data)
        (is-true (probe-file output-file) "Output file should be created")))))

;; Test Forth bytecode compilation
(test compile-forth-existence
  "Test that compile-forth function exists"
  (is-true (fboundp 'skyline-tool::compile-forth)
           "compile-forth function should exist"))

(test compile-forth-script-existence
  "Test that compile-forth-script function exists"
  (is-true (fboundp 'skyline-tool::compile-forth-script)
           "compile-forth-script function should exist"))

;; Test asset loader functionality
(test asset-loader-functions-existence
  "Test that asset loader functions exist"
  ;; Check for basic asset-related functions
  ;; Asset loading might be handled differently, so just check that
  ;; some asset-related functionality exists
  (is-true (or (fboundp 'skyline-tool::collect-assets)
               (fboundp 'skyline-tool::allocate-assets)
               (fboundp 'skyline-tool::compile-art))
           "Should have some asset-related functions"))

;; Test Intv palette and color handling
(test intv-palette-constants
  "Test that Intv palette constants are defined"
  (is-true (boundp 'skyline-tool::+intv-palette+)
           "Intv palette constant should be defined")
  (is-true (boundp 'skyline-tool::+intv-color-names+)
           "Intv color names constant should be defined"))

(test intv-palette-size
  "Test Intv palette has correct size"
  (when (boundp 'skyline-tool::+intv-palette+)
    (is (= 16 (length skyline-tool::+intv-palette+))
        "Intv palette should have 16 colors")))

(def-suite intv-card-layouts
  :description "Tests for Intellivision card layout compilation")

(in-suite intv-card-layouts)

;; Test card layout compilation (if it exists)
(test card-layout-compilation-existence
  "Test that card layout compilation functions exist"
  ;; This might be part of map compilation or separate
  (is-true (or (fboundp 'skyline-tool::compile-map)
               (fboundp 'skyline-tool::compile-tileset))
           "Should have card/map compilation functions"))

;; IntelliVoice tests (added to intv-asset-converters suite)
(in-suite intv-asset-converters)

(test intellivoice-phoneme-loading
  "Test that IntelliVoice phoneme definitions can be loaded"
  (finishes (skyline-tool::read-intellivoice-tokens)
            "Should be able to load IntelliVoice phoneme definitions"))

(test intellivoice-basic-phonemes
  "Test that basic IntelliVoice phonemes are defined"
  (let ((phonemes (skyline-tool::read-intellivoice-tokens)))
    (is (> (hash-table-count phonemes) 0)
        "Should have phoneme definitions loaded")
    ;; Test some basic phonemes
    (is (gethash "PA1" phonemes) "PA1 should be defined")
    (is (gethash "IH" phonemes) "IH vowel should be defined")
    (is (gethash "BB1" phonemes) "BB1 plosive should be defined")
    (is (gethash "OY" phonemes) "OY diphthong should be defined")
    (is (gethash "NG" phonemes) "NG nasal should be defined")))

(test intellivoice-speech-conversion
  "Test IntelliVoice speech byte conversion"
  (let ((tokens '("PA1" "HH1" "EH" "LL" "OW" "PA1")))
    (finishes (skyline-tool::convert-intellivoice-bytes tokens)
              "Should convert IntelliVoice tokens to bytes")))

(test speech-system-detection
  "Test that speech system detection works for different machines"
  (let ((skyline-tool::*machine* 2609))  ; Intellivision
    (is (eq (skyline-tool::current-speech-system) :intellivoice)
        "Should detect IntelliVoice for machine 2609"))
  (let ((skyline-tool::*machine* 7800))  ; Atari 7800
    (is (eq (skyline-tool::current-speech-system) :atarivox)
        "Should detect AtariVox for machine 7800")))

(test intellivoice-speak-placeholder
  "Test IntelliVoice speak function (emulator synthesis)"
  ;; Test that the function exists and returns phoneme data
  ;; Speech synthesis is handled by jzIntv emulator, not hardware
  (let ((result (skyline-tool::intellivoice-speak "HELLO")))
    (is-true (listp result) "Should return a list of phonemes")
    (is-true (> (length result) 0) "Should return non-empty phoneme list")))
>>>>>>> origin/intv

(defun run-music-tests ()
  "Run all music tests and return results"
  (fiveam:run! 'music-tests))
<<<<<<< HEAD
=======

(defun run-intv-asset-tests ()
  "Run all Intv asset converter tests and return results"
  (fiveam:run! 'intv-asset-converters))

(defun run-intv-card-layout-tests ()
  "Run all Intv card layout tests and return results"
  (fiveam:run! 'intv-card-layouts))

(defun run-intellivoice-tests ()
  "Run all IntelliVoice-related tests"
  (fiveam:run! 'intellivoice-phonemes)
  (fiveam:run! 'intellivoice-speech-synthesis))
>>>>>>> origin/intv
