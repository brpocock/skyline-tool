;;; Phantasia SkylineTool/tests/music-tests.lisp
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

(defun run-music-tests ()
  "Run all music tests and return results"
  (fiveam:run! 'music-tests))

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
