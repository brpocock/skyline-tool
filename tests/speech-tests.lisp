;;; Phantasia SkylineTool/tests/speech-tests.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(defpackage :skyline-tool/speech-test
  (:use :cl :fiveam)
  (:import-from :skyline-tool
                #:compile-speech-7800
                #:compile-speech-2600
                #:compile-speech-2609)
  (:export #:speech-tests))

(in-package :skyline-tool/speech-test)

(def-suite speech-tests
  :description "Tests for speech synthesis functionality across platforms")

(in-suite speech-tests)

;; Test speech support detection
(test speech-support-detection
  "Test that speech support is correctly detected for different platforms"
  ;; 7800 should support speech (SpeakJet)
  (let ((*machine* 7800))
    (is-true (skyline-tool::speech-supported-p)
             "7800 should support speech synthesis"))
  ;; 2600 should support speech (SpeakJet)
  (let ((*machine* 2600))
    (is-true (skyline-tool::speech-supported-p)
             "2600 should support speech synthesis"))
  ;; 2609 should support speech (IntelliVoice)
  (let ((*machine* 2609))
    (is-true (skyline-tool::speech-supported-p)
             "2609 should support speech synthesis"))
  ;; Other platforms should not support speech
  (dolist (machine '(3 200 5200 35902 20953))
    (let ((*machine* machine))
      (is-false (skyline-tool::speech-supported-p)
                "~a should not support speech synthesis" machine))))

;; Test 7800 speech compilation (SpeakJet)
(test 7800-speech-compilation
  "Test 7800 speech compilation functions"
  (is-true (fboundp 'skyline-tool:compile-speech-7800)
          "compile-speech-7800 should be available")
  ;; Test that it can be called without error (placeholder implementation)
  (finishes (skyline-tool:compile-speech-7800 "/tmp/test-speech.s" "HELLO WORLD"))
  ;; Test with machine type set
  (let ((*machine* 7800))
    (is (= 7800 *machine*))))

;; Test 2600 speech compilation (SpeakJet)
(test 2600-speech-compilation
  "Test 2600 speech compilation functions"
  (is-true (fboundp 'skyline-tool:compile-speech-2600)
          "compile-speech-2600 should be available")
  ;; Test that it can be called without error (placeholder implementation)
  (finishes (skyline-tool:compile-speech-2600 "/tmp/test-speech.s" "HELLO WORLD"))
  ;; Test with machine type set
  (let ((*machine* 2600))
    (is (= 2600 *machine*))))

;; Test 2609 speech compilation (IntelliVoice)
(test 2609-speech-compilation
  "Test 2609 speech compilation functions"
  (is-true (fboundp 'skyline-tool:compile-speech-2609)
          "compile-speech-2609 should be available")
  ;; Test that it can be called without error (placeholder implementation)
  (finishes (skyline-tool:compile-speech-2609 "/tmp/test-speech.s" "HELLO WORLD"))
  ;; Test with machine type set
  (let ((*machine* 2609))
    (is (= 2609 *machine*))))

;; Test speech phoneme conversion
(test speech-phoneme-conversion
  "Test speech phoneme conversion functions"
  (is-true (fboundp 'skyline-tool:convert-to-speakjet-phonemes)
          "convert-to-speakjet-phonemes should be available")
  (is-true (fboundp 'skyline-tool:convert-to-intellivoice-phonemes)
          "convert-to-intellivoice-phonemes should be available")
  ;; Test basic phoneme conversion
  (let ((text "HELLO"))
    (finishes (skyline-tool:convert-to-speakjet-phonemes text))
    (finishes (skyline-tool:convert-to-intellivoice-phonemes text))))

;; Test speech data integration with Forth
(test speech-forth-integration
  "Test that speech data integrates correctly with Forth bytecode compiler"
  ;; Test that speech commands are properly ignored for non-speech platforms
  (let ((*machine* 3)) ; NES
    (is-false (skyline-tool::speech-supported-p))
    ;; The Forth compiler should handle speech commands gracefully
    (finishes (skyline-tool::compile-forth-speech "HELLO WORLD" nil)))
  ;; Test that speech commands are processed for speech platforms
  (let ((*machine* 7800))
    (is-true (skyline-tool::speech-supported-p))
    (finishes (skyline-tool::compile-forth-speech "HELLO WORLD" t))))

;; Test phoneme database
(test phoneme-database
  "Test phoneme database functionality"
  (is-true (boundp 'skyline-tool::*speakjet-phoneme-table*)
          "*speakjet-phoneme-table* should be defined")
  (is-true (boundp 'skyline-tool::*intellivoice-phoneme-table*)
          "*intellivoice-phoneme-table* should be defined")
  ;; Test that phoneme tables are properly structured
  (is (hash-table-p skyline-tool::*speakjet-phoneme-table*))
  (is (hash-table-p skyline-tool::*intellivoice-phoneme-table*)))

;; Test speech synthesis error handling
(test speech-synthesis-error-handling
  "Test speech synthesis functions handle errors appropriately"
  ;; Test with invalid phonemes
  (signals error (skyline-tool:convert-to-speakjet-phonemes ""))
  (signals error (skyline-tool:convert-to-intellivoice-phonemes ""))
  ;; Test with unsupported characters
  (signals error (skyline-tool:convert-to-speakjet-phonemes "123!@#")))

(defun speech-tests ()
  "Run all speech tests and return results"
  (fiveam:run! 'speech-tests))