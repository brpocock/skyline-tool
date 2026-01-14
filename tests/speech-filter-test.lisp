;; Test speech filtering in Forth compiler

(in-package :skyline-tool/test)

(def-suite speech-filter-tests
  :description "Tests for platform-specific speech command filtering")

(in-suite speech-filter-tests)

;; Test that forth/discard-speech function exists
(test forth-discard-speech-exists
  "Test that the discard speech function exists"
  (is-true (fboundp 'skyline-tool::forth/discard-speech)))

;; Test platform detection
(test platform-detection
  "Test that platform variable is accessible"
  (is-true (boundp 'skyline-tool::*machine*)))

;; Test speech filtering logic by platform
(test speech-filtering-by-platform
  "Test that speech commands are filtered correctly based on platform"
  ;; Test 7800 platform filtering
  (let ((skyline-tool::*machine* 7800))
    ;; For 7800, SpeakJet commands should be kept, IntelliVoice should be discarded
    (is-true (skyline-tool::speech-command-supported-p "SpeakJet[")
             "7800 should support SpeakJet speech commands")
    (is-false (skyline-tool::speech-command-supported-p "IntelliVoice[")
             "7800 should not support IntelliVoice speech commands"))

  ;; Test Intellivision platform filtering
  (let ((skyline-tool::*machine* 2609))
    ;; For Intellivision, IntelliVoice commands should be kept, SpeakJet should be discarded
    (is-true (skyline-tool::speech-command-supported-p "IntelliVoice[")
             "Intellivision should support IntelliVoice speech commands")
    (is-false (skyline-tool::speech-command-supported-p "SpeakJet[")
             "Intellivision should not support SpeakJet speech commands")))

;; Test speech command filtering function exists
(test speech-command-filtering-function
  "Test that speech command filtering function exists"
  (is-true (fboundp 'skyline-tool::speech-command-supported-p)
           "speech-command-supported-p function should exist")
  (is-true (functionp (symbol-function 'skyline-tool::speech-command-supported-p))
           "speech-command-supported-p should be a function"))

;; Test discard speech function behavior
(test discard-speech-function
  "Test that discard speech function works correctly"
  ;; This would require mocking input, but we can test the function exists
  (is-true (functionp (symbol-function 'skyline-tool::forth/discard-speech))))

(defun run-speech-filter-tests ()
  "Run all speech filter tests"
  (fiveam:run! 'speech-filter-tests))
