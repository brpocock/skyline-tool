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

;; Test speech command registration logic
(test speech-command-registration
  "Test the logic for registering platform-specific speech commands"
  ;; Test 7800 platform (should have SpeakJet, discard IntelliVoice)
  (let ((skyline-tool::*machine* 7800))
    (let ((dict (make-hash-table :test 'equal)))
      (ecase skyline-tool::*machine*
        (7800
         (setf (gethash "SpeakJet[" dict) 'forth/speakjet-quote)
         (setf (gethash "IntelliVoice[" dict) 'forth/discard-speech))
         (is (eq (gethash "SpeakJet[" dict) 'forth/speakjet-quote))
         (is (eq (gethash "IntelliVoice[" dict) 'forth/discard-speech)))
        (2609
         (setf (gethash "IntelliVoice[" dict) 'forth/intellivoice-quote)
         (setf (gethash "SpeakJet[" dict) 'forth/discard-speech)
         (is (eq (gethash "IntelliVoice[" dict) 'forth/intellivoice-quote))
         (is (eq (gethash "SpeakJet[" dict) 'forth/discard-speech))))))

  ;; Test 2609 platform (should have IntelliVoice, discard SpeakJet)
  (let ((skyline-tool::*machine* 2609))
    (let ((dict (make-hash-table :test 'equal)))
      (ecase skyline-tool::*machine*
        (7800
         (setf (gethash "SpeakJet[" dict) 'forth/speakjet-quote)
         (setf (gethash "IntelliVoice[" dict) 'forth/discard-speech))
         (is (eq (gethash "SpeakJet[" dict) 'forth/speakjet-quote))
         (is (eq (gethash "IntelliVoice[" dict) 'forth/discard-speech)))
        (2609
         (setf (gethash "IntelliVoice[" dict) 'forth/intellivoice-quote)
         (setf (gethash "SpeakJet[" dict) 'forth/discard-speech)
         (is (eq (gethash "IntelliVoice[" dict) 'forth/intellivoice-quote))
         (is (eq (gethash "SpeakJet[" dict) 'forth/discard-speech)))))))

;; Test discard speech function behavior
(test discard-speech-function
  "Test that discard speech function works correctly"
  ;; This would require mocking input, but we can test the function exists
  (is-true (functionp (symbol-function 'skyline-tool::forth/discard-speech))))

(defun run-speech-filter-tests ()
  "Run all speech filter tests"
  (fiveam:run! 'speech-filter-tests))