;;; Phantasia SkylineTool/tests/speech-unit-test.lisp
;;; ;;;; Real tonal-envelope tests for the AtariVox speech pipeline
;;; ;;;; Copyright © 2026 Interworldly Adventuring, LLC.

;;; These tests exercise the tonal envelope (analyze-speech / compute-envelope /
;;; emit-envelope / combine-adjacent-pauses) without the SpeakJet dictionary, so
;;; they run hermeticly in any working directory.  The tokenizer mirrors the
;;; regex used by convert-for-atarivox, and the voice baseline is the default
;;; voice profile (pitch 90 / speed 90 / bend 5 / volume 12).

(in-package :skyline-tool/test)

(def-suite speech-unit-test
  :description "Speech unit construction and tonal envelope testing"
  :in skyline-tool/test)

(in-suite speech-unit-test)

(defun tokenize-words (string)
  "Tokenize STRING exactly as convert-for-atarivox does, returning the word list."
  (let (words)
    (cl-ppcre:do-scans (start end reg-starts reg-ends
                        "(\\s+|-|\\\\\\d+|[~\\\\]\\p{L}+|[\\p{L}\\p{N}’']+|[^\\s\\p{L}\\p{N}’'-]+)" string)
      (declare (ignore reg-starts reg-ends))
      (push (string-trim #(#\Space #\Tab #\Newline)
                         (subseq string start end))
            words))
    (nreverse words)))

(defun analyze-phrase (string)
  "Run the emphasis + tokenize + analyze pipeline on STRING.
Returns (values units sentence-type) as analyze-speech does."
  (skyline-tool::analyze-speech
   (skyline-tool::split-emphasis-tokens
    (tokenize-words (skyline-tool::emphasize-text string)))))

(defun content-units (units)
  "Return the :content units of UNITS."
  (remove-if-not (lambda (unit) (eql (skyline-tool::speech-unit-kind unit) :content))
                 units))

(defun last-content-unit (units)
  "Return the final :content unit of UNITS."
  (car (last (content-units units))))

(defun phonemize (units)
  "Assign placeholder phonemes to content units lacking them, for dict-free emit."
  (dolist (unit units)
    (when (and (eql (skyline-tool::speech-unit-kind unit) :content)
               (null (skyline-tool::speech-unit-phonemes unit)))
      (setf (skyline-tool::speech-unit-phonemes unit) (list "AY"))))
  units)

(test analyze-speech-returns-units-and-type
  "analyze-speech yields content/pause units and the sentence terminal type"
  (multiple-value-bind (units sentence-type)
      (analyze-phrase "Hey, can I ask you something?")
    (is (eq sentence-type :question) "sentence-type should be :question")
    (is (= 6 (length (content-units units))) "six content words expected")
    (let ((query (find-if (lambda (unit)
                            (and (eql (skyline-tool::speech-unit-kind unit) :pause)
                                 (string= (skyline-tool::speech-unit-text unit) "?")))
                          units)))
      (is-true query "a ? pause unit should exist")
      (is (member "Pause3" (skyline-tool::speech-unit-phonemes query) :test #'string=)
          "the ? pause should carry Pause3"))))

(test question-final-word-rises
  "The final word of a question rises +question-final-pitch-rise+ above baseline"
  (multiple-value-bind (units sentence-type)
      (analyze-phrase "Hey, can I ask you something?")
    (skyline-tool::compute-envelope units (skyline-tool::default-voice-profile) sentence-type)
    (let* ((content (content-units units))
           (final (car (last content)))
           (penultimate (car (last (butlast content))))
           (baseline skyline-tool::+character-default-pitch+))
      (is (= (+ baseline skyline-tool::+question-final-pitch-rise+)
             (skyline-tool::speech-unit-target-pitch final))
          "final word pitch should equal baseline + question rise")
      (is (> (skyline-tool::speech-unit-target-pitch final)
             (skyline-tool::speech-unit-target-pitch penultimate))
          "final word should rise above the penultimate word"))))

(test compound-question-rises-per-sentence
  "Each sentence-final word of a compound question rises; Pause3 survives"
  (multiple-value-bind (units sentence-type)
      (analyze-phrase "Hey, can I ask you something? Hey, _can_ I ask you something?")
    (skyline-tool::compute-envelope units (skyline-tool::default-voice-profile) sentence-type)
    (let* ((content (content-units units))
           (baseline skyline-tool::+character-default-pitch+)
           (first-final (nth 5 content))
           (second-final (car (last content))))
      (is (= (+ baseline skyline-tool::+question-final-pitch-rise+)
             (skyline-tool::speech-unit-target-pitch first-final))
          "first sentence-final word should rise 16 above baseline")
      (is (> (skyline-tool::speech-unit-target-pitch second-final) (+ baseline 12))
          "second sentence-final word should rise well above baseline")
      (is (find-if (lambda (unit)
                     (and (eql (skyline-tool::speech-unit-kind unit) :pause)
                          (member "Pause3" (skyline-tool::speech-unit-phonemes unit)
                                  :test #'string=)))
                   units)
          "an inter-sentence Pause3 should remain between the two questions"))))

(test exclamation-final-rises
  "An exclamation raises the final word's pitch and volume"
  (multiple-value-bind (units sentence-type)
      (analyze-phrase "Phantasia!")
    (skyline-tool::compute-envelope units (skyline-tool::default-voice-profile) sentence-type)
    (let* ((final (last-content-unit units))
           (baseline skyline-tool::+character-default-pitch+))
      (is (eq sentence-type :exclamation) "sentence-type should be :exclamation")
      (is (> (skyline-tool::speech-unit-target-pitch final)
             (+ baseline skyline-tool::+exclamation-final-pitch-rise+))
          "exclamation final should rise above baseline + exclamation rise")
      (is (= (+ skyline-tool::+character-default-volume+ skyline-tool::+exclamation-volume-bump+)
             (skyline-tool::speech-unit-target-volume final))
          "exclamation final should bump volume"))))

(test statement-final-falls
  "A statement's final word falls below baseline"
  (multiple-value-bind (units sentence-type)
      (analyze-phrase "I don't know.")
    (skyline-tool::compute-envelope units (skyline-tool::default-voice-profile) sentence-type)
    (let ((final (last-content-unit units)))
      (is (eq sentence-type :statement) "sentence-type should be :statement")
      (is (< (skyline-tool::speech-unit-target-pitch final)
             skyline-tool::+character-default-pitch+)
          "statement final word should fall below baseline"))))

(test emphasis-tracks-depth-and-shapes-envelope
  "Emphasized words carry emphasis depth and get pitch/speed/bend/volume deltas"
  (multiple-value-bind (units sentence-type)
      (analyze-phrase "Yes. _Yes_, I said yes.")
    (skyline-tool::compute-envelope units (skyline-tool::default-voice-profile) sentence-type)
    (let ((emph (find-if (lambda (unit)
                           (and (eql (skyline-tool::speech-unit-kind unit) :content)
                                (plusp (skyline-tool::speech-unit-emphasized unit))))
                         units)))
      (is-true emph "an emphasized content unit should exist")
      (when emph
        (is (= 1 (skyline-tool::speech-unit-emphasized emph)) "emphasis depth should be 1")
        (is (> (skyline-tool::speech-unit-target-pitch emph)
               skyline-tool::+character-default-pitch+)
            "emphasized word pitch should rise")
        (is (< (skyline-tool::speech-unit-target-speed emph)
               skyline-tool::+character-default-speed+)
            "emphasized word should slow down")
        (is (> (skyline-tool::speech-unit-target-bend emph)
               skyline-tool::+character-default-bend+)
            "emphasized word should bend more")
        (is (= (+ skyline-tool::+character-default-volume+ skyline-tool::+emphasis-volume-bump+)
               (skyline-tool::speech-unit-target-volume emph))
            "emphasized word should get the volume bump")))))

(test emit-envelope-includes-volume-and-stress
  "emit-envelope emits Volume tokens and Stress on emphasized words"
  (multiple-value-bind (units sentence-type)
      (analyze-phrase "Hey, can I ask you something? Hey, _can_ I ask you something?")
    (skyline-tool::compute-envelope units (skyline-tool::default-voice-profile) sentence-type)
    (phonemize units)
    (let ((output (skyline-tool::emit-envelope units (skyline-tool::default-voice-profile))))
      (is (member "Volume" output :test #'string=) "Volume token should be emitted")
      (is (> (count "Volume" output :test #'string=) 1)
          "Volume should change for emphasis and be restored")
      (is (member "Stress" output :test #'string=) "emphasized word should emit Stress"))))

(test emit-final-pitch-and-sentence-pause
  "emit-envelope emits the final word's target pitch and the sentence Pause3"
  (multiple-value-bind (units sentence-type)
      (analyze-phrase "Hey, can I ask you something?")
    (skyline-tool::compute-envelope units (skyline-tool::default-voice-profile) sentence-type)
    (phonemize units)
    (let* ((output (skyline-tool::emit-envelope units (skyline-tool::default-voice-profile)))
           (final (last-content-unit units))
           (pitch-hex (skyline-tool::hex-byte (skyline-tool::speech-unit-target-pitch final))))
      (is (member pitch-hex output :test #'string=)
          "the final word's target pitch hex should appear in the emitted tokens")
      (is (member "Pause3" output :test #'string=) "the sentence Pause3 should be emitted"))))

(test combine-adjacent-pauses-preserves-sentence-pauses
  "combine-adjacent-pauses keeps inter-sentence Pause3 and merges pause runs"
  (is (equal '("Pause3") (skyline-tool::combine-adjacent-pauses '("Pause4" "Pause3" "Pause4")))
      "Pause3 between sentences must survive")
  (is (equal '("Pause1") (skyline-tool::combine-adjacent-pauses '("Pause6" "Pause4")))
      "Pause6+Pause4 rounds to Pause1")
  (is (equal '("Pause1") (skyline-tool::combine-adjacent-pauses '("Pause6" "Pause4" "Pause3")))
      "a trailing pause is dropped as redundant before EndOfPhrase"))

(test empty-phrase-yields-no-units
  "Empty input produces no speech units"
  (multiple-value-bind (units sentence-type)
      (analyze-phrase "")
    (is (null units) "no units for empty input")
    (is (eq sentence-type :none) "sentence-type :none for empty input")))

(test empty-token-list-yields-no-units
  "analyze-speech of no words produces no units"
  (multiple-value-bind (units sentence-type)
      (skyline-tool::analyze-speech nil)
    (is (null units) "no units for an empty word list")
    (is (eq sentence-type :none) "sentence-type :none for empty input")))
