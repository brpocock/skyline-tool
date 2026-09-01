(in-package :skyline-tool)

;;;; Intonation envelope engine for SpeakJet speech.
;;;;
;;;; This module derives, at compile time, a per-word pitch/speed/bend
;;;; envelope for a dialogue line, relative to the speaking character's
;;;; baseline voice (see ParseDialogue.s SetUpSpeech).  It implements
;;;; pitch declination, within-clause arcs, nucleus (tonic) peaks,
;;;; emphasis from SI/SO underline controls, phrase-final lengthening,
;;;; and sentence-final contours for statements, questions, and
;;;; exclamations.  All deltas are relative to the character baseline;
;;;; the emitted control bytes are the computed absolute values.

(defconstant +speakjet-pitch-min+ 32
  "Lowest usable SpeakJet pitch in Hz; below 32 Hz is inaudible.")
(defconstant +speakjet-pitch-max+ 240
  "Highest usable SpeakJet pitch in Hz.")
(defconstant +speakjet-speed-min+ 15
  "Slowest usable SpeakJet speed; 15 is the floor.")
(defconstant +speakjet-speed-max+ 127
  "Fastest usable SpeakJet speed.")
(defconstant +speakjet-bend-min+ 0
  "Lowest SpeakJet bend value.")
(defconstant +speakjet-bend-max+ 15
  "Highest SpeakJet bend value.")
(defconstant +speakjet-volume-min+ 0
  "Lowest SpeakJet volume value.")
(defconstant +speakjet-volume-max+ 15
  "Highest SpeakJet volume value.")
(defconstant +character-default-volume+ 12
  "Default character baseline volume, 80% of maximum.")
(defconstant +emphasis-volume-bump+ 4
  "Volume increase per emphasis level on _underlined_ words. (+20% more emphasis)")
(defconstant +exclamation-volume-bump+ 3
  "Volume increase on the final word of an exclamation.")
(defconstant +speakjet-volume-max+ 15
  "Highest SpeakJet volume value (4-bit register).")
(defconstant +character-default-volume+ 12
  "Default character baseline volume, 80% of maximum.")

(defconstant +character-default-pitch+ 90
  "Default character baseline pitch, matching the runtime character tables.")
(defconstant +character-default-speed+ 90
  "Default character baseline speed, matching the runtime character tables.")
(defconstant +character-default-bend+ 5
  "Default character baseline bend, matching the runtime character tables.")

(defconstant +pitch-declination-per-clause+ 2
  "Pitch drop applied to each clause after the first (downstep).")
(defconstant +pitch-swing-amplitude+ 6
  "Height of the within-clause pitch arc, in SpeakJet pitch units.")
(defconstant +nucleus-pitch-bump+ 3
  "Pitch bump on the tonic (nucleus) word of each clause.")
(defconstant +emphasis-pitch-bump+ 5
  "Pitch bump per emphasis level on _underlined_ words. (+20% more emphasis)")
(defconstant +statement-final-fall+ 2
  "Extra pitch fall on the final word of a statement.")
(defconstant +question-final-pitch-rise+ 16  ; Increased for dramatic question rise
  "Pitch rise on the final word of a question.")
(defconstant +exclamation-final-pitch-rise+ 16  ; Increased for dramatic rise
  "Pitch rise on the final word of an exclamation.")

(defconstant +speed-phrase-final-slow+ 3
  "Speed reduction on the final word of each clause (phrase-final lengthening).")
(defconstant +speed-emphasis-slow+ 6
  "Speed reduction on emphasized words (stress lengthening). (+20% more emphasis)")
(defconstant +bend-question-rise+ 4
  "Bend increase on the final word of a question.")
(defconstant +bend-emphasis+ 2
  "Bend increase per emphasis level on _underlined_ words. (+20% more emphasis)")
(defconstant +emphasis-volume-bump+ 4
  "Volume increase per emphasis level on _underlined_ words. (+20% more emphasis)")
(defconstant +exclamation-volume-bump+ 3
  "Volume increase on the final word of an exclamation.")

(defconstant +gender-swing-male+ 0.9
  "Pitch-swing amplitude multiplier for male voices.")
(defconstant +gender-swing-female+ 1.1
  "Pitch-swing amplitude multiplier for female voices.")
(defconstant +gender-swing-nonbinary+ 1.0
  "Pitch-swing amplitude multiplier for nonbinary voices.")

(defconstant +begin-emphasis-char+ #\U+000F
  "SI ($0F): begin-emphasis control, injected from Fountain _underlines_.")
(defconstant +end-emphasis-char+ #\U+000E
  "SO ($0E): end-emphasis control, injected from Fountain _underlines_.")

(defconstant +control-emphasis-minifont+ #xd0
  "Minifont token for ControlEmphasis (see Enums.s).")
(defconstant +control-end-emphasis-minifont+ #xd1
  "Minifont token for ControlEndEmphasis (see Enums.s).")

(defstruct voice-profile
  "Baseline voice parameters for a speaking character."
  pitch speed bend volume gender)

(defun voice-profile-from-actor (actor)
  "Build a VOICE-PROFILE from the ACTOR plist's voice fields.
Missing fields fall back to the runtime character defaults."
  (make-voice-profile
   :pitch (or (getf actor :pitch) +character-default-pitch+)
   :speed (or (getf actor :speed) +character-default-speed+)
   :bend (or (getf actor :bend) +character-default-bend+)
   :volume +character-default-volume+
   :gender (or (getf actor :gender) "N")))

(defun default-voice-profile ()
  "Return the fallback VOICE-PROFILE used when no speaker is known."
  (make-voice-profile
   :pitch +character-default-pitch+
   :speed +character-default-speed+
   :bend +character-default-bend+
   :volume +character-default-volume+
   :gender "N"))

(defun gender-swing (voice)
  "Return the pitch-swing multiplier for the gender in VOICE."
  (let ((g (string-upcase (string (or (voice-profile-gender voice) "N")))))
    (cond
      ((and (plusp (length g)) (char-equal (char g 0) #\M)) +gender-swing-male+)
      ((and (plusp (length g)) (char-equal (char g 0) #\F)) +gender-swing-female+)
      (t +gender-swing-nonbinary+))))

(defun emphasize-text (string)
  "Convert Fountain _underline_ spans in STRING into SI/SO emphasis controls.
  The opening underscore becomes #\\U+000F (begin emphasis) and the closing
  underscore becomes #\\U+000E (end emphasis).  Idempotent."
  (let ((out (make-string-output-stream))
        (last 0))
    (cl-ppcre:do-scans (match-start match-end reg-starts reg-ends
                        "_([^_]*)_" string)
      (write-string string out :start last :end match-start)
      (write-char +begin-emphasis-char+ out)
      (write-string string out :start (aref reg-starts 0) :end (aref reg-ends 0))
      (write-char +end-emphasis-char+ out)
      (setf last match-end))
    (write-string string out :start last)
    (get-output-stream-string out)))

(defun strip-emphasis (string)
  "Remove SI/SO emphasis controls from STRING, returning plain text."
  (remove-if (lambda (char)
               (or (char= char +begin-emphasis-char+)
                   (char= char +end-emphasis-char+)))
             string))

(defstruct speech-unit
  "A single analyzed token of a dialogue line."
  text
  kind                          ; :content :pause :control :ignored
  emphasized                    ; emphasis depth (0 = none)
  (clause 0)                     ; clause index for content units
  (phrase 0)                     ; phrase index for content units
  (position 0)                   ; position among content units in the clause
  (phonetic-position 0)          ; position among phonemes in the phrase
  (content-count 0)              ; number of content units in the clause
  (clause-drop 0)                ; accumulated pitch drop for the clause
  (phonetic-count 0)             ; number of phonemes in the phrase
  phonemes
  target-pitch
  target-speed
  target-bend
  target-volume)

(defun control-tokens (word)
  "Translate the \\-prefixed escape WORD into raw SpeakJet tokens."
  (if (every #'digit-char-p (subseq word 1))
      (list (format nil "$~2,'0x" (parse-number (subseq word 1))))
      (list (subseq word 1))))

(defun split-emphasis-tokens (words)
  "Split any WORD in WORDS that embeds an emphasis control into separate tokens."
  (loop for word in words
        append (if (find-if (lambda (c)
                              (or (char= c +begin-emphasis-char+)
                                  (char= c +end-emphasis-char+)))
                            word)
                   (split-word-on-emphasis word)
                   (list word))))

(defun split-word-on-emphasis (word)
  "Split WORD into a list of tokens, isolating each embedded emphasis control."
  (loop with current = (make-string-output-stream)
        with result = nil
        for c across word
        if (or (char= c +begin-emphasis-char+)
               (char= c +end-emphasis-char+))
          do (let ((part (get-output-stream-string current)))
               (unless (emptyp part) (push part result))
               (push (string c) result))
        else
          do (write-char c current)
        finally (let ((part (get-output-stream-string current)))
                  (unless (emptyp part) (push part result)))
                (return (nreverse result))))

(defun clause-boundary-text-p (word)
  "True when WORD is punctuation that ends or breaks a clause."
  (and (not (emptyp word))
       (find-if (lambda (c) (find c ".:;—…?!—“”")) word)))

(defun terminal-word-p (word)
  "True when WORD is sentence-final punctuation."
  (member word '("?" "!" "?!" "." "…") :test #'string=))

(defun clause-pause-code (word)
  "Return the SpeakJet pause code for the clause punctuation WORD.
Values: Pause4=30ms (word boundary), Pause1=100ms (clause boundary), Pause2=200ms (comma), Pause3=700ms (sentence-final)"
  (cond
    ((member word '(":" ";" "—" "---") :test #'string=) 1)
    (t 2)))

(defun clause-drop-for-boundary (word)
  "Return the pitch drop amount for a clause boundary caused by WORD."
  (cond
    ((terminal-word-p word) +pitch-declination-per-clause+)
    ((member word '(":" ";" "—" "---") :test #'string=)
     (round (* 2/3 +pitch-declination-per-clause+)))
    (t 0)))

(defun analyze-speech (words)
  "Analyze tokenized WORDS into SPEECH-UNITs and a sentence terminal type.
Returns (values units sentence-type).  Emphasis is tracked from SI/SO
controls; clause and phrase indices and positions are assigned to content units."
  (let ((units (mapcar (lambda (word) (make-speech-unit :text word))
                       (split-emphasis-tokens words)))
        (emphasis 0)
        (sentence-type :none)
        (phrase 0)
        (clause 0)
        (clause-drop 0)
        (phrase-counts (make-hash-table)))
(dolist (unit units)
       (let ((word (speech-unit-text unit)))
         (cond
           ((string= word (string +begin-emphasis-char+))
            (incf emphasis)
            (setf (speech-unit-kind unit) :ignored))
           ((string= word (string +end-emphasis-char+))
            (setf emphasis (max 0 (1- emphasis)))
            (setf (speech-unit-kind unit) :ignored))
           ((emptyp word)
            (setf (speech-unit-kind unit) :ignored))
           ((char= #\\ (char word 0))
            (setf (speech-unit-kind unit) :control
                  (speech-unit-phonemes unit) (control-tokens word)))
           ((string= word "-")
            (setf (speech-unit-kind unit) :ignored))
           ((member word '("“" "”") :test #'string=)
            (setf (speech-unit-kind unit) :ignored))
           ((string= word "?")
            (setf (speech-unit-kind unit) :pause
                  (speech-unit-phonemes unit) (list "Pause3")
                  sentence-type :question))
           ((string= word "!")
            (setf (speech-unit-kind unit) :pause
                  (speech-unit-phonemes unit) (list "Pause2")
                  sentence-type :exclamation))
           ((member word '("?!" "!?" "‽") :test #'string=)
            (setf (speech-unit-kind unit) :pause
                  (speech-unit-phonemes unit) (list "Pause3")
                  sentence-type :exclamation-question))
           ((string= word ".")
            (setf (speech-unit-kind unit) :pause
                  (speech-unit-phonemes unit) (list "Pause1")
                  sentence-type :statement))
           ((string= word "")
            (setf (speech-unit-kind unit) :pause
                  (speech-unit-phonemes unit) (list "Pause1")
                  sentence-type :statement
                  phrase (1+ phrase)))
           ((member word '("," ";" ":" "---") :test #'string=)
            (setf (speech-unit-kind unit) :pause
                  (speech-unit-phonemes unit)
                  (list "Pause6")))
           (t
            (setf (speech-unit-kind unit) :content
                  (speech-unit-emphasized unit) emphasis)))))
     (let ((position 0)
           (content-in-phrase 0)
           (content-in-clause 0))
       (dolist (unit units)
         (cond
           ((and (eql (speech-unit-kind unit) :pause)
                 (clause-boundary-text-p (speech-unit-text unit)))
            (let ((word (speech-unit-text unit)))
              (when (member word '(":" ";" "" "---") :test #'string=)
                (incf clause)
                (incf clause-drop (clause-drop-for-boundary word)))
              (unless (string= word ",")
                (incf clause)
                (incf clause-drop (clause-drop-for-boundary word))
                (when (terminal-word-p word)
                  (incf phrase))
                (setf position 0
                      content-in-clause 0
                      content-in-phrase 0))))
           ((eql (speech-unit-kind unit) :content)
            (setf (speech-unit-phrase unit) phrase
                  (speech-unit-clause unit) clause
                  (speech-unit-clause-drop unit) clause-drop
                  (speech-unit-position unit) position
                  (speech-unit-phonetic-position unit) content-in-phrase)
            (let* ((new-count (incf (gethash phrase phrase-counts 0))))
              (setf (speech-unit-phonetic-count unit) new-count
                    (speech-unit-content-count unit) (incf content-in-clause)))
            (incf position)
            (incf content-in-phrase)))))
     (values units sentence-type)))

(defun group-by-clause (content)
  "Partition the CONTENT units into clauses, preserving order."
  (loop for unit in content
        with groups = nil
        for clause = (speech-unit-clause unit)
        if (and groups (= clause (speech-unit-clause (car (car groups)))))
          do (push unit (car groups))
        else
          do (push (list unit) groups)
        finally (return (nreverse (mapcar #'nreverse groups)))))

(defun clause-nucleus (clause-units)
  "Return the index within CLAUSE-UNITS of the tonic (nucleus) word.
Prefers the last word carrying a Stress marker; otherwise the penultimate."
  (or (position-if (lambda (unit) (find "Stress" (speech-unit-phonemes unit)
                                        :test #'string=))
                   clause-units :from-end t)
      (if (< 1 (length clause-units))
          (- (length clause-units) 2)
          0)))

(defun arc-delta (position count nucleus amplitude)
  "Within-clause pitch arc for the word at POSITION in a clause of COUNT words."
  (cond
    ((null count) 0)
    ((zerop count) 0)
    ((<= count 1) 0)
    ((= position nucleus) amplitude)
    ((< position nucleus)
     (round (* amplitude (/ (max 0 position) (max 1 nucleus)))))
    (t
     (round (* amplitude
                 (- 1 (/ (- position nucleus)
                         (max 1 (max 1 (- count nucleus 1))))))))))

(defun pitch-delta (unit clause-nucleus-index final-p sentence-type)
  "Relative pitch delta for the content UNIT within its clause."
  (let* ((count (speech-unit-content-count unit))
         (position (speech-unit-position unit))
         (cl (speech-unit-clause unit))
         (delta (if (zerop count)
                     0
                     (+ (- (* cl +pitch-declination-per-clause+))
                        (arc-delta position count clause-nucleus-index
                                   +pitch-swing-amplitude+)))))
    (when (and (plusp count) (= position clause-nucleus-index))
      (incf delta +nucleus-pitch-bump+))
    (incf delta (* (speech-unit-emphasized unit) +emphasis-pitch-bump+))
    (when final-p
      (ecase sentence-type
        (:statement (decf delta (statement-fall-distributed unit)))
        (:question (incf delta +question-final-pitch-rise+))
        (:exclamation (incf delta +exclamation-final-pitch-rise+))
        (:exclamation-question
         (incf delta (+ +question-final-pitch-rise+ +exclamation-final-pitch-rise+)))
        (:none nil)))
    delta))

(defun question-rise-distributed (unit)
  "Exponentially-weighted distribution with strong final rise"
  (let* ((count (max 1 (speech-unit-content-count unit)))
         (position (speech-unit-position unit))
         (progress (/ (float position) count))
         (boost-factor 2.5)
         (fraction (expt progress boost-factor)))
    (round (* fraction +question-final-pitch-rise+))))

(defun statement-fall-distributed (unit)
  "Exponentially-weighted statement fall with dramatic ending"
  (let* ((count (max 1 (speech-unit-content-count unit)))
         (position (speech-unit-position unit))
         (progress (/ (float position) count))
         (boost-factor 2.5)
         (fraction (expt progress boost-factor)))
    (round (* fraction +statement-final-fall+))))

(defun exclamation-rise-distributed (unit)
  "Exponentially-weighted exclamation rise with dramatic ending"
  (let* ((count (max 1 (speech-unit-content-count unit)))
         (position (speech-unit-position unit))
         (progress (/ (float position) count))
         (boost-factor 2.5)
         (fraction (expt progress boost-factor)))
    (round (* fraction +exclamation-final-pitch-rise+))))

(defun bend-question-rise-distributed (unit sentence-type)
  "Exponentially-weighted bend rise for questions/exclamations"
  (declare (ignore sentence-type))
  (let* ((count (max 1 (speech-unit-content-count unit)))
         (position (speech-unit-position unit))
         (progress (/ (float position) count))
         (boost-factor 2.5)
         (fraction (expt progress boost-factor)))
    (round (* fraction +bend-question-rise+))))
(defun speed-delta (unit final-p)
  "Relative speed delta for the content UNIT (slower = lower)."
  (let ((delta 0)
        (position (speech-unit-position unit))
        (count (speech-unit-content-count unit)))
    (when (= position (1- count))
      (decf delta +speed-phrase-final-slow+))
    (when (and (< 1 count) (= position (- count 2)))
      (decf delta (floor +speed-phrase-final-slow+ 2)))
    (when (plusp (speech-unit-emphasized unit))
      (decf delta (* (min 1 (speech-unit-emphasized unit)) +speed-emphasis-slow+)))
    (when final-p
      (decf delta (floor +speed-phrase-final-slow+ 2)))
    delta))

(defun bend-delta (unit final-p sentence-type)
  "Relative bend delta for the content UNIT.
Bend changes are capped at 1 unit per step."
  (let ((delta 0))
    (when (plusp (speech-unit-emphasized unit))
      (incf delta (min 1 (* (min 1 (speech-unit-emphasized unit))
                            +bend-emphasis+))))
    (when (and final-p (member sentence-type '(:question :exclamation-question)))
      (incf delta (min 1 (bend-question-rise-distributed unit sentence-type))))
    delta))

(defun clamp-speakjet-param (min max value)
  "Clamp VALUE to the inclusive MIN..MAX range."
  (max min (min max value)))

(defun compute-envelope (units voice sentence-type)
  "Compute pitch/speed/bend/volume targets for the content units in UNITS.
Targets are absolute SpeakJet values: character baseline plus the relative
deltas, clamped to the SpeakJet ranges."
  (let ((swing (gender-swing voice))
        (base-volume (voice-profile-volume voice))
        (content (remove-if-not (lambda (unit) (eql (speech-unit-kind unit) :content))
                                units))
        (phrase-finals
          (loop for unit in (remove-if-not (lambda (unit)
                                             (eql (speech-unit-kind unit) :content))
                                           units)
                with finals = (make-hash-table)
                do (setf (gethash (speech-unit-phrase unit) finals) unit)
                finally (return finals))))
    (dolist (clause-units (group-by-clause content))
      (let ((nucleus (clause-nucleus clause-units))
            (emphasis-carry 0))
        (dolist (unit clause-units)
          (let ((final-p (eq unit (gethash (speech-unit-phrase unit) phrase-finals))))
            (when (plusp (speech-unit-emphasized unit))
              (setf emphasis-carry (speech-unit-emphasized unit)))
            (setf (speech-unit-target-pitch unit)
                  (clamp-speakjet-param +speakjet-pitch-min+ +speakjet-pitch-max+
                                        (+ (voice-profile-pitch voice)
                                           (round (* swing (pitch-delta unit nucleus
                                                                         final-p sentence-type)))
                                           (if (plusp emphasis-carry)
                                               (+ +emphasis-pitch-bump+
                                                  (* (- emphasis-carry) +emphasis-pitch-bump+))
                                               0)))
                  (speech-unit-target-speed unit)
                  (clamp-speakjet-param +speakjet-speed-min+ +speakjet-speed-max+
                                        (+ (voice-profile-speed voice)
                                           (speed-delta unit final-p)
                                           (if (plusp emphasis-carry)
                                               (- (* emphasis-carry +speed-emphasis-slow+))
                                               0)))
                  (speech-unit-target-bend unit)
                  (clamp-speakjet-param +speakjet-bend-min+ +speakjet-bend-max+
                                        (+ (voice-profile-bend voice)
                                           (bend-delta unit final-p sentence-type)
                                           (if (plusp emphasis-carry)
                                               (* emphasis-carry +bend-emphasis+)
                                               0)))
                  (speech-unit-target-volume unit)
                  (clamp-speakjet-param +speakjet-volume-min+ +speakjet-volume-max+
                                        (+ base-volume
                                           (volume-delta unit final-p sentence-type)
                                           (if (plusp emphasis-carry)
                                               (* emphasis-carry +emphasis-volume-bump+)
                                               0))))
            (when (plusp emphasis-carry)
              (decf emphasis-carry))))))
    units))

(defun volume-delta (unit final-p sentence-type)
  "Relative volume delta for the content UNIT."
  (declare (ignore unit))
  (let ((delta 0))
    (when final-p
      (ecase sentence-type
        ((:statement :exclamation-question) (decf delta +exclamation-volume-bump+))
        (:question (incf delta +exclamation-volume-bump+))
        (:exclamation (incf delta +exclamation-volume-bump+))
        ((:none) 0)))
    delta))

(defun hex-byte (value)
  "Format VALUE as a SpeakJet $-prefixed hex byte token."
  (format nil "$~2,'0x" value))

(defun emphasized-phonemes (phonemes)
  "Return PHONEMES with an extra Stress marker on the primary stressed syllable."
  (let ((stress (position "Stress" phonemes :from-end t)))
    (if stress
        (append (subseq phonemes 0 (1+ stress))
                (list "Stress")
                (subseq phonemes (1+ stress)))
        (cons "Stress" phonemes))))

(defun emit-envelope (units voice)
  "Emit SpeakJet tokens for UNITS under the VOICE baseline.
The phrase begins at the first content unit's targets, so the initial
control bytes are not immediately overridden.  Control codes are emitted
only when a parameter changes, to conserve the SpeechBuffer.  Emphasized
words receive an extra Stress marker."
  (let* ((first-content (find-if (lambda (unit)
                                   (and (eql (speech-unit-kind unit) :content)
                                        (speech-unit-phonemes unit)))
                                 units))
         (pitch (or (and first-content (speech-unit-target-pitch first-content))
                    (voice-profile-pitch voice)))
         (speed (or (and first-content (speech-unit-target-speed first-content))
                    (voice-profile-speed voice)))
         (bend (or (and first-content (speech-unit-target-bend first-content))
                    (voice-profile-bend voice)))
         (volume (or (and first-content (speech-unit-target-volume first-content))
                     (voice-profile-volume voice)))
         (output (nreverse (list "Speed" (hex-byte speed)
                                 "Pitch" (hex-byte pitch)
                                 "Bend" (hex-byte bend)
                                 "Volume" (hex-byte volume)))))
    (flet ((emit (&rest tokens)
             (dolist (token tokens)
               (push token output))))
      (dolist (unit units)
        (unless unit (break "Nil unit in ~s" units))
        (ecase (speech-unit-kind unit)
          (:ignored nil)
          (:pause
           (dolist (phoneme (speech-unit-phonemes unit))
             (emit phoneme)))
          (:control
           (dolist (token (speech-unit-phonemes unit))
             (emit token)))
          (:content
           (when (speech-unit-phonemes unit)
             (let ((tp (speech-unit-target-pitch unit))
                   (ts (speech-unit-target-speed unit))
                   (tb (speech-unit-target-bend unit))
                   (tv (speech-unit-target-volume unit)))
               (when (and tp (not (= tp pitch)))
                 (setf pitch tp)
                 (emit "Pitch" (hex-byte tp)))
               (when (and ts (not (= ts speed)))
                 (setf speed ts)
                 (emit "Speed" (hex-byte ts)))
               (when (and tb (not (= tb bend)))
                 (setf bend tb)
                 (emit "Bend" (hex-byte tb)))
               (when (and tv (not (= tv volume)))
                 (setf volume tv)
                 (emit "Volume" (hex-byte tv)))
               (dolist (phoneme (if (plusp (speech-unit-emphasized unit))
                                    (emphasized-phonemes (speech-unit-phonemes unit))
                                    (speech-unit-phonemes unit)))
                 (emit phoneme)))))))
      (nreverse output))))

(defun speakjet-convert (words lookup voice)
  "Convert tokenized WORDS into SpeakJet tokens.
LOOKUP maps a word (string) to its phoneme token list, or nil when the word
produces no sound.  VOICE supplies the character baseline for the envelope."
  (multiple-value-bind (units sentence-type)
      (analyze-speech words)
    (dolist (unit units)
      (when (and (null (speech-unit-phonemes unit))
                 (member (speech-unit-kind unit) '(:content :pause)))
        (setf (speech-unit-phonemes unit)
              (funcall lookup (speech-unit-text unit)))))
    (compute-envelope units voice sentence-type)
    (emit-envelope units voice)))
