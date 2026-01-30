(in-package :skyline-tool)

(declaim (optimize (debug 3)))

;; Minimal stub implementations for music compilation functions
;; These are needed for the tests to pass

(defun midi->note-name (midi-note-number)
  "Convert a MIDI note number to a note name like 'C4'"
  (let* ((note-names #("C" "C♯" "D" "D♯" "E" "F" "F♯" "G" "G♯" "A" "A♯" "B"))
         (octave (floor midi-note-number 12))
         (note-index (mod midi-note-number 12)))
    (format nil "~a~d" (aref note-names note-index) (1- octave))))

(defun note->midi-note-number (note-name)
  "Convert a note name like 'C4' to a MIDI note number"
  (let ((note-names #("C" "C♯" "D" "D♯" "E" "F" "F♯" "G" "G♯" "A" "A♯" "B"))
        (note-name (string-upcase note-name)))
    ;; Simple parsing: find the note part and octave part
    (let* ((len (length note-name))
           (note-part (if (and (>= len 2) (char= (char note-name 1) #\♯))
                         (subseq note-name 0 2)
                         (subseq note-name 0 1)))
           (octave-part (subseq note-name (length note-part)))
           (octave (parse-integer octave-part :junk-allowed t))
           (note-index (position note-part note-names :test #'string=)))
      (when (and note-index octave)
        (+ (* octave 12) note-index)))))

;; Stub implementations for music compilation functions
(defmacro define-music-stub (name)
  `(defun ,name (output-file input-file &optional chip)
     (declare (ignore output-file input-file chip))
     (error ,(format nil "~a music compilation not yet implemented" name))))

(define-music-stub compile-music-dmg)
(define-music-stub compile-music-cgb)
(define-music-stub compile-music-nes)
(define-music-stub compile-music-snes)
(define-music-stub compile-music-colecovision)
(define-music-stub compile-music-sg1000)
(define-music-stub compile-music-sms)
(define-music-stub compile-music-sgg)
(define-music-stub compile-music-lynx)
(define-music-stub compile-music-c16)
(define-music-stub compile-music-a2)
(define-music-stub compile-music-a3)
(define-music-stub compile-music-a2gs)
(define-music-stub compile-music-bbc)
(define-music-stub compile-music-zx81)
(define-music-stub compile-music-spectrum)

;; Additional required functions
(defun compile-music (source-out-name in-file-name
                      &optional (machine-type$ "2600")
                                (sound-chip "TIA")
                                (output-coding "NTSC"))
  (declare (ignore source-out-name in-file-name machine-type$ sound-chip output-coding))
  (error "Music compilation not yet implemented"))

(defun compile-music-for-machine (machine sound-chip source-out-name in-file-name output-coding)
  (declare (ignore machine sound-chip source-out-name in-file-name output-coding))
  (error "Music compilation for machine not yet implemented"))

;; Speech compilation stubs
(defun compile-speech-7800 (output-file input-file)
  (declare (ignore output-file input-file))
  (error "7800 speech compilation (SpeakJet) not yet implemented"))

(defun compile-speech-2600 (output-file input-file)
  (declare (ignore output-file input-file))
  (error "2600 speech compilation (SpeakJet) not yet implemented"))

(defun compile-speech-2609 (output-file input-file)
  (declare (ignore output-file input-file))
  (error "2609 speech compilation (IntelliVoice) not yet implemented"))
