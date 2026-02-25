;;; SkylineTool/tests/test-data-generators.lisp
;;; Comprehensive test data generators for property-based testing

(defpackage :skyline-tool/test-generators
  (:use :cl)
  (:export
   ;; Pixel data generators
   #:generate-random-pixels
   #:generate-edge-case-pixels
   #:generate-valid-tile-pixels
   #:generate-invalid-color-pixels

   ;; Dimension generators
   #:generate-valid-tile-dimensions
   #:generate-invalid-tile-dimensions
   #:generate-random-dimensions
   #:generate-extreme-dimensions

   ;; Audio data generators
   #:generate-valid-psg-frequency
   #:generate-valid-psg-volume
   #:generate-psg-note-sequence
   #:generate-psg-command-sequence
   #:generate-invalid-psg-commands

   ;; File/path generators
   #:generate-valid-file-paths
   #:generate-invalid-file-paths
   #:generate-temp-file-path

   ;; MIDI/data generators
   #:generate-mock-midi-data
   #:generate-music-note-sequence
   #:generate-invalid-music-data

   ;; Generic utilities
   #:with-temp-file
   #:with-temp-files
   #:time-execution
   #:measure-memory-usage))

(in-package :skyline-tool/test-generators)

;;; Pixel Data Generators

(defun generate-random-pixels (width height &optional (max-color 255))
  "Generate random pixel data for testing"
  (let ((pixels (make-array (list width height) :element-type '(unsigned-byte 8))))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref pixels x y) (random (1+ max-color)))))
    pixels))

(defun generate-edge-case-pixels (width height)
  "Generate pixel data with edge case color values"
  (let ((pixels (make-array (list width height) :element-type '(unsigned-byte 8))))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref pixels x y)
              (case (random 5)
                (0 0)     ; Minimum valid
                (1 1)     ; Valid color
                (2 127)   ; Mid-range
                (3 254)   ; Maximum-1
                (4 255))))) ; Maximum possible
    pixels))

(defun generate-valid-tile-pixels (tile-width tile-height &optional (max-color 3))
  "Generate pixel data valid for tile conversion (multiples of 8)"
  (generate-random-pixels (* 8 tile-width) (* 8 tile-height) max-color))

(defun generate-invalid-color-pixels (width height &optional (invalid-color 999))
  "Generate pixel data with invalid color values"
  (let ((pixels (make-array (list width height) :element-type '(unsigned-byte 8))))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref pixels x y) invalid-color)))
    pixels))

;;; Dimension Generators

(defun generate-valid-tile-dimensions ()
  "Generate valid tile dimensions (multiples of 8)"
  (let ((size (* 8 (+ 1 (random 8))))) ; 8, 16, 24, ..., 64
    (list size size)))

(defun generate-invalid-tile-dimensions ()
  "Generate invalid tile dimensions (not multiples of 8)"
  (let* ((base (* 8 (+ 1 (random 8))))
         (offset (+ 1 (random 7)))) ; 1-7
    (list (+ base offset) (+ base offset))))

(defun generate-random-dimensions (&optional (max-size 1024))
  "Generate random dimensions within reasonable bounds"
  (let ((width (+ 1 (random max-size)))
        (height (+ 1 (random max-size))))
    (list width height)))

(defun generate-extreme-dimensions ()
  "Generate extreme dimension values for stress testing"
  (case (random 4)
    (0 (list 1 1))           ; Minimum
    (1 (list 8192 8192))     ; Very large
    (2 (list 1000000 1))     ; Extremely wide
    (3 (list 1 1000000))))   ; Extremely tall

;;; Audio Data Generators

(defun generate-valid-psg-frequency (&optional (system :sms))
  "Generate a valid PSG frequency value"
  (case system
    (:sms (random 1024))     ; 10-bit for SMS/SG-1000
    (:nes (random 2048))     ; 11-bit for NES
    (:gb (random 2048))      ; 11-bit for Game Boy
    (:tg16 (random 4096))    ; 12-bit for TG16
    (t (random 1024))))

(defun generate-valid-psg-volume (&optional (system :sms))
  "Generate a valid PSG volume value"
  (case system
    ((:sms :gb :nes) (random 16))  ; 4-bit
    (:tg16 (random 32))            ; 5-bit
    (t (random 16))))

(defun generate-psg-note-sequence (length &optional (system :sms))
  "Generate a sequence of PSG notes for testing"
  (loop for i from 1 to length
        collect (list (random 1000)    ; time
                      (random 4)       ; channel (0-3)
                      (generate-valid-psg-frequency system) ; freq low
                      (ash (generate-valid-psg-frequency system) -8) ; freq high
                      (generate-valid-psg-volume system) ; volume
                      (+ 10 (random 500))))) ; duration

(defun generate-psg-command-sequence (length)
  "Generate a sequence of raw PSG commands"
  (loop for i from 1 to length
        collect (random 256))) ; Raw command byte

(defun generate-invalid-psg-commands (length)
  "Generate invalid PSG commands for error testing"
  (loop for i from 1 to length
        collect (case (random 3)
                  (0 -1)     ; Negative
                  (1 256)    ; Too large
                  (2 'invalid)))) ; Wrong type

;;; File/Path Generators

(defun generate-valid-file-paths (count)
  "Generate valid file paths for testing"
  (let ((base-paths '("/tmp/" "/var/tmp/" "./" "../")))
    (loop for i from 1 to count
          collect (format nil "~a~a-~a.~a"
                          (nth (random (length base-paths)) base-paths)
                          (nth (random 5) '("test" "data" "file" "temp" "work"))
                          (random 10000)
                          (nth (random 4) '("bin" "dat" "tmp" "out"))))))

(defun generate-invalid-file-paths (count)
  "Generate invalid file paths for error testing"
  (let ((invalid-paths '("/dev/null/invalid" "/root/invalid" "/etc/passwd"
                         "invalid:path" "invalid<>path" "" nil)))
    (loop for i from 1 to count
          collect (nth (random (length invalid-paths)) invalid-paths))))

(defun generate-temp-file-path (&optional (prefix "test") (suffix "tmp"))
  "Generate a unique temporary file path"
  (format nil "/tmp/~a-~a.~a" prefix (get-universal-time) suffix))

;;; MIDI/Data Generators

(defun generate-mock-midi-data (&optional (num-tracks 1) (num-notes 10))
  "Generate mock MIDI data for testing"
  (let ((midi-data (make-hash-table)))
    (dotimes (track num-tracks)
      (let ((track-data (loop for i from 1 to num-notes
                             collect (list (+ (* i 100) (random 50))  ; time
                                           (+ 60 (random 24))        ; note (C4-C6)
                                           (+ 50 (random 100))       ; velocity
                                           (+ 100 (random 400))))))  ; duration
        (setf (gethash track midi-data) track-data)))
    midi-data))

(defun generate-music-note-sequence (length &optional (system :general))
  "Generate a musical note sequence"
  (let ((notes '(60 62 64 65 67 69 71 72))) ; C major scale
    (loop for i from 1 to length
          collect (list (* i 100)                    ; time
                        (nth (random (length notes)) notes) ; note
                        (+ 50 (random 77))          ; velocity 50-127
                        (+ 200 (random 800))))))   ; duration

(defun generate-invalid-music-data (length)
  "Generate invalid music data for error testing"
  (loop for i from 1 to length
        collect (case (random 4)
                  (0 (list -1 60 100 400))     ; Negative time
                  (1 (list 100 128 100 400))   ; Invalid note (>127)
                  (2 (list 100 60 -1 400))     ; Negative velocity
                  (3 (list 100 60 100 0)))))   ; Zero duration

;;; Utility Macros and Functions

(defmacro with-temp-file ((var &optional (prefix "test") (suffix "tmp")) &body body)
  "Execute body with a temporary file that gets cleaned up"
  `(let ((,var (generate-temp-file-path ,prefix ,suffix)))
     (unwind-protect
         (progn ,@body)
       (when (probe-file ,var)
         (delete-file ,var)))))

(defmacro with-temp-files ((vars &optional (prefix "test") (suffix "tmp")) &body body)
  "Execute body with multiple temporary files that get cleaned up"
  (if (null vars)
      `(progn ,@body)
      `(let ((,(first vars) (generate-temp-file-path ,prefix ,suffix)))
         (unwind-protect
             (with-temp-files (,(rest vars) ,prefix ,suffix)
               ,@body)
           (when (probe-file ,(first vars))
             (delete-file ,(first vars)))))))

(defun time-execution (thunk &optional (iterations 1))
  "Time the execution of a thunk, optionally running multiple iterations"
  (let ((start-time (get-internal-real-time))
        (results '()))
    (dotimes (i iterations)
      (push (funcall thunk) results))
    (let ((end-time (get-internal-real-time)))
      (values results
              (/ (- end-time start-time) internal-time-units-per-second)
              (/ (- end-time start-time) internal-time-units-per-second iterations)))))

(defun measure-memory-usage (thunk)
  "Measure memory usage of a thunk (simplified)"
  ;; This is a placeholder - in a real implementation you'd use
  ;; platform-specific memory measurement functions
  (let ((start-bytes 0) ; Would get actual memory usage
        (result (funcall thunk))
        (end-bytes 0)) ; Would get actual memory usage
    (values result (- end-bytes start-bytes))))

;;; All functions are exported in the defpackage above

;;; Advanced Test Data Generators

(defun generate-correlated-pixel-data (width height &key (correlation-factor 0.8))
  "Generate pixel data with spatial correlation (more realistic)"
  (let ((pixels (make-array (list width height) :element-type '(unsigned-byte 8))))
    ;; Start with random seed values
    (dotimes (x width)
      (dotimes (y height)
        (if (and (> x 0) (> y 0))
            ;; Correlated: blend with neighbors
            (let ((left (aref pixels (1- x) y))
                  (above (aref pixels x (1- y)))
                  (diag (aref pixels (1- x) (1- y))))
              (setf (aref pixels x y)
                    (round (+ (* correlation-factor (+ left above diag) 1/3)
                             (* (- 1 correlation-factor) (random 256))))))
            ;; Random for first row/column
            (setf (aref pixels x y) (random 256)))))
    pixels))

(defun generate-gradient-pixel-data (width height &key (start-color 0) (end-color 255) (direction :horizontal))
  "Generate gradient pixel data for testing"
  (let ((pixels (make-array (list width height) :element-type '(unsigned-byte 8))))
    (dotimes (x width)
      (dotimes (y height)
        (let ((factor (ecase direction
                        (:horizontal (/ x (max 1 (1- width))))
                        (:vertical (/ y (max 1 (1- height))))
                        (:diagonal (/ (+ x y) (max 1 (+ (1- width) (1- height))))))))
          (setf (aref pixels x y)
                (round (+ start-color (* factor (- end-color start-color))))))))
    pixels))

(defun generate-noise-pixel-data (width height &key (noise-type :white) (intensity 0.5))
  "Generate various types of noise patterns"
  (let ((pixels (make-array (list width height) :element-type '(unsigned-byte 8)))
        (base-value 128)) ; Mid-range base
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref pixels x y)
              (round (ecase noise-type
                      (:white (random 256))
                      (:gaussian (max 0 (min 255 (+ base-value (* intensity 50 (random-gaussian)))))
                      (:perlin (perlin-noise x y)) ; Simplified Perlin-like
                      (:salt-pepper (if (> (random 1.0) 0.95)
                                       (if (zerop (random 2)) 0 255)
                                       base-value))))))))
    pixels))

(defun random-gaussian ()
  "Generate a random value from a Gaussian distribution (Box-Muller transform)"
  (let ((u1 (random 1.0))
        (u2 (random 1.0)))
    (* (sqrt (* -2 (log u1))) (cos (* 2 pi u2)))))

(defun perlin-noise (x y)
  "Simplified Perlin noise implementation"
  ;; This is a very basic implementation - real Perlin would be more complex
  (let ((n (sin (+ (* x 0.1) (* y 0.1)))))
    (* 128 (+ 1 n))))

(defun generate-structured-pixel-data (width height &key (pattern :chessboard) (colors '(0 255)))
  "Generate structured patterns for testing"
  (let ((pixels (make-array (list width height) :element-type '(unsigned-byte 8))))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref pixels x y)
              (ecase pattern
                (:chessboard (nth (mod (+ x y) (length colors)) colors))
                (:stripes-h (nth (mod y (length colors)) colors))
                (:stripes-v (nth (mod x (length colors)) colors))
                (:diagonal (nth (mod (+ x y) (length colors)) colors))
                (:blocks (let ((block-size 8))
                          (nth (mod (+ (floor x block-size) (floor y block-size)) (length colors)) colors)))))))
    pixels))

;;; Advanced MIDI Generators

(defun generate-complex-midi-data (&key (num-tracks 3) (duration 1000) (tempo-variation t))
  "Generate complex MIDI data with multiple tracks, tempo changes, and dynamics"
  (let ((midi-data (make-hash-table))
        (base-tempo 120))
    (dotimes (track num-tracks)
      (let* ((tempo (if tempo-variation
                       (* base-tempo (+ 0.5 (* 0.5 (random 1.0))))
                       base-tempo))
             (notes-per-track (+ 10 (random 20)))
             (track-data '()))
        (dotimes (i notes-per-track)
          (let* ((time (* i (/ duration notes-per-track)))
                 (note (+ 48 (random 24))) ; C3-C5 range
                 (velocity (+ 50 (random 78))) ; 50-127
                 (note-duration (* tempo (+ 100 (random 400)))))
            (push (list time note velocity note-duration) track-data)))
        (setf (gethash track midi-data) (reverse track-data))))
    midi-data))

(defun generate-polyrhythmic-midi-data (&key (num-voices 4) (total-duration 2000))
  "Generate MIDI data with polyrhythms and complex timing"
  (let ((midi-data (make-hash-table)))
    (dotimes (voice num-voices)
      (let* ((rhythm-pattern (nth voice '((4 4 4 4) (3 3 3 2 2) (6 6 6) (8 8))))
             (voice-data '())
             (current-time 0))
        (dolist (duration rhythm-pattern)
          (let ((note (+ 60 (* voice 12) (random 12)))) ; Different octaves
            (push (list current-time note (+ 70 (random 30)) (* duration 50)) voice-data)
            (incf current-time (* duration 50))
            (when (> current-time total-duration)
              (return))))
        (setf (gethash voice midi-data) (reverse voice-data))))
    midi-data))

;;; Advanced Audio Generators

(defun generate-psg-song-data (&key (system :sms) (num-channels 4) (length 100))
  "Generate a complete PSG song sequence"
  (let ((song-data '()))
    (dotimes (time length)
      (let ((frame-data '()))
        (dotimes (channel num-channels)
          (push (list channel
                     (generate-valid-psg-frequency system)
                     (generate-valid-psg-volume system)
                     (if (zerop (random 10)) 0 1)) ; Occasional rests
                frame-data))
        (push (cons time (reverse frame-data)) song-data)))
    (reverse song-data)))

(defun generate-fm-sound-data (&key (num-operators 4) (algorithm 1))
  "Generate FM synthesis sound data (for systems that support it)"
  (let ((operators '()))
    (dotimes (i num-operators)
      (push (list :frequency (random 1000)
                  :amplitude (random 128)
                  :attack (random 100)
                  :decay (random 100)
                  :sustain (random 128)
                  :release (random 200)
                  :waveform (nth (random 4) '(:sine :square :triangle :sawtooth)))
            operators))
    (list :algorithm algorithm
          :operators (reverse operators))))

;;; Fuzz Testing Generators

(defun generate-malformed-pixel-data (width height)
  "Generate pixel data that might cause issues in converters"
  (let ((pixels (make-array (list width height) :element-type '(unsigned-byte 8))))
    (dotimes (x width)
      (dotimes (y height)
        (setf (aref pixels x y)
              (case (random 10)
                (0 -1)        ; Negative (if allowed)
                (1 256)       ; Out of range
                (2 'invalid)  ; Wrong type
                (3 nil)       ; Nil
                (otherwise (random 256))))))
    pixels))

(defun generate-malformed-file-content ()
  "Generate malformed file content for testing parsers"
  (let ((content-types '("valid content"
                        "" ; Empty
                        (make-string 100000 :initial-element #\X) ; Very large
                        "content with null\0bytes"
                        "unicode: αβγδε"
                        "control chars: ~c~c~c" ; With control chars
                        nil ; No content
                        :not-a-string))) ; Wrong type
    (nth (random (length content-types)) content-types)))

(defun generate-corrupted-midi-data ()
  "Generate MIDI data with corruption for testing robustness"
  (let ((midi-data (generate-mock-midi-data)))
    ;; Introduce corruption
    (case (random 5)
      (0 (clrhash midi-data)) ; Empty hash
      (1 (let ((track-data (gethash 0 midi-data)))
          (when track-data
            (setf (gethash 0 midi-data) (append track-data '(:corrupted)))))) ; Bad data
      (2 (setf (gethash 'invalid-key midi-data) "bad value")) ; Bad key
      (3 midi-data) ; Leave intact
      (4 nil)) ; Return nil
    midi-data))

;;; Performance Testing Utilities

(defmacro with-performance-measurement ((&key (iterations 10) (report t)) &body body)
  "Measure performance of code execution"
  `(let ((times '())
         (memory-usage '()))
     (dotimes (i ,iterations)
       (let ((start-time (get-internal-real-time))
             (start-memory (room)) ; Simplified
             (result (progn ,@body))
             (end-time (get-internal-real-time))
             (end-memory (room)))
         (push (- end-time start-time) times)
         (push (- end-memory start-memory) memory-usage)))
     (when ,report
       (format t "~&Performance Results (~D iterations):~%" ,iterations)
       (format t "  Average time: ~,2F ms~%"
               (* 1000 (/ (apply #'+ times) ,iterations internal-time-units-per-second)))
       (format t "  Min time: ~,2F ms~%"
               (* 1000 (/ (apply #'min times) internal-time-units-per-second)))
       (format t "  Max time: ~,2F ms~%"
               (* 1000 (/ (apply #'max times) internal-time-units-per-second))))
     (values (apply #'min times) (apply #'max times) (/ (apply #'+ times) ,iterations))))

;;; All functions are exported in the defpackage above
