(in-package :skyline-tool)

(declaim (optimize (debug 3)))

(define-constant +tia-voices+
    '( ;; Waveform 0 = silent
      (0)
      (1 ;; Waveform 1 = “Buzzy”
       (2096 2080)
       (1048 1040)
       (698.7  693.3)
       (524  520)
       (419.2  416)
       (349.3  346.7)
       (299.4  297.1)
       (262    260)
       (232.9  231.1)
       (209.6  208)
       (190.5  189.1)
       (174.7  173.3)
       (161.2  160)
       (149.7  148.6)
       (139.7  138.7)
       (131    130)
       (123.3  122.4)
       (116.4  115.6)
       (110.3  109.5)
       (104.8  104)
       (99.8    99)
       (95.3    94.5)
       (91.1    90.4)
       (87.3    86.7)
       (83.8    83.2)
       (80.6    80)
       (77.6    77)
       (74.9    74.3)
       (72.3    71.7)
       (69.9    69.3)
       (67.6    67.1)
       (65.5    65))

      (2 ;; Waveform 2 = lower-frequency buzz/rumble
       ( 67.6 67.1)
       ( 33.8 33.5)
       ( 22.5 22.4)
       ( 16.9 16.8)
       ( 13.5 13.4)
       ( 11.3 11.2)
       ( 9.7 9.6)
       ( 8.5 8.4)
       ( 7.5 7.5)
       ( 6.8 6.7)
       ( 6.1 6.1)
       ( 5.6 5.6)
       ( 5.2 5.2)
       ( 4.8 4.8)
       ( 4.5 4.5)
       ( 4.2 4.2)
       ( 4 4)
       ( 3.8 3.7)
       ( 3.6 3.5)
       ( 3.4 3.4)
       ( 3.2 3.2)
       ( 3.1 3)
       ( 3 2.9)
       ( 2.8 2.8)
       ( 2.7 2.7)
       ( 2.6 2.6)
       ( 2.5 2.5)
       ( 2.4 2.4)
       ( 2.3 2.3)
       ( 2.3 2.2)
       ( 2.2 2.2)
       ( 2.1 2.1))

      (;; Waveform 3 = Flangy “UFO”
       ( 67.6 67.1)
       ( 33.8 33.5)
       ( 22.5 22.4)
       ( 16.9 16.8)
       ( 13.5 13.4)
       ( 11.3 11.2)
       ( 9.7 9.6)
       ( 8.5 8.4)
       ( 7.5 7.5)
       ( 6.8 6.7)
       ( 6.1 6.1)
       ( 5.6 5.6)
       ( 5.2 5.2)
       ( 4.8 4.8)
       ( 4.5 4.5)
       ( 4.2 4.2)
       ( 4 4)
       ( 3.8 3.7)
       ( 3.6 3.5)
       ( 3.4 3.4)
       ( 3.2 3.2)
       ( 3.1 3)
       ( 3 2.9)
       ( 2.8 2.8)
       ( 2.7 2.7)
       ( 2.6 2.6)
       ( 2.5 2.5)
       ( 2.4 2.4)
       ( 2.3 2.3)
       ( 2.3 2.2)
       ( 2.2 2.2)
       ( 2.1 2.1))

      (;; Waveform 4 = Pure tone
       (7860 7800)
       (5240 5200)
       (3930 3900)
       (3144 3120)
       (2620 2600)
       (2245.7 2228.6)
       (1965 1950)
       (1746.7 1733.3)
       (1572 1560)
       (1429.1 1418.2)
       (1310 1300)
       (1209.2 1200)
       (1122.9 1114.3)
       (1048 1040)
       (982.5 975)
       (924.7 917.6)
       (873.3 866.7)
       (827.4 821.1)
       (786 780)
       (748.6 742.9)
       (714.5 709.1)
       (683.5 678.3)
       (655 650)
       (628.8 624)
       (604.6 600)
       (582.2 577.8)
       (561.4 557.1)
       (542.1 537.9)
       (507.1 503.2)
       (491.3 487.5)
       )
      (;; Waveform 5 = Pure tone
       (7860 7800)
       (5240 5200)
       (3930 3900)
       (3144 3120)
       (2620 2600)
       (2245.7 2228.6)
       (1965 1950)
       (1746.7 1733.3)
       (1572 1560)
       (1429.1 1418.2)
       (1310 1300)
       (1209.2 1200)
       (1122.9 1114.3)
       (1048 1040)
       (982.5 975)
       (924.7 917.6)
       (873.3 866.7)
       (827.4 821.1)
       (786 780)
       (748.6 742.9)
       (714.5 709.1)
       (683.5 678.3)
       (655 650)
       (628.8 624)
       (604.6 600)
       (582.2 577.8)
       (561.4 557.1)
       (542.1 537.9)
       (507.1 503.2)
       (491.3 487.5)
       )

      (;; Waveform 6 = Somewhere between Pure & Buzzy
       (   1014.2     987.8 )
       (      507.1     493.9 )
       (      338.1     329.6 )
       (      253.5     246.9 )
       (      202.83    207.7 )
       (      169       164.8 )
       (      144.9     146.8 )
       (      126.8     123.5 )
       (      112.7     110   )
       (      101.42    103.8 )
       (      92.22     92.5 )
       (      84.5      82.4 )
       (      78  2     77.8 )
       (      72.4      73.4 )
       (      67.62     69.3 )
       (      63.4      61.7 )
       (      59.71     58.3 )
       (      56.3      55   )
       (      53.41     51.9 )
       (      50.71     51.9 )
       (      48.3      49   )
       (      46.11     46.2 )
       (      44.1      43.7 )
       (      42.3      41.2 )
       (      40.6      41.2 )
       (      39  1     38.9 )
       (      37.6      36.7 )
       (      36.2      36.7 )
       (      35  1     34.6 )
       (      33.81     34.6 )
       (      32.7      32.7 )
       (      31.7      30.9 )
       )(;; Waveform 7 = Somewhere between Pure & Buzzy
       (   1014.2     987.8 )
       (      507.1     493.9 )
       (      338.1     329.6 )
       (      253.5     246.9 )
       (      202.83    207.7 )
       (      169       164.8 )
       (      144.9     146.8 )
       (      126.8     123.5 )
       (      112.7     110   )
       (      101.42    103.8 )
       (      92.22     92.5 )
       (      84.5      82.4 )
       (      78  2     77.8 )
       (      72.4      73.4 )
       (      67.62     69.3 )
       (      63.4      61.7 )
       (      59.71     58.3 )
       (      56.3      55   )
       (      53.41     51.9 )
       (      50.71     51.9 )
       (      48.3      49   )
       (      46.11     46.2 )
       (      44.1      43.7 )
       (      42.3      41.2 )
       (      40.6      41.2 )
       (      39  1     38.9 )
       (      37.6      36.7 )
       (      36.2      36.7 )
       (      35  1     34.6 )
       (      33.81     34.6 )
       (      32.7      32.7 )
       (      31.7      30.9 )
       )
      (;;Waveform 8 = white noise
       (     61.5       61.7  )
       (      30.8       30.9  )
       (      20.5       20.6  )
       (      15.4  )
       (      12.3             )
       (      10.3             )
       (      8.8             )
       (      7.7             )
       (      6.8             )
       (      6.2             )
       (      5.6             )
       (      5.1             )
       (      4.7             )
       (      4.4             )
       (      4.1             )
       (      3.8             )
       (      3.6             )
       (      3.4             )
       (      3.2             )
       (      3.1             )
       (      2.9             )
       (      2.8             )
       (      2.7             )
       (      2.6             )
       (      2.5             )
       (      2.4             )
       (      2.3             )
       (      2.2             )
       (      2.1             )
       (      2               )
       (      2               )
       (      1.9)
       )

      (;; Waveform 9 = Somewhere between Pure & Buzzy
       (   1014.2     987.8 )
       (      507.1     493.9 )
       (      338.1     329.6 )
       (      253.5     246.9 )
       (      202.83    207.7 )
       (      169       164.8 )
       (      144.9     146.8 )
       (      126.8     123.5 )
       (      112.7     110   )
       (      101.42    103.8 )
       (      92.22     92.5 )
       (      84.5      82.4 )
       (      78  2     77.8 )
       (      72.4      73.4 )
       (      67.62     69.3 )
       (      63.4      61.7 )
       (      59.71     58.3 )
       (      56.3      55   )
       (      53.41     51.9 )
       (      50.71     51.9 )
       (      48.3      49   )
       (      46.11     46.2 )
       (      44.1      43.7 )
       (      42.3      41.2 )
       (      40.6      41.2 )
       (      39  1     38.9 )
       (      37.6      36.7 )
       (      36.2      36.7 )
       (      35  1     34.6 )
       (      33.81     34.6 )
       (      32.7      32.7 )
       (      31.7      30.9 )
       )(;; Waveform 10 = Somewhere between Pure & Buzzy
       (   1014.2     987.8 )
       (      507.1     493.9 )
       (      338.1     329.6 )
       (      253.5     246.9 )
       (      202.83    207.7 )
       (      169       164.8 )
       (      144.9     146.8 )
       (      126.8     123.5 )
       (      112.7     110   )
       (      101.42    103.8 )
       (      92.22     92.5 )
       (      84.5      82.4 )
       (      78  2     77.8 )
       (      72.4      73.4 )
       (      67.62     69.3 )
       (      63.4      61.7 )
       (      59.71     58.3 )
       (      56.3      55   )
       (      53.41     51.9 )
       (      50.71     51.9 )
       (      48.3      49   )
       (      46.11     46.2 )
       (      44.1      43.7 )
       (      42.3      41.2 )
       (      40.6      41.2 )
       (      39  1     38.9 )
       (      37.6      36.7 )
       (      36.2      36.7 )
       (      35  1     34.6 )
       (      33.81     34.6 )
       (      32.7      32.7 )
       (      31.7      30.9 ))


      (;; Waveform 12 = lower-pitch pure tones
       )

      (;; Waveform 14 = low-pitch electronic tones
       )

      (;; Waveform 15 = low-pitch electronic tones

       ))
  :test #'equalp
  :documentation "NTSC and PAL/SECAM sound values for each frequency code

Format: NTSC followed by PAL/SECAM frequency values for each AUDF value")

(define-constant +vic-notes+
    '()
  :test 'equalp)

(defgeneric midi-to-sound-binary (output-coding machine-type midi-notes sound-chip)
  (:method (output-coding machine-type midi-notes sound-chip)
    (error "No handler for output coding ~s (machine ~s, sound chip ~s); ~
skipping MIDI music with ~:d track~:p"
           output-coding machine-type sound-chip (length midi-notes)))

  ;; Intellivision AY-3-8910 PSG support
  (:method (output-coding (machine-type (eql 2609)) midi-notes (sound-chip (eql :ay-3-8910)))
    (midi-to-ay-3-8910 midi-notes output-coding)))

(defun best-tia-ntsc-note-for (freq &optional (voice 1))
  (when freq
    (let ((notes (mapcar #'first
                         (rest (elt +tia-voices+ voice)))))
      (when-let (freq-code (position (first (sort (copy-list notes) #'<
                                                  ::key (curry #'frequency-distance freq)))
                                     notes :test #'=))
        (let ((dist-1 (when (plusp freq-code) (frequency-distance freq (elt notes (1- freq-code)))))
              (dist0 (frequency-distance freq (elt notes freq-code)))
              (dist+1 (when (< freq-code #xff) (frequency-distance freq (elt notes (1+ freq-code))))))
          (if (> (if dist+1 (+ dist0 dist+1) most-positive-fixnum) (if dist-1 (+ dist-1 dist0) most-positive-fixnum))
              (list voice (1- freq-code) (/ dist-1 (+ dist-1 dist0)))
              (list voice freq-code (/ dist0 (+ dist0 dist+1)))))))))

(defun best-tia-pal-note-for (freq &optional (voice 1))
  (let ((notes (mapcar #'second
                       (rest (elt +tia-voices+ voice)))))
    (when-let (freq-code (position (first (sort (copy-list notes) #'<
                                                :key (curry #'frequency-distance freq)))
                                   notes :test #'=))
      (let ((dist-1 (when (plusp freq-code) (frequency-distance freq (elt notes (1- freq-code)))))
            (dist0 (frequency-distance freq (elt notes freq-code)))
            (dist+1 (when (< freq-code #xff) (frequency-distance freq (elt notes (1+ freq-code))))))
        (if (> (if dist+1 (+ dist0 dist+1) most-positive-fixnum) (if dist-1 (+ dist-1 dist0) most-positive-fixnum))
          (list voice (1- freq-code) (/ dist-1 (+ dist-1 dist0)))
          (list voice freq-code (/ dist0 (+ dist0 dist+1))))))))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defun ooxml->string (xml)
;;     (if (consp xml)
;;         (format nil "~{~a~}" (mapcar #'ooxml->string (cddr xml)))
;;         xml))
;;
;;   (defun ooxml-cell-repeats (cell)
;;     (if-let (repeat-index (and (consp (second cell))
;;                                (consp (first (second cell)))
;;                                (position-if (lambda (attr) (equal (first attr) "number-columns-repeated"))
;;                                             (second cell))))
;;       (parse-integer (second (elt (second cell) repeat-index)))
;;       1))
;;
;;   (defun ooxml-repeated-cell (cell string)
;;     (loop repeat (ooxml-cell-repeats cell)
;;           collect string))
;;
;;   (defun ods-table-rows->list (table)
;;     (mapcar (lambda (row)
;;               (loop for cell in (remove-if-not (lambda (el) (equal (caar el) "table-cell"))
;;                                                row)
;;                     append (ooxml-repeated-cell cell (ooxml->string cell))))
;;             (mapcar #'rest (remove-if-not (lambda (el)
;;                                             (equal (caar el) "table-row"))
;;                                           table))))

  (defun midi->note-name (note)
    (if (and note (<= 0 note 127))
        (multiple-value-bind (octave letter) (floor note 12)
          (format nil "~a~d" (elt '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B") letter)
                  (- octave 1)))
        (error "Invalid MIDI note number: ~a" note)))

  (defun note->midi-note-number (octave note-name)
    (+ 12
       (* 12 octave)
       (or (position note-name '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B")
                     :test #'string-equal)
           (position note-name '("C" "Db" "D" "Eb" "E" "F" "Gb" "G" "Ab" "A" "Bb" "B")
                     :test #'string-equal)
           (position note-name '("C" "C♯" "D" "D♯" "E" "F" "F♯" "G" "G♯" "A" "A♯" "B")
                     :test #'string-equal)
           (position note-name '("C" "D♭" "D" "E♭" "E" "F" "G♭" "G" "A♭" "A" "B♭" "B")
                     :test #'string-equal))))

  (assert (= 60 (note->midi-note-number 4 "C")) ()
          "Note->MIDI-Note-Number is not tuned correctly")

  (defun interpret-pokey-sheet1 (sheet1)
    (assert (eql 0 (search "POKEY table" (aref sheet1 2 0))) (sheet1)
            "POKEY tables not in expected format~%~s" (aref sheet1 2 0))
    (assert (equal "C" (aref sheet1 0 3)) ()
            "Table does not start with C~%~s" (aref sheet1 0 3))
    (loop for row from 3 below 112
          for out-row from 0
          with notes = (make-array (list 18 109) :element-type '(or null (unsigned-byte 16)))
          with octave = 1
          do (progn
               (when (eql 0 (search "OCTAVE " (aref sheet1 35 row)))
                 (setf octave (parse-integer (subseq (aref sheet1 35 row) 7))))
               (setf (aref notes 0 out-row) (note->midi-note-number
                                             octave
                                             (string-trim " " (aref sheet1 0 row))))
               (loop for column from 1 below 35 by 2
                     for out-column from 1
                     do (let ((number$ (aref sheet1 column row)))
                          (setf (aref notes out-column out-row)
                                (if (and (not (emptyp number$))
                                         (every #'digit-char-p number$))
                                    (parse-integer number$)
                                    nil)))))
          finally (return notes)))

  (defun interpret-pokey-sheet2 (sheet2)
    (return-from interpret-pokey-sheet2 nil)
    (assert (eql 0 (search "POKEY table" (aref sheet2 2 0))) (sheet2)
            "POKEY tables not in expected format~%~s" (aref sheet2 2 0))
    (assert (equal "C" (aref sheet2 0 3)) ()
            "Table does not start with C~%~s" (aref sheet2 0 3))
    (loop for row from 3 below 112
          for out-row from 0
          with notes = (make-array (list 18 109) :element-type '(or null (unsigned-byte 16)))
          with octave = 1
          do (progn
               (when (eql 0 (search "OCTAVE " (aref sheet2 35 row)))
                 (setf octave (parse-integer (subseq (aref sheet2 35 row) 7))))
               (setf (aref notes 0 out-row) (note->midi-note-number
                                             octave
                                             (string-trim " " (aref sheet2 0 row))))
               (loop for column from 1 below 35 by 2
                     for out-column from 1
                     do (let ((number$ (aref sheet2 column row)))
                          (setf (aref notes out-column out-row)
                                (if (and (not (emptyp number$))
                                         (every #'digit-char-p number$))
                                    (parse-integer number$)
                                    nil)))))
          finally (return notes)))

  (defun interpret-pokey-tables (tables)
    (destructuring-bind (sheet1 sheet2 sheet3 sheet4) tables
      (declare (ignore sheet3 sheet4))
      (interpret-pokey-sheet2 sheet2) ; TODO: #1228, ignored
      (interpret-pokey-sheet1 sheet1)))

  (defun lists->2d-array (lists)
    (let ((rows (length lists))
          (cols (reduce #'max (mapcar #'length lists))))
      (loop with array = (make-array (list cols rows))
            for row in lists
            for r from 0
            do (loop for column in row
                     for c from 0
                     do (setf (aref array c r) column))
            finally (return array))))

  ;; (defun read-pokey.ods ()
  ;;   (format *trace-output* "~&Reading Synthpopalooza's POKEY tables spreadsheet… ")
  ;;   (prog1
  ;;       (zip:with-zipfile (zip (make-pathname
  ;;                               :name "Synthpopalooza POKEY tables"
  ;;                               :type "ods"
  ;;                               :directory (append (pathname-directory
  ;;                                                   (asdf:system-source-directory :skyline-tool))
  ;;                                                  (list "data")))
  ;;                          :force-utf-8 t)
  ;;         (let ((xml (xmls:parse-to-list
  ;;                     (map 'string #'code-char
  ;;                          (zip:zipfile-entry-contents
  ;;                           (zip:get-zipfile-entry "content.xml" zip))))))
  ;;           (assert (and (consp (first xml))
  ;;                        (equal (car (first xml)) "document-content")
  ;;                        (equal (cdr (first xml))
  ;;                               "urn:oasis:names:tc:opendocument:xmlns:office:1.0"))
  ;;                   (xml)
  ;;                   "ODS file seems to be malformed: document-content tag missing or invalid~%~s"
  ;;                   (first xml))
  ;;           (let ((body (first (remove-if-not (lambda (el) (equal (caar el) "body")) (rest xml)))))
  ;;             (assert (and (consp (caaddr body))
  ;;                          (equal (car (caaddr body)) "spreadsheet")
  ;;                          (equal (cdr (caaddr body)) "urn:oasis:names:tc:opendocument:xmlns:office:1.0"))
  ;;                     (xml)
  ;;                     "ODS is not a spreadsheet?~%~s"
  ;;                     (first body))
  ;;             (let* ((tables (mapcar #'cdr
  ;;                                    (remove-if-not (lambda (el) (equal (caar el) "table"))
  ;;                                                   (subseq (caddr body) 2)))))
  ;;               (mapcar #'ods-table-rows->list tables)))))
  ;;     (format *trace-output* " done.")))

  ;; (defun read-pokey-tables ()
  ;;   (interpret-pokey-tables (mapcar #'lists->2d-array (read-pokey.ods)))))

;; (define-constant +pokey-notes-table+
;;     #2A((0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0)
;;         (0 0 0 0 0 0 0 0))
;;   :test 'equalp)

(defvar +pokey-notes-table+
  #2A((0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0)))

(define-constant +all-hokey-distortions+
    '(:10 :2 :12a :12b :8 :4b :4a)
  :test #'equalp)

(defun pokey-distortion-column (distortion bits)
  (+ (ecase bits
       (16 0)
       (8 1))
     (ecase distortion
       (:10 1)
       (:2 4)
       (:12a 7)
       (:12b 10)
       (:8 12)
       (:4b 14)
       (:4a 16))))

;; (assert (= 97 (aref +pokey-notes-table+ (pokey-distortion-column :12a 8) 5))
;;         () "POKEY notes table seems to be incorrect")

(defun pokey->frequency (AUDF)
  (/ 15699.9 #| FIXME: #1229 NTSC? |# (* 2 (1+ AUDF))))

(defun frequency->pokey (frequency)
  (ceiling (/ (- 15699.9 #| FIXME: #1229 NTSC? |# (* 2 frequency)) (* 2 frequency))))

(defun best-pokey-note-for (midi-note-number &optional distortion bits)
  (declare (ignore distortion bits))
  (multiple-value-bind (value error)
      (frequency->pokey (freq<-midi-key midi-note-number))
    #+ () (format *trace-output* "~&For ~a, ~d × ~d then ~d × ~d" (midi->note-name midi-note-number)
                  value (ash (logand #xf0 (apply #'fraction-nybbles (simplify-to-rational error))) -4)
                  (1+ value) (logand #x0f (apply #'fraction-nybbles (simplify-to-rational error))))
    (values value error)))

(defun null-if-zero-note (n)
  (if (or (null n) (zerop (third n))) nil n))

(defun best-tia-note-for-ntsc (note)
  (labels ((nearest (&rest set)
             (first (sort (remove-if #'null set) #'< :key #'third))))
    (nearest (null-if-zero-note (best-tia-ntsc-note-for note 4))
             (null-if-zero-note (best-tia-ntsc-note-for note 5))
             (null-if-zero-note (best-tia-ntsc-note-for note 2))
             (null-if-zero-note (best-tia-ntsc-note-for note 6))
             (null-if-zero-note (best-tia-ntsc-note-for note 8))
             (list 0 0 most-positive-fixnum))))

(defun best-tia-note-for-pal (note)
  (labels ((nearest (&rest set)
             (first (sort (remove-if #'null set) #'< :key #'third))))
    (nearest (null-if-zero-note (best-tia-pal-note-for note 4))
             (null-if-zero-note (best-tia-pal-note-for note 5))
             (null-if-zero-note (best-tia-pal-note-for note 2))
             (null-if-zero-note (best-tia-pal-note-for note 6))
             (null-if-zero-note (best-tia-pal-note-for note 8))
             (list 0 0 most-positive-fixnum))))

(defun array<-7800-tia-notes-list (notes tv-type)
  (let ((frame-rate (ecase tv-type (:ntsc 60) (:pal 50))))
    (coerce (sort (adjust-note-timing-for-frame-rate (merge-tia-voices notes) frame-rate)
                  #'< :key #'second)
            'vector)))

(defun array<-tia-notes-list (list output-coding)
  (let ((array (make-array (list (length list) 5))))
    (loop for note in list
          for i from 0
          for (control freq)
            =
            (if-let (note (elt note 2))
              (ecase output-coding
                (:ntsc
                 (best-tia-note-for-ntsc note))
                (:pal
                 (best-tia-note-for-pal note)))
              (list nil nil))
          do (setf (aref array i 0) (when (elt note 0)
                                      (floor (max (/ (elt note 0) +midi-duration-divisor+)
                                                  1))) ; duration
                   (aref array i 1) control            ;control
                   (aref array i 2) freq               ;frequency
                   (aref array i 3) (when (elt note 3)
                                      (floor (elt note 3))) ; volume
                   (aref array i 4) (elt note 4)))          ;comment
    array))

(defun midi->2600-tia (notes)
  (let ((volume 8)
        (last-duration 0)
        (output (list)))
    (loop for note in notes
          for i from 0
          for (note/rest . info) = note
          do (ecase note/rest
               (:note
                (push (make-array 5 :initial-contents (list (let ((d (getf info :duration)))
                                                              (prog1 (if (plusp d)
                                                                         d
                                                                         last-duration)
                                                                (setf last-duration d)))
                                                            0
                                                            (freq<-midi-key (getf info :key))
                                                            volume
                                                            (nth-value 2 (key<-midi-key (getf info :key)))))
                      output))
               (:text (push (make-array 5 :initial-contents (list nil nil nil nil info))
                            output))))
    (reverse output)))

(defun find-tia-distortion (params)
  (warn "Not finding best TIA distortion for ~a" params)
  1)

(defun midi->7800-tia (midi-notes tv)
  (loop for track in midi-notes
        with distortion
        with tia-voices = (make-array 2 :initial-element nil)
        append
        (loop for (kind . params) in track
              collecting (ecase kind
                           (:text (setf distortion (find-tia-distortion params)))
                           (:note (destructuring-bind (&key time key duration) params
                                    (typecase distortion
                                      (null
                                       (warn "Note without knowing instrument: dropping note ~a at time ~d"
                                             (midi->note-name key) time))
                                      (number
                                       (let ((voice (find-free-voice tia-voices time)))
                                         (if voice
                                             (setf (aref tia-voices voice)
                                                   (cons (list time (best-tia-note-for key distortion tv)
                                                               duration distortion)
                                                         (aref tia-voices voice)))
                                             (warn "Too much polyphony: dropping note ~a at time ~d (TIA 7800)"
                                                   (midi->note-name key) time))))
                                      (t (error "Unhandled DISTORTION ~s for TIA only" distortion)))))))
        finally (return tia-voices)))

(defun voice-free-at-time-p (voice time)
  (or (emptyp voice)
      (notany (lambda (note)
                (destructuring-bind (start key duration distortion) note
                  (declare (ignore key distortion))
                  (<= start time (+ start duration))))
              voice)))

(defun find-free-voice (voices time)
  (position-if (lambda (voice) (voice-free-at-time-p voice time))
               voices))

(defun find-pokey-distortion (text)
  (cond ((search "Piano" text) :10)
        ((search "Drums" text) :12a)
        (t (warn "Ignored text in MIDI: ~s" text))))

(defun best-tia-note-for (key distortion tv)
  (let ((freq (freq<-midi-key key)))
    (ecase tv
      (:ntsc (or (best-tia-ntsc-note-for freq distortion)
                 (best-tia-note-for-ntsc freq)))
      (:pal (or (best-tia-pal-note-for freq distortion)
                (best-tia-note-for-pal freq))))))

(defun adjust-note-timing-for-frame-rate (notes frame-rate)
  (loop for (voice time key duration distortion) in notes
        ;; 454 = convert 1⁄4 note to seconds at 120bpm
        collecting (list voice (floor (* (/ time 454) 1/2 frame-rate))
                         key
                         (floor (* (/ duration 454) 1/2 frame-rate))
                         distortion)))

(defun array<-pokey-notes-list (notes tv-type)
  (let ((frame-rate (ecase tv-type (:ntsc 60) (:pal 50))))
    (coerce (sort (adjust-note-timing-for-frame-rate (merge-pokey-tia-voices notes) frame-rate)
                  #'< :key #'second)
            'vector)))

(defmethod midi-to-sound-binary (output-coding machine-type midi-notes (sound (eql :pokey)))
  (error "superseded"))

(defun collect-midi-texts (midi)
  (loop for track in (midi:midifile-tracks midi)
        for track-number from 0
        appending (loop for chunk in track
                        when (typep chunk 'midi::text-message)
                          collect (format nil "~& ~d. ~:(~a~) “~a”"
                                          track-number (type-of chunk)
                                          (slot-value chunk 'midi::text)))))

(defun midi-track-notes-count (track)
  (count-if (lambda (chunk)
              (typep chunk 'midi::note-on-message))
            track))

(defun midi-tracks-with-music (midi)
  (remove-if-not (lambda (track)
                   (plusp (midi-track-notes-count track)))
                 (midi:midifile-tracks midi)))

(defconstant +a4/hz+ 440
  "The frequency (Hz) of the A in octave 4; by convention, 440Hz.")

(define-constant +semitone+ (expt 2 1/12)
  :documentation "The ratio of each semitone in equal temperment"
  :test #'=)

(defun midi-distance-from-a4 (key)
  (- key 69))

(defun freq<-midi-key (key)
  (* (expt 2 (/ (midi-distance-from-a4 key) 12)) +a4/hz+))

(defun frequency-distance (a b)
  (cond ((or (null a) (null b)) most-positive-fixnum)
        ((> b a)
         (frequency-distance b a))
        ((zerop b) most-positive-fixnum)
        (t
         (log (/ a b) 2))))

(defun read-midi (file)
  (let ((midi (midi:read-midi-file file)))
    (let ((parts/quarter (midi::midifile-division midi))
          (texts (collect-midi-texts midi))
          (real-tracks (midi-tracks-with-music midi)))
      (when (zerop (length real-tracks))
        (error "File ~a contains no tracks with actual music?
Gathered text:~{~% • ~a~}"
               file texts))
      (map 'list (rcurry #'midi-track-decode parts/quarter)
           real-tracks))))

(defun import-music-for-playlist (output-coding line catalog comments-catalog
                                  &key ((:sound-chip sound-chip$)))
  (let ((sound-chip (make-keyword sound-chip$)))
    (when (or (not (find #\= line))
              (char= #\; (first-elt (string-trim #(#\Space #\Tab) line))))
      (return-from import-music-for-playlist nil))
    (destructuring-bind (symbol-name$ midi-file-name$)
        (split-string line :separator "=")
      (let ((symbol-name (make-keyword (string-trim " " symbol-name$)))
            (midi-file-name (string-trim " " midi-file-name$)))
        (format *trace-output* "~&Converting MIDI file ~a to ~a format for ~a…"
                midi-file-name sound-chip output-coding)
        (multiple-value-bind (numbers comments)
            (midi-to-sound-binary output-coding
                                  *machine*
                                  (read-midi midi-file-name)
                                  sound-chip)
          (format *trace-output* "~& - Generated ~:d sound value~:p…" (first (array-dimensions numbers)))
          (when (plusp (first (array-dimensions numbers)))
            (setf (gethash symbol-name catalog) numbers
                  (gethash symbol-name comments-catalog) comments)))))))

(defun edit-long-notes (table)
  (let ((list (loop for note from 0 below (array-dimension table 0)
                    if (< (aref table note 0) #x80)
                      collect (list (aref table note 0)
                                    (aref table note 2))
                    else
                      append (list (list (floor (aref table note 0) #x80)
                                         (aref table note 2))
                                   (list (mod (aref table note 0) #x80)
                                         (aref table note 2))))))
    (make-array (list (length list) 2)
                :element-type '(unsigned-byte 8)
                :initial-contents list)))

(defvar *room-available-for-music%* 2000) ; FIXME: #1230

(defmacro memoize (var &body fn)
  `(or (and (boundp ',var) ,var)
       (setf ,var (progn ,@fn))))

(defun room-available-for-music ()
  (memoize *room-available-for-music%*
    (- #x1000
       (find-size-of-bank)
       #x100)))

(defun increment-name (name)
  (check-type name symbol)
  (let ((name$ (string name)))
    (if (digit-char-p (last-elt name$))
        (let ((number-starts (1+ (position-if-not #'digit-char-p
                                                  name$
                                                  :from-end t))))
          (intern (concatenate
                   'string
                   (subseq name$ 0 number-starts)
                   (princ-to-string
                    (1+ (parse-integer
                         (subseq name$ number-starts)))))))
        (intern (concatenate 'string name$ "-2")
                (symbol-package name)))))

(defun fill-array² (destination source start-index end-index)
  (loop
    for x₀ from (first start-index) upto (first end-index)
    for x₁ from 0
    do (loop
         for y₀ from (second start-index) upto (second end-index)
         for y₁ from 0
         do (setf (aref destination x₁ y₁) (aref source x₀ y₀))))
  destination)

(defun song-split (array bytes)
  (let ((split-length (floor bytes 2)))
    (let ((before (fill-array² (make-array (list (1+ split-length) 2)
                                           :element-type '(unsigned-byte 8)
                                           :initial-element #xff)
                               array
                               '(0 0) (list (1- split-length) 1)))
          (after
            (fill-array² (make-array (list (- (array-dimension array 0)
                                              split-length)
                                           2)
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)
                         array (list split-length 1)
                         (mapcar #'1- (array-dimensions array)))))
      (setf (aref before split-length 0) #xff
            (aref before split-length 1) #xff)
      (values before after))))

(defun split-long-songs (songs)
  (loop
    with room-used = 0
    for (title . notes) = (pop songs)
    for song-bytes = (* 2 (array-dimension notes 0))
    collect
    (if (< (room-available-for-music)
           (+ room-used song-bytes))
        ;; split the song into two with a continuation flag
        (progn
          (format *trace-output* "~&Splitting song “~a” after ~:d bytes …"
                  title (- (room-available-for-music)
                           room-used
                           2))
          (multiple-value-bind (before after)
              (song-split notes (- (room-available-for-music)
                                   room-used
                                   2))
            (check-type before (array (unsigned-byte 8) (* 2)))
            (check-type after (array (unsigned-byte 8) (* 2)))
            (prog1 (cons title before)
              (push (cons (increment-name title) after)
                    songs)
              (setf room-used 0))))
        (prog1
            (cons title notes)
          (incf room-used song-bytes)))))

(defun song-bytes (song)
  (check-type song cons)
  (check-type (car song) symbol)
  (check-type (cdr song) (array * (* 2)))
  (* 2 (array-dimension (cdr song) 1)))


(defun assign-repertoire-to-banks (repertoire)
  (loop
    with assignments = nil
    for music-bank downfrom 128
    while repertoire
    do (loop with room = (room-available-for-music)
             for length = (song-bytes (first repertoire))
             while (> room length)
             do (progn
                  (decf room length)
                  (push (list music-bank (pop repertoire)) assignments)))
    finally
       (return assignments)))

(defconstant +midi-duration-divisor+ 4)

(defun write-song-data-to-file (title notes source-file)
  (format source-file "~%;;;~|~%~a:" (assembler-label-name title))
  (fresh-line *trace-output*)
  (loop for i below (array-dimension notes 0)
        do (let ((duration (aref notes i 0))
                 (control (aref notes i 1))
                 (frequency (aref notes i 2))
                 (volume (aref notes i 3))
                 (comment (aref notes i 4)))
             (princ "♪" *trace-output*)
             (finish-output *trace-output*)
             (if (or (null duration) (< duration +midi-duration-divisor+))
                 (if (= i (1- (array-dimension notes 0)))
                     (format source-file "~%	.sound 0, 0, 0, 0, 1	; ~a" comment)
                     (format source-file "~%	;; ~a" comment))
                 (format source-file "~%	.sound $~x, $~x, $~2,'0x, ~3d, ~d	; ~a"
                         volume
                         control
                         frequency
                         (round (min (max 1 (/ duration +midi-duration-divisor+)) #xff))
                         (if (= i (1- (array-dimension notes 0))) ; last note
                             1
                             0)
                         comment))
             (finish-output source-file)))
  (format source-file "~2%;;; end of ~a" (assembler-label-name title)))

(defgeneric write-song-data-to-binary (notes object machine sound-chip)
  (:method (notes object machine sound-chip)
    (error "Unimplemented: Cannot write notes for machine ~a, sound-chip ~a" machine sound-chip)))

(defun pokey-distortion-code (distortion)
  (ecase distortion
    (:10 10) (:2 2) (:12a 12) (:12b 13) (:8 8) (:4a 4) (:4b 5))

  (parse-integer (symbol-name distortion) :junk-allowed t))

(defun assigned-song-bank-and-title (assignment)
  (list (car assignment)
        (assembler-label-name
         (second assignment))))

(defun default-playlist-pathname (file-name playlist-name)
  (make-pathname :defaults playlist-name
                 :name file-name
                 :type "playlist"))

(defun max-of-cars (assignments)
  (reduce #'max (mapcar #'car assignments)))

(defun numbered-bank-filename (basename bank)
  (make-pathname :name (concatenate 'string
                                    (pathname-name basename)
                                    "."
                                    (princ-to-string bank))
                 :defaults basename))

(defun build-repertoire-from-raw-catalog (catalog)
  (split-long-songs
   (loop for symbol-name being the hash-keys of catalog
         collect (cons symbol-name
                       (edit-long-notes
                        (gethash symbol-name catalog))))))

(defun write-music-bank-header (assignments source-file)
  (format source-file ";;; -*- asm -*-
;;; This file generated by Skyline-Tool; editing it is futile.

.include \"preamble.s\"
.include \"music-bank.s'

;;; start of encoded music data from MIDI files
MusicBanks:
~{~%          .byte ~d ; ~a~}
Music:~:*
~{~%          .word ~*~a~} ~%"
          (mapcar #'assigned-song-bank-and-title
                  assignments)))

(defun import-songs-from-playlist-to-catalog (&key
                                                playlist-file-name
                                                output-coding
                                                catalog
                                                comments-catalog)
  (with-input-from-file (playlist-file playlist-file-name)
    (loop for line = (read-line playlist-file nil)
          while line
          do (import-music-for-playlist output-coding line
                                        catalog comments-catalog)))
  (format *trace-output* "~&End of playlist; collected ~r song~:p~
~:*~[~:; in “~a” coding~]."
          (hash-table-count catalog) output-coding))

(defun import-song-to-catalog (&key song-file-name
                                    output-coding
                                    catalog
                                    comments-catalog
                                    (sound-chip "TIA"))
  (import-music-for-playlist output-coding
                             (format nil
                                     "Song_~a=~a"
                                     (pathname-name song-file-name) song-file-name)
                             catalog comments-catalog
                             :sound-chip sound-chip)
  (format *trace-output* "~&Collected ~r song~:p~
~:*~[~:; in “~a” coding~] for ~a."
          (hash-table-count catalog) output-coding sound-chip))

(defun compile-music-7800 (object-name midi-name sound-chip output-coding)
  (let ((*machine* 7800)
        (catalog (make-hash-table))
        (comments-catalog (make-hash-table)))
    (with-output-to-file (object object-name :element-type '(unsigned-byte 8)
                                             :if-exists :supersede :if-does-not-exist :create)
      (format *trace-output* "~&Writing ~a…" object-name)
      (import-song-to-catalog :song-file-name midi-name
                              :sound-chip sound-chip
                              :output-coding output-coding
                              :catalog catalog
                              :comments-catalog comments-catalog)
      (loop for symbol being the hash-keys of catalog
            for notes = (gethash symbol catalog)
            do (write-song-data-to-binary notes object *machine* sound-chip)))))

(defun compile-music-2609 (source-out-name midi-name sound-chip output-coding)
  "Compile music for Intellivision AY-3-8910 PSG"
  (let ((*machine* 2609)
        (catalog (make-hash-table))
        (comments-catalog (make-hash-table)))
    (with-output-to-file (source-out source-out-name :if-exists :supersede :if-does-not-exist :create)
      (format *trace-output* "~&Writing ~a…" source-out-name)
      (format source-out ";;; Music compiled from ~a for Intellivision AY-3-8910 PSG
;;; do not bother editing (generated file will be overwritten)"
              midi-name)
      (import-song-to-catalog :song-file-name midi-name
                              :sound-chip sound-chip
                              :output-coding output-coding
                              :catalog catalog
                              :comments-catalog comments-catalog)
      (loop for symbol being the hash-keys of catalog
            for notes = (gethash symbol catalog)
            do (write-song-data-to-ay-3-8910 notes source-out)))))

(defun compile-music-2600 (source-out-name in-file-name)
  (let ((catalog (make-hash-table))
        (comments-catalog (make-hash-table)))
    (with-output-to-file (source-out source-out-name :if-exists :supersede :if-does-not-exist :create)
      (format *trace-output* "~&Writing ~a…" source-out-name)
      (format source-out ";;; Music compiled from ~a;
;;; do not bother editing (generated file will be overwritten)"
              in-file-name)
      (dolist (output-coding '(:NTSC :PAL))
        (format *trace-output* "Music encoded for ~a TV standard…" output-coding)
        (when (eql :NTSC output-coding)
          (format source-out "~%	.if TV == NTSC~2%"))
        (when (eql :PAL output-coding)
          (format source-out "~%	.else ; PAL or SECAM"))
        (import-song-to-catalog
         :song-file-name in-file-name
         :output-coding output-coding
         :catalog catalog
         :comments-catalog comments-catalog)
        (loop for symbol being the hash-keys of catalog
              for notes = (gethash symbol catalog)
              do (write-song-data-to-file (string symbol) notes source-out)))
      (format source-out "~2%	.fi~%"))
    (format *trace-output* "~&… done.~%")
    (finish-output)))

(defun compile-music (source-out-name in-file-name
                      &optional (sound-chip "TIA")
                                (output-coding "NTSC"))
  (format *trace-output* "~&Writing music from playlist ~a…" in-file-name)
  (ecase *machine*
    (2600 (compile-music-2600 source-out-name in-file-name))
    (5200 (compile-music-7800 source-out-name in-file-name
                              (make-keyword (string-upcase sound-chip))
                              (make-keyword (string-upcase output-coding))))
    (7800 (compile-music-7800 source-out-name in-file-name
                              (make-keyword (string-upcase sound-chip))
                              (make-keyword (string-upcase output-coding))))
    (2609 (compile-music-2609 source-out-name in-file-name
                              (make-keyword (string-upcase sound-chip))
                              (make-keyword (string-upcase output-coding))))
    (35902 (compile-music-cgb source-out-name in-file-name sound-chip)) ; CGB
    (20953 (compile-music-dmg source-out-name in-file-name sound-chip)) ; DMG
    (9918 (compile-music-colecovision source-out-name in-file-name sound-chip)) ; ColecoVision
    (1000 (compile-music-sg1000 source-out-name in-file-name sound-chip)) ; SG-1000
    (3010 (compile-music-sms source-out-name in-file-name sound-chip)) ; SMS
    (837 (compile-music-sgg source-out-name in-file-name sound-chip)) ; SGG
    (3 (compile-music-nes source-out-name in-file-name sound-chip)) ; NES
    (6 (compile-music-snes source-out-name in-file-name sound-chip)) ; SNES
    (7 (compile-music-bbc source-out-name in-file-name sound-chip)) ; BBC
    (64 (compile-music-c64 source-out-name in-file-name sound-chip)) ; C=64
    (128 (compile-music-c128 source-out-name in-file-name sound-chip)) ; C=128
    (264 (compile-music-c16 source-out-name in-file-name sound-chip)) ; C=16
    (8 (compile-music-a2 source-out-name in-file-name sound-chip)) ; Apple ][
    (9 (compile-music-a3 source-out-name in-file-name sound-chip)) ; Apple ///
    (10 (compile-music-a2gs source-out-name in-file-name sound-chip)) ; Apple //gs
    (81 (compile-music-zx81 source-out-name in-file-name sound-chip)) ; ZX81
    (2068 (compile-music-spectrum source-out-name in-file-name sound-chip)))) ; Spectrum

(defvar *sec/quarter-note* 1/2)

(defun midi-track-decode (track parts/quarter)
  (format *trace-output* "~&~|~%MIDI track:~%")
  (let ((keyboard (make-array '(#x100)))
        (output (list)))
    (labels ((midi-note (&key time velocity key)
               (cond ((and key (plusp velocity))
                      #+ () (format *trace-output* "~&<start ~a at ~d>" (midi->note-name key) time)
                      (setf (aref keyboard key) (list time velocity)))
                     ((and key (zerop velocity) (aref keyboard key))
                      (let ((d (- time (first (aref keyboard key)))))
                        #+ () (format *trace-output* "~& end ~a at ~d (duration ~d)" (midi->note-name key) time d)
                        (destructuring-bind (start-time first-velocity)
                            (aref keyboard key)
                          (push (list :note :time start-time :duration d
                                            :key key :velocity first-velocity)
                                output))
                        (setf (aref keyboard key) nil)))
                     (t (warn "Dunno what to do with note time ~d velocity ~d key ~d (is ~s)"
                              time velocity key (aref keyboard key))))))
      (loop for chunk in track
            with time-signature-num = 4
            with time-signature-den = 4
            do (typecase chunk
                 (midi::text-message
                  (push (list :text
                              (substitute #\Space #\Null
                                          (slot-value chunk 'midi::text)))
                        output)
                  nil)
                 (midi::time-signature-message
                  (setf time-signature-num (midi::message-numerator chunk)
                        time-signature-den (expt 2 (midi::message-denominator chunk)))
                  (format *trace-output* " … Time signature is ~d⁄~d … "
                          time-signature-num time-signature-den)
                  nil)
                 (midi::tempo-message
                  (setf *sec/quarter-note* (/ (slot-value chunk 'midi::tempo) (expt 10 6)))
                  (format *trace-output* " … Tempo is ~s sec/quarter-note … " *sec/quarter-note*)
                  nil)
                 (midi::control-change-message nil)
                 (midi::note-on-message
                  (with-slots ((key midi::key) (time midi::time)
                               (velocity midi::velocity))
                      chunk
                    (let ((time/seconds (* *sec/quarter-note* (/ 1 parts/quarter) time)))
                      (midi-note :time time/seconds :key (- key 12) :velocity velocity))))
                 (midi:key-signature-message nil)
                 (midi::reset-all-controllers-message nil)
                 (midi:program-change-message nil)
                 (midi::midi-port-message nil)
                 (t (warn "~&Ignored (unsupported) chunk ~s" chunk)))
            do (progn (finish-output *error-output*) (finish-output *trace-output*)))
      (reverse output))))

(defun key<-midi-key (key)
  (multiple-value-bind (octave-ish note-in-octave) (floor key 12)
    (let ((note-name (nth note-in-octave '(c c♯ d d♯ e f f♯ g g♯ a a♯ b))))
      (values (1- octave-ish) note-in-octave note-name))))

(defconstant +a4/hz+ 440
  "The frequency (Hz) of the A in octave 4; by convention, 440Hz.")


(defun frame-rate->fps (frame-rate)
  (ecase frame-rate
    (:ntsc 60)
    (:secam 50)
    (:pal 60)))

(defun midi->score (input &optional _ignored)
  (declare (ignore _ignored))
  (let ((score (list)))
    (dolist (track (read-midi input))
      (let ((instrument (make-keyword (string-upcase (param-case (if (eql :text (caar track))
                                                                     (prog1
                                                                         (cadar track)
                                                                       (setf track (rest track)))
                                                                     "Piano")))))
            (lyric nil))
        (loop for token in track
              do (ecase (first token)
                   (:text (setf lyric (second token)))
                   (:note
                    (push (list* :lyric lyric :instrument instrument
                                 (rest token))
                          score)
                    (setf lyric nil))))
        (setf score (sort score (lambda (a b)
                                  (< (getf a :time 0) (getf b :time 0)))))))
    score))

(defstruct hokey-note
  start-time
  duration
  instrument
  volume
  hokey-f
  hokey-error
  tia-f
  tia-error)

(defun hokey-reckon (note instrument &optional (q 1))
  (let* ((o (get-orchestration))
         (i (loop for i* in o for i from 0
                  when (string-equal (param-case (string instrument))
                                     (param-case (string (getf i* :instrument))))
                    return i
                  finally (return 0))))
    (when note
      (multiple-value-bind (best1 best-e) (best-pokey-note-for note)
        (when (and best1 (< 0 best1 #xff))
          (return-from hokey-reckon (values i best1 best-e)))))
    (when (> q 1/2)
      (hokey-reckon note (getf (elt o (mod (1- i) (length o))) :instrument) (* q 3/4))
      (cerror "Continue, dropping this note"
              "Hokey cannot play ~a on any instrument" (midi->note-name note)))))

(defun simplify-to-rational (fraction &optional (smallest-part 1/3))
  (let* ((numerator (floor fraction smallest-part))
         (denominator (/ 1 smallest-part))
         (multiple (lcm numerator denominator))
         (den (if (plusp numerator)
                  (/ multiple numerator)
                  0))
         (num (if (plusp denominator)
                  (/ multiple denominator)
                  (/ 1 smallest-part))))
    (list num den)))

(defun fraction-nybbles (num den)
  (cond ((zerop num)
         #xf0)
        ((>= num den)
         #xf0)
        (t (logior (ash (max 0 (min 15 num)) 4)
                   (max 0 (min 15 (- den num)))))))

(defun score->hokey-notes (score frame-rate)
  (remove-if #'null
             (mapcar (lambda (score-note)
                       (let ((key (getf score-note :key)))
                         (unless (<= 24 key 72)
                           (warn "Note ~a is unlikely to play correctly on Hokey"
                                 (midi->note-name key)))
                         (multiple-value-bind (instrument hokey-f hokey-error)
                             #| FIXME: #1231 PAL |#
                             (hokey-reckon key (getf score-note :instrument))
                           (destructuring-bind (&optional _tia-c tia-f (tia-error 0))
                               #| FIXME: #1231 PAL |#
                               (best-tia-ntsc-note-for key)
                             (declare (ignore _tia-c))
                             (when (getf score-note :velocity)
                               (make-hokey-note :start-time (getf score-note :time)
                                                :duration (getf score-note :duration)
                                                :instrument instrument
                                                :hokey-f (or hokey-f 0)
                                                :hokey-error
                                                (apply #'fraction-nybbles
                                                       (simplify-to-rational
                                                        (or hokey-error #xf0)))
                                                :tia-f (or tia-f 0)
                                                :tia-error
                                                (apply #'fraction-nybbles
                                                       (simplify-to-rational
                                                        (or tia-error #xf0)))
                                                :volume (/ (getf score-note :velocity) 127)))))))
                     score)))

(defmethod score->song (score (format (eql :hokey)) frame-rate)
  (score->hokey-notes score frame-rate))

(defun pokey-distortion-for-instrument (instrument-name)
  (or (loop for row in (get-orchestration)
            when (string-equal instrument-name (getf row :instrument))
              do (return (getf row :distortion)))
      :|10|))

(defvar *orchestra* nil)

(defun get-orchestration ()
  (or *orchestra*
      (setf *orchestra* (read-orchestration))))

(defun enumerate-orchestral-instruments ()
  (let ((i 0))
    (mapcar (lambda (row)
              (prog1
                  (cons (getf row :instrument) i)
                (incf i)))
            (get-orchestration))))

(defun quieter-note (note)
  (let ((quieter (min 1 (max 0 (* 4/5 (hokey-note-volume note))))))
    (make-hokey-note :start-time (hokey-note-start-time note)
                     :duration (if (zerop (floor (* 15 quieter)))
                                   0
                                   (hokey-note-duration note))
                     :instrument (hokey-note-instrument note)
                     :volume quieter
                     :hokey-f (hokey-note-hokey-f note)
                     :hokey-error (hokey-note-hokey-error note)
                     :tia-f (hokey-note-tia-f note)
                     :tia-error (hokey-note-tia-error note))))

(defmethod calculate-duration-for ((note hokey-note) instrument-number)
  (when (< (hokey-note-duration note) 1/60); FIXME: #1231 NTSC
    (format *trace-output* "~%Dropping note at time ~d as it is too short to play (duration ~ds = ~5fs < 1/60s)"
            (hokey-note-start-time note)
            (hokey-note-duration note) (hokey-note-duration note))
    (return-from calculate-duration-for 0))
  (let* ((instrument (elt *orchestra* instrument-number))
         (total-duration (ceiling (* 60 (hokey-note-duration note)))) ; FIXME: #1231 NTSC
         (attack-duration (ceiling (/ (ceiling (* 15 (hokey-note-volume note)))
                                      (getf instrument :attack-addend))))
         (decay-duration (ceiling (getf instrument :decay-duration)))
         (sustained-volume-after-decay
           (max 1
                (- (ceiling (* 15 (hokey-note-volume note)))
                   (ceiling (* (getf instrument :decay-subtrahend)
                               (getf instrument :decay-duration))))))
         (release-duration
           (ceiling (/ sustained-volume-after-decay
                       (getf instrument :release-subtrahend))))
         (sustain-duration (- total-duration
                              attack-duration
                              decay-duration
                              release-duration)))
    #+ ()
    (format *trace-output* "~& ~3d frame~:p — A ~3d D ~3d S ~3d R ~3d (vol ~3d%)"
            total-duration
            attack-duration
            decay-duration
            sustain-duration
            release-duration
            (floor (* 100 (hokey-note-volume note))))
    (if (< sustain-duration 1)
        (let ((quieter (quieter-note note)))
          (format *trace-output* "~& Sustain duration would have been below 1 frame at ~d, reducing volume from ~d%"
                  sustain-duration (floor (* 100 (hokey-note-volume note))))
          (calculate-duration-for quieter instrument-number))
        (values (max 1 sustain-duration) note))))

(defmethod write-song-binary (hokey-notes (format (eql :hokey)) output)
  (with-output-to-file (out output :if-exists :supersede :element-type '(unsigned-byte 8))
    (let ((orchestra (enumerate-orchestral-instruments)))
      (format *trace-output* " … ~d instrument~:p in orchestra"
              (length orchestra))
      (let ((time 0)
            (note-count 0))
        (format *trace-output* "~&Writing Hokey song data to ~a (~d note~:p)"
                (enough-namestring output) (length hokey-notes))
        (terpri *trace-output*)
        (princ " 𝄞 " *trace-output*)
        (dolist (note hokey-notes)
          #+ () (format *trace-output* "~x" (max 0 (min 15 (floor (* #x10 (hokey-note-volume note))))))
          (case (random 8)
            (0 (princ "♪" *trace-output*))
            (2 (princ "𝅘𝅥" *trace-output*)))
          (let ((d-t (* 60 (- (hokey-note-start-time note) time))); NTSC XXX
                (instrument (or (hokey-note-instrument note) 0)))
            (setf time (hokey-note-start-time note))
            (multiple-value-bind (duration note) ; shadows outer NOTE
                (calculate-duration-for note instrument)
              (loop while (> d-t #xff)
                    do (progn
                         (decf d-t #xff)
                         (incf note-count)
                         (write-bytes #(#xff #xff 0 0 0 0 0 0) out)))
              (when (plusp (floor duration))
                (loop while (> duration #xff)
                      do (progn
                           (decf duration #xff)
                           (incf note-count)
                           (write-byte (floor d-t) out)
                           (write-byte #xff out)
                           (write-byte instrument out)
                           (write-byte (hokey-note-hokey-f note) out)
                           (write-byte (floor (min #xff (* #x100 (hokey-note-hokey-error note)))) out)
                           (write-byte (min 15 (round (* 15 (hokey-note-volume note)))) out)
                           (write-byte (hokey-note-tia-f note) out)
                           (write-byte (floor (min #xff (* #x100 (hokey-note-tia-error note)))) out)
                           (setf d-t #xff)))
                (incf note-count)
                (write-byte (floor d-t) out)
                (write-byte duration out)
                (write-byte instrument out)
                (write-byte (hokey-note-hokey-f note) out)
                (write-byte (hokey-note-hokey-error note) out)
                (write-byte (min 15 (round (* 15 (hokey-note-volume note)))) out)
                (write-byte (hokey-note-tia-f note) out)
                (write-byte (hokey-note-tia-error note) out)))))
        (write-bytes #(0 0 0 0 0 0 0 0) out)
        (format *trace-output* " … wrote ~:d note~:p (total ~:d bytes)"
                note-count (* 8 (1+ note-count))))
      (terpri *trace-output*))))

(defun midi-to-ay-3-8910 (midi-notes output-coding)
  "Convert MIDI notes to AY-3-8910 PSG register values for Intellivision"
  (let ((frame-rate (ecase output-coding (:ntsc 60) (:pal 50)))
        (output (list)))
    (dolist (track midi-notes)
      (let ((voice-assignments (make-array 3 :initial-element nil)) ; 3 PSG channels
            (current-time 0))
        (dolist (event track)
          (ecase (first event)
            (:note
             (destructuring-bind (&key time key duration velocity) (rest event)
               (let ((psg-channel (find-free-psg-channel voice-assignments time)))
                 (when psg-channel
                   (let ((frequency (freq<-midi-key key))
                         (period (frequency-to-ay-period frequency)))
                     ;; Store note data: (time channel period-low period-high volume duration)
                     (push (list time psg-channel (logand period #xff) (ash period -8)
                                (min 15 (floor (* 15 (/ velocity 127)))) duration)
                           output)
                     (setf (aref voice-assignments psg-channel)
                           (+ time duration)))))))))
        ;; Sort by time
        (setf output (sort output #'< :key #'first))))
    ;; Convert to array format expected by the system
    (let ((result (make-array (list (length output) 6))))
      (loop for i from 0
            for note in output
            do (destructuring-bind (time channel period-lo period-hi volume duration) note
                 (setf (aref result i 0) (floor time)) ; time in frames
                 (setf (aref result i 1) channel)       ; PSG channel (0-2)
                 (setf (aref result i 2) period-lo)     ; period low byte
                 (setf (aref result i 3) period-hi)     ; period high byte
                 (setf (aref result i 4) volume)        ; volume (0-15)
                 (setf (aref result i 5) duration)))    ; duration in frames
      (values result nil))))

(defun find-free-psg-channel (voice-assignments current-time)
  "Find the first available PSG channel"
  (loop for channel from 0 below 3
        when (or (null (aref voice-assignments channel))
                 (< (aref voice-assignments channel) current-time))
          return channel
        finally (return nil)))

(defun frequency-to-ay-period (frequency)
  "Convert frequency in Hz to AY-3-8910 period value"
  (let ((clock-frequency 2000000)) ; AY-3-8910 clock is 2MHz
    (max 1 (min #xffff (round (/ clock-frequency (* 16 frequency)))))))

(defun write-song-data-to-ay-3-8910 (notes source-out)
  "Write AY-3-8910 PSG music data to assembly source"
  (format source-out "~2%;;; AY-3-8910 PSG music data")
  (loop for i below (array-dimension notes 0)
        do (let ((time (aref notes i 0))
                 (channel (aref notes i 1))
                 (period-lo (aref notes i 2))
                 (period-hi (aref notes i 3))
                 (volume (aref notes i 4))
                 (duration (aref notes i 5)))
             (format source-out "~%	.byte ~d, ~d, $~2,'0x, $~2,'0x, ~d, ~d	; Time:~d Ch:~d Vol:~d Dur:~d"
                     time channel period-lo period-hi volume duration
                     time channel volume duration))))

(defmethod write-song-data-to-binary (notes object (machine (eql 2609)) (sound-chip (eql :ay-3-8910)))
  "Write AY-3-8910 binary data for Intellivision"
  (with-output-to-file (out object :element-type '(unsigned-byte 8)
                           :if-exists :supersede :if-does-not-exist :create)
    ;; Write header (number of notes)
    (let ((num-notes (array-dimension notes 0)))
      (write-byte (logand num-notes #xff) out)
      (write-byte (ash num-notes -8) out))
    ;; Write note data
    (loop for i below (array-dimension notes 0)
          do (let ((time (aref notes i 0))
                   (channel (aref notes i 1))
                   (period-lo (aref notes i 2))
                   (period-hi (aref notes i 3))
                   (volume (aref notes i 4))
                   (duration (aref notes i 5)))
               (write-byte time out)
               (write-byte channel out)
               (write-byte period-lo out)
               (write-byte period-hi out)
               (write-byte volume out)
               (write-byte duration out)))))

(defun compile-midi (argv0 input format frame-rate
                     &optional (output (make-pathname
                                        :name (format nil "Song.~a.~a" frame-rate (pathname-name input))
                                        :type "o"
                                        :directory '(:relative "Object" "Assets"))))
  (declare (ignore argv0))
  (midi-compile input format frame-rate output))

(defun midi-compile (input format frame-rate
                     &optional (output (make-pathname
                                        :name (format nil "Song.~a.~a" (pathname-name input) frame-rate)
                                        :type "o"
                                        :directory '(:relative "Object" "Assets"))))
  "Compile INPUT for device FORMAT with FRAME-RATE TV (eg NTSC)"
  (format *trace-output* "~&Compiling ~a to format ~a for frame rate ~a into ~a"
          (enough-namestring input) format frame-rate (enough-namestring output))
  (finish-output *trace-output*)
  (write-song-binary (score->song (midi->score input (make-keyword frame-rate))
                                  (make-keyword format) (make-keyword frame-rate))
                     (make-keyword format) output))
;; Platform-specific music compilation functions (stub implementations)
(defun compile-music-cgb (output-file input-file &optional chip)
  (error "Game Boy Color music compilation not yet implemented"))

(defun compile-music-dmg (output-file input-file &optional chip)
  (error "DMG music compilation not yet implemented"))

(defun compile-music-nes (output-file input-file &optional chip)
  (error "NES music compilation not yet implemented"))

(defun compile-music-snes (output-file input-file &optional chip)
  (error "SNES music compilation not yet implemented"))

(defun compile-music-colecovision (output-file input-file &optional chip)
  (error "ColecoVision music compilation not yet implemented"))

(defun compile-music-sg1000 (output-file input-file &optional chip)
  (error "SG-1000 music compilation not yet implemented"))

(defun compile-music-sms (output-file input-file &optional chip)
  (error "SMS music compilation not yet implemented"))

(defun compile-music-sgg (output-file input-file &optional chip)
  (error "SGG music compilation not yet implemented"))

(defun compile-music-c64 (output-file input-file &optional chip)
  "Compile music for Commodore 64 SID chipset"
  (let ((*machine* 64))
    (with-output-to-file (source output-file :if-exists :supersede :if-does-not-exist :create)
      (format *trace-output* "~&Writing SID music ~a…" output-file)
      (format source ";;; SID Music compiled from ~a~%;" input-file)
      (format source ";;; Commodore 64 SID synthesizer~2%")
      ;; Basic SID music framework - would need full MIDI parsing
      (format source "sid_init:~%")
      (format source "    lda #$00~%")
      (format source "    sta $d404  ; Voice 1 control~%")
      (format source "    sta $d40b  ; Voice 2 control~%")
      (format source "    sta $d412  ; Voice 3 control~%")
      (format source "    rts~2%")
      ;; Placeholder for actual MIDI conversion
      (format source ";;; TODO: Implement MIDI to SID conversion~%")
      (format source ";;; SID has 3 voices, each with:~%")
      (format source ";;; - Oscillator (triangle, sawtooth, pulse, noise)~%")
      (format source ";;; - ADSR envelope~%")
      (format source ";;; - Filter~%"))))

(defun compile-music-c128 (output-file input-file &optional chip)
  "Compile music for Commodore 128 (same as C64 SID)"
  (compile-music-c64 output-file input-file chip))

(defun compile-music-c16 (output-file input-file &optional chip)
  (error "C=16/Plus4 music compilation not yet implemented"))

(defun compile-music-a2 (output-file input-file &optional chip)
  "Compile music for Apple ][ with Mockingboard sound card"
  (let ((*machine* 8))
    (with-output-to-file (source output-file :if-exists :supersede :if-does-not-exist :create)
      (format *trace-output* "~&Writing Mockingboard music ~a…" output-file)
      (format source ";;; Mockingboard Music compiled from ~a~%;" input-file)
      (format source ";;; Apple ][ Mockingboard (AY-3-8910 PSG)~2%")
      ;; Mockingboard uses AY-3-8910 PSG chip, similar to Intellivision
      (format source "mock_init:~%")
      (format source "    lda #$00~%")
      (format source "    sta AY_REG  ; AY-3-8910 register select~%")
      (format source "    rts~2%")
      ;; Placeholder for actual MIDI conversion
      (format source ";;; TODO: Implement MIDI to AY-3-8910 conversion~%")
      (format source ";;; Mockingboard has 3 voices, 8 registers each~%"))))

(defun compile-music-a3 (output-file input-file &optional chip)
  (error "Apple III music compilation not yet implemented"))

(defun compile-music-a2gs (output-file input-file &optional chip)
  "Compile music for Apple IIGS enhanced sound"
  (let ((*machine* 10))
    (with-output-to-file (source output-file :if-exists :supersede :if-does-not-exist :create)
      (format *trace-output* "~&Writing Apple IIGS music ~a…" output-file)
      (format source ";;; Apple IIGS Enhanced Sound compiled from ~a~%;" input-file)
      (format source ";;; Apple IIGS has Ensoniq DOC (32 oscillator wavetable synthesis)~2%")
      ;; Apple IIGS has sophisticated sound hardware
      (format source "sound_init:~%")
      (format source "    lda #$00~%")
      (format source "    sta SOUNDCTL  ; Sound control register~%")
      (format source "    rts~2%")
      ;; Placeholder for actual MIDI conversion
      (format source ";;; TODO: Implement MIDI to Ensoniq DOC conversion~%")
      (format source ";;; IIGS has 32 oscillators, 8-bit samples, stereo~%"))))

(defun compile-music-bbc (output-file input-file &optional chip)
  (error "BBC music compilation not yet implemented"))

(defun compile-music-lynx (output-file input-file &optional chip)
  (error "Lynx music compilation not yet implemented"))

(defun compile-speech-2609 (output-file input-file)
  "Compile speech for Intellivision platform"
  (error "Intellivision speech compilation not yet implemented"))

(defun compile-music-zx81 (output-file input-file &optional chip)
  "Compile music for ZX81 EAR cassette interface"
  (let ((*machine* 81))
    (with-output-to-file (source output-file :if-exists :supersede :if-does-not-exist :create)
      (format *trace-output* "~&Writing ZX81 EAR music ~a…" output-file)
      (format source ";;; ZX81 EAR Music compiled from ~a~%;" input-file)
      (format source ";;; ZX81 cassette EAR interface (1-bit audio)~2%")
      ;; ZX81 has very limited sound capabilities via EAR cassette port
      (format source "ear_init:~%")
      (format source "    ld a, $00~%")
      (format source "    out ($fe), a  ; EAR output (bit 4 of port $fe)~%")
      (format source "    ret~2%")
      ;; Placeholder for actual MIDI conversion to 1-bit audio
      (format source ";;; TODO: Implement MIDI to 1-bit EAR audio conversion~%")
      (format source ";;; ZX81 EAR can only produce simple tones/beeps~%"))))

(defun compile-music-spectrum (output-file input-file &optional chip)
  "Compile music for ZX Spectrum beeper"
  (let ((*machine* 2068))
    (with-output-to-file (source output-file :if-exists :supersede :if-does-not-exist :create)
      (format *trace-output* "~&Writing Spectrum beeper music ~a…" output-file)
      (format source ";;; ZX Spectrum Beeper Music compiled from ~a~%;" input-file)
      (format source ";;; ZX Spectrum internal speaker (1-bit audio)~2%")
      ;; ZX Spectrum has AY-3-8912 in 128K models, but basic beeper in 48K
      (format source "beeper_init:~%")
      (format source "    ld a, $00~%")
      (format source "    out ($fe), a  ; Border color and speaker (bit 4)~%")
      (format source "    ret~2%")
      ;; Placeholder for actual MIDI conversion
      (format source ";;; TODO: Implement MIDI to 1-bit beeper audio conversion~%")
      (format source ";;; Spectrum 128K has AY-3-8912 PSG chip available~%"))))
