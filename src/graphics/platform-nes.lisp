(in-package :skyline-tool)

(defun compile-art-nes (index-out index-in)
  "Compile art assets for Nintendo Entertainment System platform"
  (let ((*machine* 3))
    (write-nes-chr-rom index-out
                       (parse-into-nes-chr-data
                        (read-nes-art-index index-in)))))
(defun grab-nes-palette (mode palette-pixels)
  "Extract NES palette from the bottom of the image"
  (declare (ignore mode))
  ;; NES expects 4 palettes, each with 4 colors (background + 3 sprite colors)
  ;; The palette key at the bottom contains palette data laid out as:
  ;; background, color1, color2, color3, background, color1, color2, color3, ...
  ;; for each of the 4 palettes
  (let* ((height (array-dimension palette-pixels 0))
         (width (array-dimension palette-pixels 1))
         (last-row (1- height))
         (palettes (list)))
    ;; Extract 4 palettes from the bottom row: Each palette takes colors
    ;; sequentially: bg, p1c1, p1c2, p1c3, bg, p2c1, p2c2, p2c3, ...
    (dotimes (palette-index 4)
      (let ((palette-colors (list))
            (background-color (aref palette-pixels last-row 0)))
        ;; Collect colors for  this palette (up to 16  pixels worth, but
        ;; we'll use first 4)
        (loop for x from (* palette-index 16) ;; 16 pixels per palette block
              while (or (>= x width) (>= x (* (1+ palette-index) 16)))
              do (let ((color (aref palette-pixels last-row x)))
                   (push color palette-colors)))
        ;; Reverse to get correct order
        (setf palette-colors (nreverse palette-colors))
        ;; For NES, we only use the first 4 colors, starting with background
        (let ((nes-colors (subseq palette-colors 0 (min 4 (length palette-colors)))))
          ;; Ensure we have exactly 4 colors
          (loop while (< (length nes-colors) 4)
                do (push (or background-color 0) nes-colors))
          (push nes-colors palettes))))
    ;; Return the 4 palettes
    (nreverse palettes)))

(defun parse-into-nes-chr-data (art-index)
  "Parse PNG files into NES CHR ROM data (8x8 tiles with 2-bit color)"
  (let ((chr-data (list)))
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&~A: parsing NES CHR data in mode ~A … "
                png-name mode)
        (let* ((png (png-read:read-png-file png-name))
               (height (png-read:height png))
               (width (png-read:width png))
               (palette-pixels (png->palette (png-read:image-data png)
                                             (png-read:transparency png)))
               (palette (grab-nes-palette mode palette-pixels)))
          ;; NES CHR ROM format: 8x8 tiles, 2 bits per pixel
          (let ((tile-data (parse-nes-chr-tiles palette-pixels width-px height-px palette)))
            (setf chr-data (append chr-data tile-data)))))
      (format *trace-output* " done."))
    chr-data))


(defun read-nes-art-index (index-in)
  "Read NES art index file and return list of (png-name mode width-px height-px)"
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading NES art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while (and line (plusp (length line)) (not (char= #\; (char line 0))))
            do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page) line)))
                 (cond
                   ((emptyp line) nil)
                   ((char= #\# (char line 0)) nil)
                   (t (destructuring-bind (png-name mode cell-size)
                          (split-sequence #\Space line :remove-empty-subseqs t :test #'char=)
                        (destructuring-bind (width-px height-px)
                            (split-sequence #\× cell-size :test #'char=)
                          (push (list (make-keyword mode)
                                      (make-pathname :defaults index-in
                                                     :name (subseq png-name 0
                                                                   (position #\. png-name :from-end t))
                                                     :type "png")
                                      (parse-integer width-px)
                                      (parse-integer height-px))
                                png-list))))))))
    (format *trace-output* " done. Got ~:D PNG files to read." (length png-list))
    (reverse png-list)))

(defun parse-nes-chr-tiles (palette-pixels width-px height-px palette)
  "Convert palette pixels to NES CHR ROM format (8x8 tiles, 2 bits per pixel)"
  (let ((tiles (list))
        (tile-width (/ width-px 8))
        (tile-height (/ height-px 8)))
    ;; Create color index mapping (palette position -> NES color index 0-3)
    (let ((color-map (make-hash-table :test #'equal)))
      (dotimes (i (length palette))
        (setf (gethash (nth i palette) color-map) i))
      ;; Process each 8x8 tile
      (dotimes (ty tile-height)
        (dotimes (tx tile-width)
          (let ((tile-bytes (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
            ;; Convert 8x8 pixels to 16 bytes (2 bitplanes)
            (dotimes (y 8)
              (dotimes (x 8)
                (let* ((px (+ (* tx 8) x))
                       (py (+ (* ty 8) y))
                       (palette-color (if (and (< px width-px) (< py height-px))
                                          (aref palette-pixels py px)
                                          (first palette)))  ; default to first palette color
                       (color-index (gethash palette-color color-map 0))  ; map to 0-3
                       (bit0 (if (logbitp 0 color-index) 1 0))
                       (bit1 (if (logbitp 1 color-index) 1 0)))
                  ;; Set bits in the two bitplanes (LSB first for NES)
                  (when (= bit0 1)
                    (setf (aref tile-bytes y)
                          (logior (aref tile-bytes y) (ash 1 (- 7 x)))))
                  (when (= bit1 1)
                    (setf (aref tile-bytes (+ y 8))
                          (logior (aref tile-bytes (+ y 8)) (ash 1 (- 7 x))))))))
            (push tile-bytes tiles)))))
    (nreverse tiles)))

(defun write-nes-chr-rom (index-out chr-data)
  "Write NES CHR ROM data to binary file"
  (with-output-to-file (out index-out :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
    (dolist (tile chr-data)
      (dotimes (i 16)
        (write-byte (aref tile i) out))))
  (format *trace-output* "~&Wrote ~:D bytes to ~A" (* (length chr-data) 16) index-out))
