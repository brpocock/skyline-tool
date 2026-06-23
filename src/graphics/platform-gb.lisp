(in-package :skyline-tool)

;;; Game Boy Color (CGB) Graphics Support

(defun compile-art-cgb (index-out index-in)
  "Compile art assets for Game Boy Color platform"
  (let ((*machine* 20953))
    (write-cgb-tile-data index-out
                         (parse-into-cgb-tile-data
                          (read-cgb-art-index index-in)))))

(defun read-cgb-art-index (index-in)
  "Read CGB art index file and return list of (png-name width-px height-px palette-mode)"
  (let ((png-list (list)))
    (format *trace-output* "~&CGB: reading art index ~A…" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while line
            do (let ((trimmed (string-trim " " line)))
                 (when (and (> (length trimmed) 0)
                            (not (char= (char trimmed 0) #\;)))
                   (let* ((parts (split-sequence #\space trimmed :remove-empty-subseqs t))
                          (png-name (first parts))
                          (width (parse-integer (second parts)))
                          (height (parse-integer (third parts)))
                          (palette-mode (or (fourth parts) "bg"))) ; bg or obj
                     (push (list png-name width height palette-mode) png-list))))))
    (reverse png-list)))

(defun parse-into-cgb-tile-data (art-index-entries)
  "Parse art index entries into CGB tile data format"
  (let ((tile-data (make-array 0 :adjustable t :fill-pointer 0)))
    (dolist (entry art-index-entries)
      (destructuring-bind (png-name width height palette-mode) entry
        (let* ((image (load-png-image png-name))
               (tiles-wide (/ width 8))
               (tiles-high (/ height 8)))
          (dotimes (ty tiles-high)
            (dotimes (tx tiles-wide)
              (let ((tile-pixels (extract-region image (* tx 8) (* ty 8)
                                                 (+ (* tx 8) 7) (+ (* ty 8) 7))))
                (let ((tile-bytes (cgb-tile-to-bytes tile-pixels palette-mode)))
                  (dotimes (i (length tile-bytes))
                    (vector-push-extend (aref tile-bytes i) tile-data)))))))))
    tile-data))

(defun cgb-tile-to-bytes (tile-pixels palette-mode)
  "Convert 8x8 tile pixels to CGB tile bytes (2BPP)"
  (let ((bytes (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    (dotimes (y 8)
      (let ((low-byte 0)
            (high-byte 0))
        (dotimes (x 8)
          (let* ((pixel (aref tile-pixels y x))
                 (palette-index (if (string= palette-mode "obj")
                                    (min pixel 3)  ; OBJ palettes are 0-3
                                    pixel)))       ; BG palettes can use 0-3
            (when (logbitp 0 palette-index)
              (setf low-byte (logior low-byte (ash 1 (- 7 x)))))
            (when (logbitp 1 palette-index)
              (setf high-byte (logior high-byte (ash 1 (- 7 x)))))))
        (setf (aref bytes (* y 2)) low-byte)
        (setf (aref bytes (1+ (* y 2))) high-byte)))
    bytes))

(defun write-cgb-tile-data (output-file tile-data)
  "Write CGB tile data to output file"
  (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
    (format out ";;; CGB Tile Data~%")
    (format out ";;; Generated automatically from art assets~2%")
    (let ((address 0))
      (dotimes (i (length tile-data))
        (when (zerop (mod address 16))
          (format out "~%tile_data_~4,'0x:" address))
        (format out " ~2,'0x" (aref tile-data i))
        (incf address))
      (format out "~2%"))))
(defun compile-sgb-frame (frame-out frame-in)
  "Compile Super Game Boy frame/border graphics"
  (let ((*machine* 35902)) ; SGB works with both DMG and CGB
    (write-sgb-frame-data frame-out
                          (parse-sgb-frame frame-in))))

(defun parse-sgb-frame (frame-in)
  "Parse SGB frame PNG file into SGB packet format"
  (let ((png (png-read:read-png-file frame-in)))
    (format *trace-output* "~&SGB: parsing frame from ~a …" (enough-namestring frame-in))
    (let* ((height (png-read:height png))
           (width (png-read:width png))
           (palette-pixels (png->palette (png-read:image-data png)
                                         (png-read:transparency png)))
           (frame-data (make-array (* height width) :element-type '(unsigned-byte 8))))
      ;; SGB frames are typically 256x224 pixels
      ;; Convert to SGB packet format
      (dotimes (y height)
        (dotimes (x width)
          (let ((pixel-index (aref palette-pixels y x)))
            (setf (aref frame-data (+ (* y width) x)) pixel-index))))
      (format *trace-output* " done.")
      frame-data)))

(defun write-sgb-frame-data (output-file frame-data)
  "Write SGB frame data in packet format for SNES transmission"
  (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
    (format out ";;; SGB Frame Data compiled from ~a~%;;; Generated automatically for Super Game Boy~2%" output-file)
    (format out ".include \"sgb.inc\"~2%")
    (format out ".segment \"SGB_FRAME\"~%")
    (format out "sgb_frame_data:~%")

    ;; SGB frame data is sent as packets to the SNES
    ;; Each packet can contain up to 15 bytes of data
    (let ((data-size (length frame-data))
          (packet-count 0))
      (dotimes (i (ceiling data-size 15))
        (let ((packet-start (* i 15))
              (packet-end (min (+ (* i 15) 15) data-size)))
          (format out "~%;;; Packet ~d~%" packet-count)
          (format out "    .byte $01  ; SGB packet command (data transfer)~%")
          (format out "    .byte ~d   ; Packet length~%" (- packet-end packet-start))
          (dotimes (j (- packet-end packet-start))
            (if (zerop (mod (+ (* i 15) j) 16))
                (format out "~%    .byte $~2,'0x" (aref frame-data (+ (* i 15) j)))
                (format out ", $~2,'0x" (aref frame-data (+ (* i 15) j)))))
          (incf packet-count))))

    (format out "~2%;;; End of SGB frame data~%")
    (format out "    .byte $00  ; End marker~2%")))
(defun parse-into-gb-tile-data (art-index &key color)
  "Parse PNG files into Game Boy tile data"
  (let ((all-tiles (list)))
    (dolist (art-item art-index)
      (destructuring-bind (png-name width-tiles height-tiles) art-item
        (format *trace-output* "~&~A: parsing Game Boy tile data (~Dx~D tiles) … "
                png-name width-tiles height-tiles)
        (let* ((png (png-read:read-png-file png-name))
               (height (png-read:height png))
               (width (png-read:width png))
               (palette-pixels (png->palette (png-read:image-data png)
                                             (png-read:transparency png)))
               (palette (if color
                            +gameboy-color-palette+
                            +gameboy-palette+)))
          ;; Convert to Game Boy tile format
          (let ((tile-data (parse-gb-tile-data palette-pixels width height palette)))
            (setf all-tiles (append all-tiles tile-data))))))
    all-tiles))

(defun write-gb-tile-data (index-out tile-data)
  "Write Game Boy tile data to binary file"
  (with-output-to-file (out index-out :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
    (dolist (tile tile-data)
      (dotimes (i 16)
        (write-byte (aref tile i) out))))
  (format *trace-output* "~&Wrote ~:D bytes (~:D tiles) to ~A"
          (* (length tile-data) 16) (length tile-data) index-out))

(defun read-dmg-art-index (index-in)
  "Read DMG art index file; return list of (png-name width-tiles height-tiles &optional color)."
  (let ((png-list (list)))
    (format *trace-output* "~&DMG: reading art index ~A…" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while line
            do (let ((trimmed (string-trim " " line)))
                 (when (and (> (length trimmed) 0)
                            (not (char= (char trimmed 0) #\#))
                            (not (char= (char trimmed 0) #\;)))
                   (let* ((parts (split-sequence #\Space trimmed :remove-empty-subseqs t))
                          (png-name (first parts))
                          (width (parse-integer (second parts)))
                          (height (parse-integer (third parts)))
                          (color (ignore-errors (find (fourth parts) '("color" "cgb") :test #'string-equal))))
                     (push (list png-name width height color) png-list))))))
    (reverse png-list)))

(defun compile-art-dmg (index-out index-in)
  "Compile art assets for Game Boy DMG platform."
  (let ((*machine* 35902))
    (let ((art-index (read-dmg-art-index index-in)))
      (write-gb-tile-data index-out
                          (parse-into-gb-tile-data art-index
                                                   :color (some #'fourth art-index))))))
