(in-package :skyline-tool)

(defun read-snes-art-index (index-in)
  "Read SNES art index file and return list of (mode png-path width-px height-px).
Lines must have three space-separated tokens: png-name mode dimensions (e.g. name 2bpp 16×16).
Malformed lines (e.g. missing mode) are skipped."
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading SNES art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while (and line (plusp (length line)) (not (char= #\; (char line 0))))
            do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page) line)))
                 (cond
                   ((emptyp line) nil)
                   ((char= #\# (char line 0)) nil)
                   (t (let ((tokens (split-sequence #\Space line :remove-empty-subseqs t :test #'char=)))
                        (when (= 3 (length tokens))
                          (destructuring-bind (png-name mode cell-size) tokens
                            (let ((dims (split-sequence #\× cell-size :test #'char=)))
                              (when (= 2 (length dims))
                                (destructuring-bind (width-px height-px) dims
                                  (push (list (make-keyword mode)
                                              (make-pathname :defaults index-in
                                                             :name (subseq png-name 0
                                                                           (position #\. png-name :from-end t))
                                                             :type "png")
                                              (parse-integer width-px)
                                              (parse-integer height-px))
                                        png-list))))))))))))
    (format *trace-output* " done. Got ~:D PNG files to read." (length png-list))
    (reverse png-list)))

(defun parse-into-snes-chr-data (art-index)
  "Parse PNG files into SNES CHR data (8x8 tiles with various bit depths)"
  (let ((chr-data (list)))
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&~A: parsing SNES CHR data … "
                png-name)
        (let* ((png (png-read:read-png-file png-name))
               (height (png-read:height png))
               (width (png-read:width png))
               (palette-pixels (png->palette height width
                                             (png-read:image-data png)
                                             (png-read:transparency png))))
          ;; SNES CHR format: 8x8 tiles, various bit depths (2, 4, 8)
          (let ((tile-data (parse-snes-chr-tiles palette-pixels width-px height-px mode)))
            (setf chr-data (append chr-data tile-data))))
        (format *trace-output* " done.")))
    chr-data))

(defun parse-snes-chr-tiles (palette-pixels width-px height-px mode)
  "Convert palette pixels to SNES CHR ROM format (8x8 tiles, various bit depths)"
  (let ((tiles (list))
        (tile-width (/ width-px 8))
        (tile-height (/ height-px 8))
        (bits-per-pixel (ecase mode
                          (:2bpp 2)
                          (:4bpp 4)
                          (:8bpp 8))))
    ;; Process each 8x8 tile
    (dotimes (ty tile-height)
      (dotimes (tx tile-width)
        (let ((tile-bytes (make-array (* bits-per-pixel 8) :element-type '(unsigned-byte 8) :initial-element 0)))
          ;; SNES uses interleaved bitplanes
          (dotimes (y 8)
            (dotimes (x 8)
              (let* ((px (+ (* tx 8) x))
                     (py (+ (* ty 8) y))
                     (color-index (if (and (< px width-px) (< py height-px))
                                      (mod (aref palette-pixels py px) (ash 1 bits-per-pixel))
                                      0)))
                ;; Set bits in the bitplanes
                (dotimes (bit  bits-per-pixel)
                  (when (logbitp bit color-index)
                    (let ((byte-index (+ (* bit 8) y)))
                      (setf (aref tile-bytes byte-index)
                            (logior (aref tile-bytes byte-index) (ash 1 (- 7 x))))))))))
          (push tile-bytes tiles))))
    (nreverse tiles)))

(defun write-snes-chr-rom (index-out chr-data)
  "Write SNES CHR ROM data to binary file"
  (with-output-to-file (out index-out :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
    (dolist (tile chr-data)
      (dotimes (i (length tile))
        (write-byte (aref tile i) out))))
  (format *trace-output* "~&Wrote ~:D bytes to ~A"
          (reduce #'+ (mapcar #'length chr-data)) index-out))

(defun compile-art-snes (index-out index-in)
  "Compile art assets for Super Nintendo platform"
  (let ((*machine* 88))
    (write-snes-chr-rom index-out
                        (parse-into-snes-chr-data
                         (read-snes-art-index index-in)))))

(defun blob-rip-snes-tile (png-file)
  "Extract tile data from PNG for SNES (8x8 tiles)"
  (let ((*machine* 88))
    (format *trace-output* "~&Ripping SNES tile data from ~a …" (enough-namestring png-file))
    (let* ((png (png-read:read-png-file png-file))
           (height (png-read:height png))
           (width (png-read:width png))
           (palette-pixels (png->palette height width
                                         (png-read:image-data png)
                                         (png-read:transparency png)))
           (output-file (merge-pathnames
                         (make-pathname :name (pathname-name png-file)
                                        :type "s")
                         (directory-namestring png-file))))
      (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
        (format out ";;; SNES tile data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"snes.inc\"~2%")
        (format out ".segment \"TILES\"~%")
        (format out "~a:~%" (pathname-name png-file))

        ;; Convert to SNES 2BPP tile format (16 bytes per 8x8 tile)
        (let ((tile-width (/ width 8))
              (tile-height (/ height 8)))
          (dotimes (ty tile-height)
            (dotimes (tx tile-width)
              (let ((tile-bytes (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
                ;; Extract 8x8 pixel block
                (dotimes (y 8)
                  (dotimes (x 8)
                    (let* ((px (+ (* tx 8) x))
                           (py (+ (* ty 8) y))
                           (color-index (if (and (< px width) (< py height))
                                            (mod (aref palette-pixels py px) 4)
                                            0)))
                      ;; SNES 2BPP: 2 bitplanes, 8 bytes each
                      (dotimes (bit 2)
                        (when (logbitp bit color-index)
                          (let ((byte-index (+ (* bit 8) y)))
                            (setf (aref tile-bytes byte-index)
                                  (logior (aref tile-bytes byte-index) (ash 1 (- 7 x))))))))))
                ;; Write tile data
                (dotimes (i 16)
                  (format out "    .byte $~2,'0X~%" (aref tile-bytes i)))))))
        (format out "~%;;; End of tile data~%"))
      (format *trace-output* " done. Wrote ~:D tiles." (* (/ width 8) (/ height 8))))))

(defun blob-rip-snes-sprite (png-file)
  "Extract sprite data from PNG for SNES"
  ;; For SNES sprites, we can use the tile format but mark as sprite data
  (let ((*machine* 88))
    (format *trace-output* "~&Ripping SNES sprite data from ~a …" (enough-namestring png-file))
    (let ((output-file (merge-pathnames
                        (make-pathname :name (pathname-name png-file)
                                       :type "s")
                        (directory-namestring png-file))))
      (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
        (format out ";;; SNES sprite data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"snes.inc\"~2%")
        (format out ".segment \"SPRITES\"~%")
        (format out "~a:~%" (pathname-name png-file))
        (format out "    ;;; Sprite data - using tile format for now~%"))
      ;; Use tile ripping as base, then add sprite-specific metadata
      (blob-rip-snes-tile png-file))))

(defun blob-rip-snes-font (png-file)
  "Extract font data from PNG for SNES"
  (let ((*machine* 88))
    (format *trace-output* "~&Ripping SNES font data from ~a …" (enough-namestring png-file))
    (let ((output-file (merge-pathnames
                        (make-pathname :name (pathname-name png-file)
                                       :type "s")
                        (directory-namestring png-file))))
      (with-output-to-file (out output-file :if-exists :supersede :if-does-not-exist :create)
        (format out ";;; SNES font data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"snes.inc\"~2%")
        (format out ".segment \"FONT\"~%")
        (format out "~a:~%" (pathname-name png-file))
        (format out "    ;;; Font data - 8x8 or 8x16 characters~%"))
      ;; Use tile ripping as base for font characters
      (blob-rip-snes-tile png-file))))
