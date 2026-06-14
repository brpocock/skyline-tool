(in-package :skyline-tool)

(defun blob-rip-264-bitmap (png-file)
  "Extract bitmap data from PNG for TED"
  (let ((*machine* 264))
    (format *trace-output* "~&Ripping TED bitmap data from ~a …" (enough-namestring png-file))
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
        (format out ";;; TED bitmap data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"ted.inc\"~2%")
        (format out ".segment \"BITMAP\"~%")
        (format out "~a:~%" (pathname-name png-file))

        ;; Convert to TED bitmap format (320x200, 2 colors per 8x8 cell)
        (dotimes (cell-y 25)  ; 200/8 = 25 rows
          (dotimes (cell-x 40) ; 320/8 = 40 columns
            (dotimes (row 8)   ; 8 rows per cell
              (let ((bitmap-byte 0))
                (dotimes (col 8) ; 8 columns per row
                  (let* ((x (+ (* cell-x 8) col))
                         (y (+ (* cell-y 8) row))
                         (color-index (if (and (< x width) (< y height))
                                          (aref palette-pixels x y)
                                          0)))
                    ;; Use color > 0 as foreground (bit set)
                    (when (> color-index 0)
                      (setf bitmap-byte (logior bitmap-byte (ash 1 (- 7 col)))))))
                (format out "    .byte $~2,'0X~%" bitmap-byte))))))
      (format *trace-output* " done. Wrote TED bitmap data."))))

(defun blob-rip-264-sprite (png-file)
  "Extract sprite data from PNG for TED"
  (let ((*machine* 264))
    (format *trace-output* "~&Ripping TED sprite data from ~a …" (enough-namestring png-file))
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
        (format out ";;; TED sprite data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"ted.inc\"~2%")
        (format out ".segment \"SPRITES\"~%")
        (format out "~a:~%" (pathname-name png-file))

        ;; TED sprites are 24x21 pixels (63 bytes)
        (dotimes (row height)
          (dotimes (byte 3)  ; 3 bytes per row (24 pixels)
            (let ((sprite-byte 0))
              (dotimes (bit 8)
                (let* ((x (+ (* byte 8) bit))
                       (color-index (if (< x width)
                                        (aref palette-pixels x row)
                                        0)))
                  (when (> color-index 0)
                    (setf sprite-byte (logior sprite-byte (ash 1 (- 7 bit)))))))
              (format out "    .byte $~2,'0X~%" sprite-byte)))))
      (format *trace-output* " done. Wrote TED sprite data."))))

(defun blob-rip-264-font (png-file)
  "Extract font data from PNG for TED"
  (let ((*machine* 264))
    (format *trace-output* "~&Ripping TED font data from ~a …" (enough-namestring png-file))
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
        (format out ";;; TED font data ripped from ~a~%;;; Generated automatically~2%" png-file)
        (format out ".include \"ted.inc\"~2%")
        (format out ".segment \"FONT\"~%")
        (format out "~a:~%" (pathname-name png-file))

        ;; TED character set: 8x8 characters
        (let ((chars-wide (/ width 8))
              (chars-high (/ height 8)))
          (dotimes (char-y chars-high)
            (dotimes (char-x chars-wide)
              (format out "~%    ;; Character (~D,~D)~%" char-x char-y)
              (dotimes (row 8)
                (let ((font-byte 0))
                  (dotimes (col 8)
                    (let* ((x (+ (* char-x 8) col))
                           (y (+ (* char-y 8) row))
                           (color-index (if (and (< x width) (< y height))
                                            (aref palette-pixels x y)
                                            0)))
                      (when (> color-index 0)
                        (setf font-byte (logior font-byte (ash 1 (- 7 col)))))))
                  (format out "    .byte $~2,'0X~%" font-byte)))))))
      (format *trace-output* " done. Wrote TED font data."))))
(defun compile-art-264 (index-out index-in)
  "Compile art assets for Commodore 16/Plus4 (TED) platform"
  (let ((*machine* 264))
    (write-ted-art-index index-out
                         (read-ted-art-index index-in))))

(defun read-ted-art-index (index-in)
  "Read TED art index file and return list of (png-name mode width-px height-px)"
  (let ((png-list (list)))
    (format *trace-output* "~&TED: reading art index ~A…" (enough-namestring index-in))
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
                                png-list)))))))
      (format *trace-output* " done. Got ~:D PNG files to read." (length png-list))
      (reverse png-list))))

(defgeneric compile-ted-art-by-mode (mode png-name directory height-px width-px)
  (:documentation "Compile TED art based on mode using EQL specializers")
  (:method ((mode (eql :bitmap)) png-name directory height-px width-px)
    (compile-ted-bitmap png-name directory height-px width-px
                        (png->palette width-px height-px
                                      (png-read:image-data (png-read:read-png-file png-name))
                                      (png-read:transparency (png-read:read-png-file png-name)))))
  (:method ((mode (eql :chars)) png-name directory height-px width-px)
    (compile-ted-charmap png-name directory height-px width-px
                         (png->palette width-px height-px
                                       (png-read:image-data (png-read:read-png-file png-name))
                                       (png-read:transparency (png-read:read-png-file png-name)))))
  (:method ((mode (eql :sprite)) png-name directory height-px width-px)
    (compile-ted-sprite png-name directory height-px width-px
                        (png->palette width-px height-px
                                      (png-read:image-data (png-read:read-png-file png-name))
                                      (png-read:transparency (png-read:read-png-file png-name)))))
  (:method ((mode (eql :multicolor-sprite)) png-name directory height-px width-px)
    (compile-ted-multicolor-sprite png-name directory height-px width-px
                                   (png->palette width-px height-px
                                                 (png-read:image-data (png-read:read-png-file png-name))
                                                 (png-read:transparency (png-read:read-png-file png-name))))))

(defun write-ted-art-index (index-out art-index)
  "Write TED art data to output file"
  (with-output-to-file (out index-out :if-exists :supersede :if-does-not-exist :create)
    (format out ";;; TED Art Assets compiled
;;; Generated automatically
~2%")
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&Processing TED art: ~A (~Dx~D)…" png-name width-px height-px)
        ;; Dispatch to appropriate compilation function based on mode
        (compile-ted-art-by-mode mode png-name (directory-namestring index-out) height-px width-px))))
  (format *trace-output* "~&TED art compilation complete."))
(defun tile-cell-vic2-x (cell width)
  "Each tile's data is arranged into four cells, like so:

 0 1
 2 3

This gives the X position of the top-left corner of a 16×16 pixel tile
cell (where the cell's number is (+ (* tile 4) cell)) within an image
of the given width."
  (mod (+ (* (floor cell 4) 16)
          (* (mod cell 2) 8))
       width))

(defun tile-cell-vic2-y (cell width)
  (+ (* (floor (floor cell 4) (floor width 16)) 16)
     ;; even cells are on alternate rows
     (* (mod cell 2) 8)))

;;; Unit tests. This actually took me a while to get right!

(dotimes (i 62)
  (assert (= (tile-cell-vic2-y (* i 4) 16) (* 16 i)) nil
          "Tile ~D in 16px image should start at ~D, but TILE-CELL-VIC2-Y reports ~D"
          i (* 16 i) (tile-cell-vic2-y (* 4 i) 16)))

(loop for width in '(16 32 64 128)
      do (dotimes (i #xff)
           (assert (> (/ 16384 width) (tile-cell-vic2-y i width))
                   nil "The TILE-CELL-VIC2-Y function must return a valid value;
value ~D for tile-cell ~D is too far down for an image with width ~D" (tile-cell-vic2-y i width) i width)))

(defun compile-atari-8×8 (png-file target-dir height width)
  (let ((out-file (merge-pathnames
                   (make-pathname :name
                                  (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; This is a generated file. Editing is futile.~2%")
      (loop for x1 from 0 below width by 8
            for y1 from 0 below height by 8
            for i from 0
            do (loop for y0 from 7 downto 0
                     do (format src-file "~t.byte %~0,8b" 0))))))

(defun bits-to-art (byte)
(defun vic2-cell-multicolor-map (tile-data colors)
  "Build a 3-color map for multicolor encoding of an 8×8 cell.

Returns a list of up to 3 color palette indices: the background (always
index 0), the most frequent non-zero color, and the second-most-frequent.
Used by VIC-II multicolor character mode: bit-pair 0 = bg, 1 = color1,
2 = color2."
  (let ((freq (make-hash-table)))
    (loop for y from 0 below 8
          do (loop for x from 0 below 8
                   for c = (aref tile-data x y)
                   unless (zerop c)
                     do (incf (gethash c freq 0))))
    (let* ((sorted (sort (loop for c being the hash-keys of freq
                               collect (cons c (gethash c freq)))
                         #'> :key #'cdr))
           (bg 0)
           (c1 (first (first sorted)))
           (c2 (first (second sorted))))
      (list bg c1 c2))))

(defun compile-c64-blob (png-file target-dir height width palette-pixels)
  "Write a C64 character-cell full-screen blob: char data + screen map + color RAM.

The image (ideally 320×200 px) is divided into 40×25 8×8 character cells.
Duplicate cells share the same char definition (up to 256 unique).  Output
@file{.s} assembly with labels @code{<stem>BlobChars}, @code{<stem>BlobScreen},
and @code{<stem>BlobColorRAM}.

@table @asis
@item PNG-FILE
Source PNG path (for comments).
@item TARGET-DIR
Directory for the @file{.s} output.
@item HEIGHT, WIDTH
Pixel dimensions (cropped to multiples of 8).
@item PALETTE-PIXELS
2D palette-pixel array.
@end table"
  (declare (ignore height width))
  (setf width (- width (mod width 8)))
  (setf height (- height (mod height 8)))
  (when (or (< width 8) (< height 8))
    (error "C64 blob ~A too small: ~D×~D" png-file width height))
  (let* ((cols (/ width 8))
         (rows (/ height 8))
         (total-cells (* cols rows))
         (uniq (make-array 256 :adjustable t :fill-pointer 0))
         (ht (make-hash-table :test 'equal))
         (screen (make-array total-cells :element-type '(unsigned-byte 8)))
         (cram (make-array total-cells :element-type '(unsigned-byte 8)))
         (stem (pathname-name (merge-pathnames png-file))))
    ;; Dedup char cells
    (loop for idx from 0 below total-cells
          for row = (floor idx cols)
          for col = (mod idx cols)
          for sx = (* col 8)
          for sy = (* row 8)
          for key = (cell-binary-key-from-image palette-pixels sx sy 8 8)
          for char-id = (gethash key ht)
          do (unless char-id
               (setf char-id (length uniq))
               (setf (gethash key ht) char-id)
               (vector-push-extend key uniq))
             (setf (aref screen idx) char-id)
             (setf (aref cram idx) (dominant-color palette-pixels sx sy 8 8 1)))
    ;; Reduce if char table overflows 256
    (when (> (length uniq) 256)
      (warn "C64 blob ~A: ~D unique chars, reducing to 256 via H-distance..."
            png-file (length uniq))
      (reduce-tile-set uniq screen :max-slots 256))
    ;; Write assembly
    (let ((out-file (merge-pathnames
                     (make-pathname :name (concatenate 'string "Blob." stem)
                                    :type "s")
                     target-dir)))
      (ensure-directories-exist (directory-namestring out-file))
      (with-output-to-file (src out-file :if-exists :supersede
                                         :external-format :utf-8)
        (format src ";;; C64 blob: ~A~%" png-file)
        (format src ";;; Grid: ~D×~D chars; ~D unique char~:P~2%" cols rows (length uniq))
        (format src "~ABlobChars:~%" stem)
        (loop for u from 0 below (length uniq)
              for row-bytes = (aref uniq u)
              do (loop for byte in row-bytes
                       do (format src "    .byte $~2,'0X~%" byte)))
        (format src "~ABlobScreen:~%" stem)
        (loop for i from 0 below total-cells
              do (format src "    .byte ~D~%" (aref screen i)))
        (format src "~ABlobColorRAM:~%" stem)
        (loop for i from 0 below total-cells
              do (format src "    .byte ~D~%" (aref cram i))))
      (format *trace-output* "~&Wrote C64 blob to ~A." out-file))))

(defun compile-vdc-blob (png-file target-dir height width palette-pixels)
  "Write a C128 VDC bitmap blob: 640-wide or 320-wide, 200 or 400 rows.

VDC bitmap mode stores one byte per 8 horizontal pixels, linear row-major.
Width is rounded down to a byte multiple; height is rounded down to an even
number of rows (200 or 400 for interlaced VDC).

Output @file{.s} assembly with @code{<stem>VDCBlobBitmap} taking
@code{VDC_Blob_Width} bytes per row.

@table @asis
@item PNG-FILE
Source PNG path (for comments).
@item TARGET-DIR
Directory for the output assembly file.
@item HEIGHT
Pixel height.
@item WIDTH
Pixel width (640 or 320).
@item PALETTE-PIXELS
2D palette-pixel array.
@end table"
  (let* ((stem (pathname-name (merge-pathnames png-file)))
         (byte-width (floor width 8))
         (rows height))
    (setf byte-width (max byte-width 1))
    (when (or (< byte-width 1) (< rows 1))
      (error "VDC blob ~A too small: ~D×~D (need at least 8×1)" png-file width height))
    (let ((out-file (merge-pathnames
                     (make-pathname :name (concatenate 'string "Blob." stem)
                                    :type "s")
                     target-dir)))
      (ensure-directories-exist (directory-namestring out-file))
      (with-output-to-file (src out-file :if-exists :supersede
                                         :external-format :utf-8)
        (format src ";;; VDC bitmap blob: ~A~%" png-file)
        (format src ";;; ~D×~D px → ~D bytes/row × ~D rows~2%" width height byte-width rows)
        (format src "~AVDCBlobWidth     EQU ~D~%" stem byte-width)
        (format src "~AVDCBlobHeight    EQU ~D~2%" stem rows)
        (format src "~AVDCBlobBitmap:~%" stem)
        (loop for y from 0 below rows
              do (loop for byte-x from 0 below byte-width
                       for byte = (loop for bit from 0 below 8
                                        for px = (+ (* byte-x 8) bit)
                                        for c = (if (< px width) (aref palette-pixels px y) 0)
                                        sum (if (zerop c) 0 (ash 1 (- 7 bit))))
                       do (format src "    .byte $~2,'0X~%" byte))
                 (format src "    ;; row ~D~%" y)))
      (format *trace-output* "~&Wrote VDC blob to ~A." out-file))))

(defun monochrome-lines-p (palette-pixels height width)
  (every
   #'identity
   (loop for row from 0 below height
         for colors = (remove-duplicates
                       (remove-if
                        #'zerop
                        (loop for column from 0 below width
                              collect (aref palette-pixels column row)))
                       :test #'=)
         collect (or (null colors)
                     (= 1 (length colors))))))


