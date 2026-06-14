(in-package :skyline-tool)

(defun 7800-image-to-320a (image &key byte-width height palette best-fit-p)
  "@cindex graphics conversion
@cindex 320A mode
@cindex monochrome graphics

@table @code
@item Package: skyline-tool
@item Arguments: &key image (2D array), byte-width (integer), height (integer), palette (vector), best-fit-p (boolean)
@item Returns: list of byte lists
@item Side Effects: none
@end table

Convert image data to 320A mode bytes for monochrome graphics display.

@strong{320A Mode Characteristics:}
@itemize
@item 8 pixels per byte (1 bit per pixel)
@item Monochrome display (foreground over transparent)
@item 320 pixels horizontal resolution
@item 1 bit per pixel = 1/8 byte per pixel
@item Pixel value 0 = transparent, value 1 = foreground color
@end itemize

@strong{Conversion Process:}
@itemize
@item Extract 8-pixel wide columns from image
@item Map pixels to palette indices (0 or 1)
@item Pack 8 bits into single byte (MSB left)
@item Return list of byte rows for each column
@end itemize

Used internally by BLOB ripping for monochrome stamp conversion."
  (let ((bytes-across (list)))
    (dotimes (b byte-width)
      (let ((bytes (list)))
        (dotimes (y height)
          (let ((byte-pixels (extract-region image
                                             (* b 8) y
                                             (1- (* (1+ b) 8)) y)))
            ;; For 320A, treat as monochrome - convert to 0/1 based on palette
            (let ((indices (pixels-into-palette byte-pixels palette
                                                :x0 (* b 8) :y0 y
                                                :best-fit-p best-fit-p)))
              (push (reduce #'logior
                            (mapcar (lambda (bit)
                                      (ash (if (zerop (aref indices bit)) 0 1)
                                           (- 7 bit)))
                                    '(7 6 5 4 3 2 1 0)))
                    bytes))))
        (push (reverse bytes) bytes-across)))
    (reverse bytes-across)))

(defun 7800-image-to-320c (image &key byte-width height palette best-fit-p)
  "@cindex graphics conversion
@cindex 320C mode
@cindex color graphics

@table @code
@item Package: skyline-tool
@item Arguments: &key image (2D array), byte-width (integer), height (integer), palette (vector), best-fit-p (boolean)
@item Returns: list of byte lists
@item Side Effects: none
@end table

Convert image data to 320C mode bytes for 5-color graphics display.

@strong{320C Mode Characteristics:}
@itemize
@item 4 pixels per byte with embedded palette information
@item Complex palette encoding using P2, D3, D2, D1, D0 bits
@item Pixel data in D7, D6, D5, D4 bits (on=palette color 2, off=transparent)
@item 320 pixels horizontal resolution
@item Allows multiple palettes per scanline
@item Non-standard encoding with palette+graphics data mixed
@end itemize

@strong{Conversion Process:}
@itemize
@item Extract 4-pixel wide columns from image
@item Analyze palette requirements for pixel pairs
@item Encode palette information in D3, D2, D1, D0 bits
@item Encode pixel on/off states in D7, D6, D5, D4 bits
@item Pack complex encoding into single byte per 4 pixels
@item Return list of byte rows for each column
@end itemize

Used internally by BLOB ripping for color stamp conversion."
  (let ((bytes-across (list)))
    (dotimes (b byte-width)
      (let ((bytes (list)))
        (dotimes (y height)
          (let* ((byte-pixels (extract-region image
                                              (* b 4) y
                                              (1- (* (1+ b) 4)) y))
                 (indices (pixels-into-palette byte-pixels palette
                                               :x0 (* b 4) :y0 y
                                               :best-fit-p best-fit-p))
                 ;; 320C encoding: D7-D4 = foreground bits (low bit of each pixel)
                 ;; D3-D2 = palette select for pixel pair (0,1)
                 ;; D1-D0 = palette select for pixel pair (2,3)
                 ;; Palette pair select: if both pixels in pair are 0 → 0
                 ;;                      if first pixel is 0 → C2 bits of second
                 ;;                      otherwise → C2 bits of first
                 ;; C2 bits = (pixel & #x06) >> 1 (bits 1-2 of palette index)
                 (px-pair-palette (mapcar (lambda (pair)
                                            (cond
                                              ((and (zerop (car pair)) (zerop (cdr pair)))
                                               0)
                                              ((zerop (car pair))
                                               (ash (logand (cdr pair) #x06) -1))
                                              (t
                                               (ash (logand (car pair) #x06) -1))))
                                          (list (cons (aref indices 0) (aref indices 1))
                                                (cons (aref indices 2) (aref indices 3))))))
            (push (logior
                   (ash (logand (aref indices 0) #x01) 7)
                   (ash (logand (aref indices 1) #x01) 6)
                   (ash (logand (aref indices 2) #x01) 5)
                   (ash (logand (aref indices 3) #x01) 4)
                   (ash (first px-pair-palette) 2)
                   (second px-pair-palette))
                  bytes)))
        (push (reverse bytes) bytes-across)))
    (reverse bytes-across)))
(defun convert-stamp-to-palette (stamp pal-index palettes
                                 &key (allow-imperfect-p t))
  (map-region-to-palette
   stamp
   (mapcar #'palette->rgb (coerce (elt (2a-to-list palettes) pal-index) 'list))
   :allow-imperfect-p allow-imperfect-p))

(defun blob/write-spans (spans output &key imperfectp)
  (format output "~2%Spans:~%")
  (let ((stamp-buffer (make-array #x1000 :adjustable t))
        (stamp-offsets (make-hash-table)))
    (loop for span being the hash-keys in spans using (hash-value id)
          for serial from 0
          do (progn
               (if (and (< serial #x100)
                        (>= (+ serial (length span)) #x100))
                   (setf serial #x100))
               (blob/write-span-to-stamp-buffer span stamp-buffer
                                                :stamp-offsets stamp-offsets
                                                :serial serial
                                                :output output
                                                :id id
                                                :imperfectp imperfectp)
               (incf serial (length span))))
    (format *trace-output* " writing stamps … ")
    (format output "~2%;;; Binary stamp data follows.~%")
    (hex-dump-bytes stamp-buffer output)
    (format output "~2%~10t.bend~%")
    (format output "~2%;;; This size marker is the estimated amount of ROM that this
;;; blob may take up, used for allocation purposes.
;;; $SIZE$~x~%"
            (+ #x20
               (* 4 (hash-table-count spans))
               (length stamp-buffer)))))
(defun blob/write-spans-320ac (spans output &key imperfectp)
  "Write spans for 320A/C mode, handling both monochrome (320A) and color (320C) stamps."
  (format output "~2%Spans:~%")
  (let ((stamp-buffer (make-array #x1000 :adjustable t))
        (stamp-offsets (make-hash-table)))
    (loop for span being the hash-keys in spans using (hash-value id)
          for serial from 0
          do (progn
               (if (and (< serial #x100)
                        (>= (+ serial (length span)) #x100))
                   (setf serial #x100))
               (blob/write-span-to-stamp-buffer-320ac span stamp-buffer
                                                      :stamp-offsets stamp-offsets
                                                      :serial serial
                                                      :output output
                                                      :id id
                                                      :imperfectp imperfectp)
               (incf serial (length span))))
    (format *trace-output* " writing 320A/C stamps … ")
    (format output "~2%;;; Binary stamp data follows.~%")
    (hex-dump-bytes stamp-buffer output)
    (format output "~2%~10t.bend~%")
    (format output "~2%;;; This size marker is the estimated amount of ROM that this
;;; blob may take up, used for allocation purposes.
;;; $SIZE$~x~%"
            (+ #x20
               (* 4 (hash-table-count spans))
               (length stamp-buffer)))))
(defun blob/write-span-to-stamp-buffer-320ac (span stamp-buffer
                                              &key stamp-offsets serial output id
                                                   imperfectp)
  "Write a span of stamps for 320A/C mode, detecting and converting each stamp appropriately."
  (setf (gethash id stamp-offsets) serial)
  (let ((start (+ (* #x1000 (floor serial #x100))
                  (mod serial #x100))))
    (when (>= start (array-dimension stamp-buffer 0))
      (adjust-array stamp-buffer (+ #x1000 (array-dimension stamp-buffer 0))))
    (format output "~%~10tSpan~x = * + $~4,'0x" id start)
    (dotimes (stamp (length span))
      (let* ((stamp-data (elt span stamp))
             (mode (if (stamp-is-monochrome-p stamp-data) :320a :320c))
             (bytes (if (eq mode :320a)
                        ;; 320A mode: 8 pixels per byte, monochrome; stamps are 4×16 cells
                        ;; but 320A conversion expects 8 pixel columns — pad with transparent.
                        ;; Map any non-zero palette index to 1 (foreground); 0 stays transparent.
                        (let* ((w (array-dimension stamp-data 0))
                               (h (array-dimension stamp-data 1))
                               (padded (if (>= w 8)
                                           stamp-data
                                           (let ((p (make-array (list 8 h)
                                                                :element-type '(unsigned-byte 8))))
                                             (dotimes (x w)
                                               (dotimes (y h)
                                                 (setf (aref p x y) (aref stamp-data x y))))
                                             (dotimes (x (- 8 w))
                                               (dotimes (y h)
                                                 (setf (aref p (+ w x) y) 0)))
                                             p)))
                               (binary (let ((b (make-array (array-dimensions padded)
                                                            :element-type '(unsigned-byte 8))))
                                         (destructuring-bind (pw ph) (array-dimensions padded)
                                           (dotimes (x pw)
                                             (dotimes (y ph)
                                               (setf (aref b x y) (if (zerop (aref padded x y)) 0 1)))))
                                         b))
                               (bytes-across (7800-image-to-320a binary
                                                                 :byte-width 1
                                                                 :height 16
                                                                 :palette #(0 1)
                                                                 :best-fit-p imperfectp)))
                          (assert (= 1 (length bytes-across)))
                          (car bytes-across))
                        ;; 320C mode: 4 pixels per byte, 4 colors
                        (let ((bytes-across (7800-image-to-320c stamp-data
                                                                :byte-width 1
                                                                :height 16
                                                                :palette #(0 1 2 3)
                                                                :best-fit-p imperfectp)))
                          (assert (= 1 (length bytes-across)))
                          (car bytes-across)))))
        (dotimes (byte 16)
          (let ((i (+ start stamp (* #x100 byte))))
            (assert (let ((b (aref stamp-buffer i)))
                      (or (null b) (zerop b))) ()
                      "Stamp buffer contains ~x at index ~x; serial ~x, stamp ~x"
                      (aref stamp-buffer i) i serial stamp)
            (setf (aref stamp-buffer i)
                  (elt bytes (- 15 byte)))))))))
(defun blob-rip-5200 (png-file)
  "Rip a Bitmap Large Object Block from PNG-FILE for Atari 5200 Mode E.

@cindex BLOB ripping
@cindex Mode E graphics
@cindex ANTIC playfield

@table @code
@item Package: skyline-tool
@item Arguments: png-file (pathname or string)
@item Returns: nil
@item Side Effects: Creates @file{Source/Generated/5200/Assets/Blob.*.s}
@end table

5200 BLOBs are 160-pixel-wide ANTIC Mode E playfield bitmaps (four colors per
scanline, per-row palette in @code{CoLu}), not 7800 MARIA display-list stamps."
  (let* ((*machine* 5200)
         (*region* :ntsc)
         (png (png-read:read-png-file png-file))
         (height (png-read:height png))
         (width (png-read:width png))
         (palette-pixels (png->palette height width
                                       (png-read:image-data png)))
         (output-pathname (png-to-blob-pathname png-file))
         (blob-label (format nil "Blob_~a"
                             (assembler-label-name (pathname-name png-file)))))
    (assert (= width 160) ()
            "5200 BLOB ripper requires 160px width (Mode E), not ~d" width)
    (assert (zerop (mod width 4)) (width)
            "5200 Mode E BLOB width must be a multiple of 4, not ~d" width)
    (format *trace-output* "~&Ripping 5200 Mode E BLOB from ~a (~:d×~:d px)… "
            (enough-namestring png-file) width height)
    (finish-output *trace-output*)
    (compile-5200-mode-e-bitmap palette-pixels
                                :png-file png-file
                                :output-pathname output-pathname
                                :block-label blob-label
                                :height height
                                :width width
                                :compressp t
                                :lenient-palette-p t)
    (format *trace-output* " … done!~%")))
(defun blob-rip-7800 (png-file &optional (imperfectp$ nil))
  "Rip a Bitmap Large Object Block from PNG-FILE

automatically selecting the appropriate Atari 7800 graphics mode based on image width.

@cindex BLOB ripping
@cindex graphics mode auto-detection
@cindex 160A mode
@cindex 320A/C mode

@table @code
@item Package: skyline-tool
@item Arguments: png-file (pathname or string), &optional imperfectp$ (boolean)
@item Returns: nil
@item Side Effects: Creates .s file with BLOB data, outputs progress to *trace-output*
@end table
@strong{Mode Selection:}
@itemize
@item 320px width → 320A/C mixed mode (navigation charts)
@item Other widths → 160A mode (standard sprites)
@end itemize

@strong{Graphics Modes:}
@table @asis
@item 160A Mode
Standard 160-pixel wide sprites with 4-color palette
@item 320A/C Mode
Mixed 320-pixel wide graphics for navigation displays
@end table

@strong{Mode Selection:}
@itemize
@item 320px width → 320A/C mixed mode (navigation charts)
@item Other widths → 160A mode (standard sprites)
@end itemize

@strong{Graphics Modes:}
@table @asis
@item 160A Mode
4 pixels per byte, 25 palettes, for general sprite graphics
@item 320A/C Mode
Mixed monochrome (320A) and color (320C) modes for 320px wide navigation displays
@end table

Pass --imperfect to allow imperfect palette matches instead of signaling errors."
  (format *trace-output* "~&Ripping BLOB from ~a … " (enough-namestring png-file))
  (finish-output *trace-output*)
  (let* ((*machine* 7800)
         (*region* :ntsc)
         (png (png-read:read-png-file png-file))
         #+ () (height (png-read:height png))
         (width (png-read:width png))
         #+ () (palette-pixels (png->palette height width
                                             (png-read:image-data png)))
         #+ () (output-pathname (png-to-blob-pathname png-file))
         (imperfectp (or (eql :imperfect imperfectp$)
                         (equal imperfectp$ "--imperfect"))))
    (format *trace-output* "accepting ~:[only perfect palette matches~;imperfect palette matches~]… " imperfectp)
    ;; Route to appropriate ripping method based on width
    (if (= width 320)
        (blob-rip-7800-320ac png-file imperfectp$)
        (blob-rip-7800-160ab png-file imperfectp$))))
(defun blob-rip-7800-160ab (png-file &optional (imperfectp$ nil))
  "@cindex BLOB ripping
@cindex 160A/B graphics mode
@cindex sprite graphics

@table @code
@item Package: skyline-tool
@item Arguments: png-file (pathname or string), &optional imperfectp$ (boolean)
@item Returns: nil
@item Side Effects: Creates .s file with BLOB data, outputs progress to *trace-output*
@end table

Rip a Bitmap Large Object Block in 160A/B mode from PNG-FILE for standard sprite graphics.

@strong{Graphics Modes:}

160A
@itemize
@item 4 pixels per byte (2 bits per pixel)
@item Up to 4 colors (background + 1 palette × 3 colors)
@item Variable width (multiple of 4 pixels)
@item Height multiple of 16 + 1 pixels (palette strip)
@end itemize

160B
@itemize
@item 2 pixels per byte (4 bits per pixel)
@item Up to 13 colors (background + 4 palettes × 3 colors each)
@item Variable width (multiple of 2 pixels)
@item Height multiple of 16 + 1 pixels (palette strip)
@end itemize

Pass --imperfect to allow imperfect palette matches instead of signaling errors."
  (let* ((imperfectp (and imperfectp$ (not (emptyp imperfectp$))))
         (png (png-read:read-png-file png-file))
         (height (png-read:height png))
         (width (png-read:width png))
         (palette-pixels (png->palette height width
                                       (png-read:image-data png)))
         (output-pathname (png-to-blob-pathname png-file))
         (imperfectp (or (eql :imperfect imperfectp$)
                         (equal imperfectp$ "--imperfect"))))
    (format *trace-output* "accepting ~:[only perfect palette matches~;imperfect palette matches~]… " imperfectp)
    (check-height+width-for-blob height width palette-pixels)
    (let* ((palettes (extract-palettes palette-pixels))
           (palettes-list (2a-to-lol palettes))
           (stamps (extract-4×16-stamps palette-pixels))
           (zones (floor height 16))
           (columns (floor width 4))
           (spans (make-hash-table :test 'equalp))
           (stamp-counting 0)
           (next-span-id 0))
      (format *trace-output* " generating drawing lists in ~a… " (enough-namestring output-pathname))
      (%write-blob-assembly-atomically
       output-pathname
       (lambda (output)
         (format output ";;; Bitmap Large Object Block for Atari 7800
;;; Derived from source file ~a. This is a generated file.~3%

Blob_~a:~10t.block
    .byte Mode160AB~2%"
                 (enough-namestring png-file)
                 (assembler-label-name (pathname-name png-file)))
         (write-blob-palettes png output)
         (format output "~%Zones:~%~10t.byte ~d~10t; zone count" zones)
         (dotimes (zone zones)
           (format output "~2&Zone~d:" zone)
           (flet ((emit-span (x span pal-index)
                    (when span
                      (let ((id (or (gethash span spans)
                                    (prog1
                                        (setf (gethash span spans) (prog1 next-span-id
                                                                     (incf next-span-id)))
                                      (cond
                                        ((and (< stamp-counting #x100)
                                              (< (+ stamp-counting (length span)) #x100))
                                         (incf stamp-counting (length span)))
                                        ((and (< stamp-counting #x100)
                                              (>= (+ stamp-counting (length span)) #x100))
                                         (setf stamp-counting #x100))
                                        (t (incf stamp-counting)))))))
                        (format output "~%~10t.DLHeader Span~x, ~d, ~d, ~d"
                                id pal-index (length span)
                                (- x (* 4 (length span))))))))
             (loop with span = nil
                   with last-palette = nil
                   for x from 0 by 4
                   for column from 0 below columns
                   for stamp = (aref stamps column zone)
                   for palette = (or (when (and last-palette
                                                (tile-fits-palette-p
                                                 stamp
                                                 (elt palettes-list last-palette)))
                                       last-palette)
                                     (best-palette stamp palettes
                                                   :allow-imperfect-p imperfectp
                                                   :x column :y zone))
                   for paletted-stamp = (limit-region-to-palette
                                         stamp (elt palettes-list palette)
                                         :allow-imperfect-p imperfectp)
                   do
                      (cond
                        ((zerop column)
                         (setf span (list paletted-stamp)
                               last-palette palette))
                        ((blank-stamp-p stamp (aref palettes 0 0))
                         (emit-span x span last-palette)
                         (setf span nil
                               last-palette nil))
                        ((and (or (null last-palette)
                                  (= palette last-palette))
                              (< (length span) 31))
                         (appendf span (list paletted-stamp))
                         (setf last-palette palette))
                        (t
                         (emit-span x span last-palette)
                         (setf span (list paletted-stamp)
                               last-palette palette)))
                   finally
                      (emit-span x span last-palette)))
           (format output "~%~10t.word $0000"))
         (blob/write-spans spans output :imperfectp imperfectp))))
    (format *trace-output* " … done!~%")))
(defun blob-rip-7800-320ac (png-file &optional (imperfectp$ nil))
  "@cindex BLOB ripping
@cindex 320A/C graphics mode
@cindex navigation chart graphics
@cindex mixed mode graphics

@table @code
@item Package: skyline-tool
@item Arguments: png-file (pathname or string), &optional imperfectp$ (boolean)
@item Returns: nil
@item Side Effects: Creates .s file with BLOB data, outputs progress to *trace-output*
@end table

Rip a Bitmap Large Object Block in mixed 320A/C mode from PNG-FILE for 320px wide navigation chart graphics.

@strong{Graphics Mode:}
@itemize
@item Uses 320A mode for monochrome stamps (1 bit per pixel, 1 color + transparent)
@item Uses 320C mode for color stamps (4 bits per pixel, 4 colors + transparent)
@item Automatically detects appropriate mode per 4×16 pixel stamp
@end itemize

@strong{Requirements:}
@itemize
@item Image width must be exactly 320 pixels
@item Image height must be (N × 16) + 1 pixels (palette strip)
@item PNG should contain appropriate palette data
@end itemize

Pass --imperfect to allow imperfect palette matches instead of signaling errors."
  (let* ((*machine* 7800)
         (*region* :ntsc)
         (png (png-read:read-png-file png-file))
         (height (png-read:height png))
         (width (png-read:width png))
         (palette-pixels (png->palette height width
                                       (png-read:image-data png)))
         (output-pathname (png-to-blob-pathname png-file))
         (imperfectp (or (eql :imperfect imperfectp$)
                         (equal imperfectp$ "--imperfect"))))
    (format *trace-output* "accepting ~:[only perfect palette matches~;imperfect palette matches~]… " imperfectp)
    (check-height+width-for-blob-320ac height width palette-pixels)
    (let* ((zone-spans nil)
           (palettes (extract-palettes-320ac palette-pixels))
           (palettes-list (2a-to-lol palettes))
           (stamps (extract-4×16-stamps palette-pixels)) ; Use 4px stamps for 320C mode
           (zones (floor height 16))
           (columns (floor width 4)) ; 320 / 4 = 80 columns
           (spans (make-hash-table :test 'equalp))
           (stamp-counting 0)
           (next-span-id 0))
      (format *trace-output* " generating 320A/C drawing lists in ~a… " (enough-namestring output-pathname))
      (force-output *trace-output*)
      (format *trace-output* " zones=~d, stamps=~d×~d~%" zones columns zones)
      (force-output *trace-output*)
      (%write-blob-assembly-atomically
       output-pathname
       (lambda (output)
         (format output ";;; Bitmap Large Object Block for Atari 7800 (320A/C mode)
;;; Derived from source file ~a. This is a generated file.~3%

Blob_~a:~10t.block~2%"
                 (enough-namestring png-file)
                 (assembler-label-name (pathname-name png-file)))
         (format output "~%Mode:~10t.byte Mode320AC")
         (write-blob-palettes png output :extractor 'extract-palettes-320ac :start-offset 2)
         (format output "~%Zones:~%~10t.byte ~d~10t; zone count" zones)
         (dotimes (zone zones)
           (format output "~2&Zone~d:" zone)
           (flet ((collect-span (x span last-palette last-mode)
                    (when span
                      (push (list x span last-palette last-mode) zone-spans))))
             (loop with span = nil
                   with last-palette = nil
                   with last-mode = nil
                   for x from 0 by 1
                   for column from 0 below columns
                   for stamp = (aref stamps column zone)
                   for mode = (if (stamp-is-monochrome-p stamp) :320a :320c)
                   for palette = (if (eq mode :320c)
                                     (let ((cands (if last-palette
                                                      (list (if (< last-palette 4) 0 4))
                                                      '(0 4))))
                                       (block found
                                         (dolist (base cands)
                                           (let ((c2 (vector (aref palettes base 2)
                                                             (aref palettes (1+ base) 2)
                                                             (aref palettes (+ base 2) 2)
                                                             (aref palettes (+ base 3) 2))))
                                             (when (320c-choose-limit-palette stamp c2)
                                               (return-from found base))))
                                         (first cands)))
                                     (or (when (and last-palette
                                                    (tile-fits-palette-p
                                                     stamp
                                                     (elt palettes-list last-palette)))
                                           last-palette)
                                         (best-palette stamp palettes
                                                       :allow-imperfect-p imperfectp
                                                       :x column :y zone)))
                   for c2-entries = (when (eq mode :320c)
                                      (let ((base (if (< palette 4) 0 4)))
                                        (vector (aref palettes base 2)
                                                (aref palettes (1+ base) 2)
                                                (aref palettes (+ base 2) 2)
                                                (aref palettes (+ base 3) 2))))
                   for group-palette = (if (eq mode :320c)
                                           (if (< palette 4) 0 4)
                                           palette)
                   for limit-chosen = (when (eq mode :320c)
                                        (320c-choose-limit-palette stamp c2-entries))
                   for limit-palette = (if (eq mode :320c)
                                           (or limit-chosen
                                               (list 0 (aref c2-entries 0)
                                                     (aref c2-entries 1)
                                                     (aref c2-entries 2)))
                                           (elt palettes-list palette))
                   for use-imperfect = (if (eq mode :320c)
                                           (or imperfectp (null limit-chosen))
                                           imperfectp)
                   for paletted-stamp = (limit-region-to-palette
                                         stamp limit-palette
                                         :allow-imperfect-p use-imperfect)
                   do (when (= (mod column 20) 0)
                        (format *trace-output* " col ~d/~d…" column columns)
                        (force-output *trace-output*))
                      (cond
                        ((zerop column)
                         (setf span (list paletted-stamp)
                               last-palette group-palette
                               last-mode mode))
                        ((blank-stamp-p stamp (aref palettes 0 0))
                         (collect-span x span last-palette last-mode)
                         (setf span nil
                               last-palette nil
                               last-mode nil))
                        ((and (or (null last-palette)
                                  (= group-palette last-palette))
                              (eq mode last-mode)
                              (< (length span) 31))
                         (appendf span (list paletted-stamp))
                         (setf last-palette group-palette
                               last-mode mode))
                        (t
                         (collect-span x span last-palette last-mode)
                         (setf span (list paletted-stamp)
                               last-palette group-palette
                               last-mode mode)))
                   finally
                      (collect-span x span last-palette last-mode)))
           (let ((spans-this-zone (sort (nreverse zone-spans) #'< :key #'first)))
             (setf zone-spans nil)
             (dolist (entry spans-this-zone)
               (let* ((x (first entry))
                      (span (second entry))
                      (pal (third entry))
                      (mode (fourth entry))
                      (header (if (eq (fourth entry) :320c) "DLAltHeader" "DLHeader"))
                      (id (or (gethash span spans)
                              (prog1
                                  (setf (gethash span spans) (prog1 next-span-id
                                                               (incf next-span-id)))
                                (cond
                                  ((and (< stamp-counting #x100)
                                        (< (+ stamp-counting (length span)) #x100))
                                   (incf stamp-counting (length span)))
                                  ((and (< stamp-counting #x100)
                                        (>= (+ stamp-counting (length span)) #x100))
                                   (setf stamp-counting #x100))
                                  (t (incf stamp-counting)))))))
                 (format output "~%~10t.~a Span~x, ~d, ~d, ~d"
                         header id pal (length span)
                         (* 2 (- x (length span)))))))
           (format output "~%~10t.DLEnd")
           (blob/write-spans-320ac spans output :imperfectp imperfectp))))
      (format *trace-output* " … done!~%"))))

