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
                                             (* (1+ b) 8) (1+ y))))
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
                                              (* (1+ b) 4) (1+ y)))
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

(defmethod parse-7800-object ((mode (eql :160a)) pixels &key width height palette)
  (declare (ignore palette))
  (let ((total-width (array-dimension pixels 0))
        (total-height (1- (array-dimension pixels 1))))
    (assert (zerop (mod total-height height)) (total-height)
            "Image height must be modulo ~:Dpx plus 1px for palette strip, but got ~:Dpx"
            height (1+ total-height))
    (assert (zerop (mod total-width width)) (total-width)
            "Image width must be module ~:Dpx, but get ~:Dpx" width total-width)
    (assert (zerop (mod width 4)) (width)
            "Width for mode 160A must be modulo 4px, not ~:Dpx" width))
  (let* ((byte-width (/ width 4))
         (images (extract-regions pixels width height))
         (bytes-lists (list))
         (palettes (extract-palettes pixels)))
    (dolist (image images)
      (dolist (bytes-list (7800-image-to-160a image
                                              :byte-width byte-width
                                              :height height
                                              :palette (elt (2a-to-lol palettes)
                                                            (best-palette image palettes))))
        (push (reverse bytes-list) bytes-lists)))
    (reverse bytes-lists)))
    
(defmethod parse-7800-object ((mode (eql :160b)) pixels &key width height palette)
  (assert (= 16 (length palette)))
  (let ((total-width (array-dimension pixels 0))
        (total-height (1- (array-dimension pixels 1))))
    (assert (zerop (mod total-height height)) (total-height)
            "Image height must be modulo ~:Dpx plus 1px for palette strip, but got ~:Dpx"
            height (1+ total-height))
    (assert (zerop (mod total-width width)) (total-width)
            "Image width must be modulo ~:Dpx, but get ~:Dpx" width total-width)
    (assert (zerop (mod width 2)) (width)
            "Width for mode 160B must be modulo 2px, not ~:Dpx" width))
  (let* ((width-in-bytes (/ width 2))
         (images (extract-regions pixels width height))
         (bytes-lists (list))
         (i 0))
    (dolist (image images)
      (dotimes (byte-i width-in-bytes)
        (let ((bytes (list)))
          (dotimes (y height)
            (handler-bind
                ((color-not-in-palette-error
                   (lambda (c)
                     (princ c)
                     (if (tty-xterm-p)
                         (with-output-to-string (*standard-output*)
                           (format t "~2&~c[2mProblem with this image:~c[0m~2%"
                                   #\Escape #\Escape)
                           (pixels-to-ansi image :x (* 2 byte-i) :y y))
                         (format nil "Problem with this image"))
                     (cerror (format nil "Continue, using $~2,'0x (probably transparent)"
                                     (elt palette 0))
                             "Color not in palette")
                     (elt palette 0))))
               (let* ((byte-pixels (extract-region image
                                                   (* 2 byte-i) y
                                                   (+ 2 (* 2 byte-i)) (1+ y)))
                     (indices (pixels-into-palette byte-pixels palette
                                                   :x0 (* 2 byte-i) :y0 y :i i)))
                (let ((a (aref indices 0))
                      (b (aref indices 1)))
                  (flet ((binny (n e d)
                           (ash (if (zerop (logand n (expt 2 e))) 0 1) d)))
                    (push (logior (binny a 3 3) (binny b 3 1)
                                  (binny a 2 2) (binny b 2 0)
                                  (binny a 1 7) (binny b 1 5)
                                  (binny a 0 6) (binny b 0 4))
                          bytes))))))
          (push bytes bytes-lists)))
      (incf i))
    (nreverse bytes-lists)))
(defmethod parse-7800-object ((mode (eql :320a)) pixels &key width height palette)
  (declare (ignore palette))
  (let ((total-width (array-dimension pixels 0))
        (total-height (array-dimension pixels 1)))
    (unless (zerop (mod total-height height))
      (warn "Image height must be modulo ~:Dpx, but got ~:Dpx"
            height (1+ total-height)))
    (assert (zerop (mod total-width width)) (total-width)
            "Image width must be module ~:Dpx, but get ~:Dpx" width total-width)
    (assert (zerop (mod width 8)) (width)
            "Width for mode 320A must be modulo 8px, not ~:Dpx" width))
  (let* ((byte-width (/ width 8))
         (images (extract-regions pixels width height))
         (bytes-lists (list)))
    (dolist (image images)
      (dotimes (b byte-width)
        (let ((bytes (list)))
          (dotimes (y height)
            (let ((byte-pixels (extract-region image
                                               (* b 8) y
                                               (* (1+ b) 8) (1+ y))))
              (push (reduce #'logior
                            (mapcar (lambda (bit)
                                      (ash (if (zerop (aref byte-pixels (- 7 bit) 0))
                                               0 1)
                                           bit))
                                    '(7 6 5 4 3 2 1 0)))
                    bytes)))
          (push bytes bytes-lists))))
    (reverse bytes-lists)))
(defmethod parse-7800-object ((mode (eql :320b)) pixels &key width height palette)
  (assert (>= 4 (length palette)))
  (let ((total-width (array-dimension pixels 0))
        (total-height (1- (array-dimension pixels 1))))
    (assert (zerop (mod total-height height)) (total-height)
            "Image height must be modulo ~:Dpx plus 1px for palette strip, but got ~:Dpx"
            height (1+ total-height))
    (assert (zerop (mod total-width width)) (total-width)
            "Image width must be module ~:Dpx, but get ~:Dpx" width total-width)
    (assert (zerop (mod width 4)) (width)
            "Width for mode 320B must be modulo 4px, not ~:Dpx" width))
  (let* ((byte-width (/ width 4))
         (images (extract-regions pixels width height))
         (bytes-lists (list)))
    (dolist (image images)
      (dotimes (b byte-width)
        (let ((bytes (list)))
          (dotimes (y height)
            (let* ((byte-pixels (extract-region image
                                                (* b 4) y
                                                (* (1+ b) 4) (1+ y)))
                   (indices (pixels-into-palette byte-pixels palette
                                                  :x0 (* b 4) :y0 y)))
              (push (logior
                     (ash (aref indices 0) 6)
                     (ash (aref indices 1) 4)
                     (ash (aref indices 2) 2)
                     (aref indices 3))
                    bytes)))
          (push bytes bytes-lists))))
    (reverse bytes-lists)))
(defmethod parse-7800-object ((mode (eql :320d)) pixels &key width height palette)
  (assert (>= 8 (length palette)))
  (let ((total-width (array-dimension pixels 0))
        (total-height (1- (array-dimension pixels 1))))
    (assert (zerop (mod total-height height)) (total-height)
            "Image height must be modulo ~:Dpx plus 1px for palette strip, but got ~:Dpx"
            height (1+ total-height))
    (assert (zerop (mod total-width width)) (total-width)
            "Image width must be module ~:Dpx, but get ~:Dpx" width total-width)
    (assert (zerop (mod width 8)) (width)
            "Width for mode 320D must be modulo 8px, not ~:Dpx" width))
  (let* ((byte-width (/ width 8))
         (images (extract-regions pixels width height))
         (bytes-lists (list)))
    (dolist (image images)
      (dotimes (b byte-width)
        (let ((bytes (list)))
          (dotimes (y height)
            (let* ((byte-pixels (extract-region image
                                                (* b 8) y
                                                (* (1+ b) 8) (1+ y)))
                   (indices (pixels-into-palette byte-pixels palette
                                                  :x0 (* b 8) :y0 y)))
              (push (logior
                     (ash (if (> (aref indices 0) 1) 1 0) 7)
                     (ash (if (> (aref indices 1) 1) 1 0) 6)
                     (ash (if (> (aref indices 2) 1) 1 0) 5)
                     (ash (if (> (aref indices 3) 1) 1 0) 4)
                     (ash (if (> (aref indices 4) 1) 1 0) 3)
                     (ash (if (> (aref indices 5) 1) 1 0) 2)
                     (ash (if (> (aref indices 6) 1) 1 0) 1)
                     (ash (if (> (aref indices 7) 1) 1 0) 0))
                    bytes)))
          (push bytes bytes-lists))))
    (reverse bytes-lists)))
(defmethod parse-7800-object ((mode (eql :320c)) pixels &key width height palette)
  (assert (>= 8 (length palette)))
  (assert (zerop (mod width 4)) (width)
          "Width for mode 320C must be modulo 4px, not ~:Dpx" width)
  (let* ((byte-width (/ width 4))
         (images (extract-regions pixels width height))
         (bytes-lists (list)))
    (dolist (image images)
      (dotimes (b byte-width)
        (let ((bytes (list)))
          (dotimes (y height)
            (let* ((byte-pixels (extract-region image
                                                (* b 4) y
                                                (* (1+ b) 4) (1+ y)))
                   (indices (pixels-into-palette byte-pixels palette
                                                 :x0 (* b 4) :y0 y))
                   (px-pair-palette (mapcar (lambda (pair)
                                              (cond
                                                ((and (zerop (car pair))
                                                      (zerop (cdr pair)))
                                                 0)
                                                ((zerop (car pair))
                                                 (ash (logand (cdr pair) #x06) -1))
                                                (t
                                                 (ash (logand (car pair) #x06) -1))))
                                            (list (cons (aref indices 0)
                                                        (aref indices 1))
                                                  (cons (aref indices 2)
                                                        (aref indices 3))))))
              (push (logior
                     (ash (logand (aref indices 0) #x01) 7)
                     (ash (logand (aref indices 1) #x01) 6)
                     (ash (logand (aref indices 2) #x01) 5)
                     (ash (logand (aref indices 3) #x01) 4)
                     (ash (first px-pair-palette) 2)
                     (second px-pair-palette))
                    bytes)))
           (push (reverse bytes) bytes-lists))))
    (reverse bytes-lists)))
   
(defun color-average (colors)
  (let ((colors (remove-if #'null colors)))
    (if colors
        (list (round (mean (mapcar #'first colors)))
              (round (mean (mapcar #'second colors)))
              (round (mean (mapcar #'third colors))))
        (list 0 0 0))))

(defun tile-hash (left right big-endian-p)
  (logior (ash left 8) (ash right 16) (if big-endian-p 1 0)))

(defun screen-to-grid/tia/tles (screen)
  (check-type screen (array integer (8 8)))
  (let ((tiles (make-array (list 4 8) :element-type 'fixnum)))
    (dotimes (y 8)
      (dotimes (2x 4)
        (let* ((big-endian-p (evenp 2x))
               (left (aref screen (* 2x 2) y))
               (right (aref screen (1+ (* 2x 2)) y))
               (tile-hash (tile-hash left right big-endian-p))
               (merged-tile (or (gethash tile-hash *merged-tiles*)
                                (setf (gethash tile-hash *merged-tiles*)
                                      (incf *tile-counter*)))))
          (assert (<= merged-tile *tile-counter*))
          (setf (aref tiles 2x y) merged-tile))))
    tiles))

(defun screen-to-grid/tia (screen)
  (make-instance 'grid/tia
                 :tiles (screen-to-grid/tia/tles screen)
                 :colors (maptimes (y 8)
                                   (collect-foreground-color/tia
                                    (maptimes (x 8) (aref screen x y))))
                 ;; TODO: #1243
                 :background-color #x44))

#+ ()
(defun map-tiles/tia (world levels)
  (format *trace-output* "~&Sorting tile art into TIA format in world ~a…" world)
  (let* ((*merged-tiles* (make-hash-table :test #'equal))
         (*tile-counter* -1)
         (grids (mapcar #'screen-to-grid/tia (extract-8×8-screens levels))))
    (unless (> +tia-tile-limit+ *tile-counter*)
      (error "Too many merged tiles; TIA core can't handle more than ~:d tiles,
but world “~a” needs ~:d for the ~r level~:p
~{“~a”~^ and ~}"
             +tia-tile-limit+ world *tile-counter* (length levels) levels))
    (values grids *merged-tiles*)))

(defun list-chomp (n list)
  (if (< (length list) n)
      (append list (loop repeat (- n (length list)) collect 0))
      (subseq list 0 n)))

(defun most-popular-colors (pixels width height &key count background)
  (let ((popularity (make-hash-table)))
    (dotimes (x width)
      (dotimes (y height)
        (unless (and background (= background (aref pixels x y)))
          (incf (gethash (aref pixels x y) popularity 0)))))
    (list-chomp count (sort (hash-table-keys popularity)
                            #'> :key (lambda (n) (gethash n popularity))))))

(defun most-popular-13-colors (pixels width height)
  (most-popular-colors pixels width height :count 13))

(defun palette-reference (rgb palette &key allow-imperfect-p)
  (or (position rgb palette :test 'equalp)
      (if allow-imperfect-p
          (let ((nearest (find-nearest-in-palette (copy-list palette)
                                                  (first rgb)
                                                  (second rgb)
                                                  (third rgb))))
            (or (position nearest palette :test 'equalp)
                (error "Could not map ~s to anything close to palette ~s (wanted ~s)"
                       rgb palette nearest)))
          (error "Palette value ~s is not in palette ~s" rgb palette))))

(defun map-region-to-palette (region palette &key allow-imperfect-p)
  (let ((output (make-array (array-dimensions region) :element-type '(unsigned-byte 8))))
    (dotimes (x (array-dimension region 0))
      (dotimes (y (array-dimension region 1))
        (setf (aref output x y) (palette-reference (aref region x y) palette
                                                   :allow-imperfect-p allow-imperfect-p))))
    output))

(defun 160b-wiggle-nybbles (a b)
  (flet ((truthy (n) (if (zerop n) 0 1)))
    (logior (ash (truthy (logand a #x2)) 7)
            (ash (truthy (logand a #x1)) 6)
            (ash (truthy (logand b #x2)) 5)
            (ash (truthy (logand b #x1)) 4)
            (ash (truthy (logand a #x8)) 3)
            (ash (truthy (logand a #x4)) 2)
            (ash (truthy (logand b #x8)) 1)
            (ash (truthy (logand b #x4)) 0))))

(defun write-direct-stamp-header (label stamp-offset screen-x stream)
  (format stream "~&~10t.byte <(~a + $~2,'0x), $c0, >(~a + $~2,'0x), $10, $~2,'0x"
          label stamp-offset label stamp-offset screen-x))

(defun write-stamp-bytes-for-blob (stamp-bytes stream)
  (format *trace-output* "2px × 16px bytes × ~:d" (array-dimension stamp-bytes 0))
  (dotimes (y #x10)
    (loop for stamp-index from 0
            below (floor (array-dimension stamp-bytes 0) #x10)
          for stamp-page = (floor stamp-index 8)
          do (dotimes (span 5)
               (write-bytes (loop for x from 0 below #x10
                                  collecting (aref stamp-bytes
                                                   (+ x (* #x10 stamp-index))
                                                   (- #x0f y)))
                            stream)))))

(defun gather-stamp-bytes (normalized-pixels stamp-bytes &key stamp-index)
  (dotimes (b #x10)
    (dotimes (y #x10)
      (let ((a (aref normalized-pixels (* 2 b) y))
            (b (aref normalized-pixels (1+ (* 2 b)) y)))
        (setf (aref stamp-bytes (+ (* #x10 stamp-index) b) y)
              (160b-wiggle-nybbles a b))))))

(defun load-blob-image (pathname$)
  (format *trace-output* "~&Loading BLOB image from ~a" (enough-namestring pathname$))
  (png->palette (png-read:image-data (png-read:read-png-file
                                      (let ((pathname (parse-namestring pathname$)))
                                        (make-pathname
                                         :name (pathname-name pathname)
                                         :type (pathname-type pathname)
                                         :defaults #p"./Source/Art/"))))))

(defun extract-4×16-stamps (image)
  (let* ((rows (floor (1- (array-dimension image 1)) 16))
         (columns (floor (array-dimension image 0) 4))
         (output (make-array (list columns rows))))
    (dotimes (row rows)
      (dotimes (column columns)
        (let ((stamp (extract-region image (* column 4) (* row 16)
                                     (+ (* column 4) 4) (+ (* row 16) 16))))
          (assert (= 4 (array-dimension stamp 0)))
          (assert (= 16 (array-dimension stamp 1)))
          (setf (aref output column row) stamp))))
    output))

(defun blank-stamp-p (region background-color)
  (destructuring-bind (width height) (array-dimensions region)
    (dotimes (x width)
      (dotimes (y height)
        (unless (= background-color (aref region x y))
          (return-from blank-stamp-p nil)))))
  t)

(defun stamp-is-monochrome-p (stamp)
  "@cindex graphics mode detection
@cindex monochrome detection
@cindex 320A mode suitability

@table @code
@item Package: skyline-tool
@item Arguments: stamp (2D array of palette indices)
@item Returns: boolean
@item Side Effects: none
@end table

Determine if a 4×16 pixel stamp contains only 2 colors, making it suitable for 320A monochrome mode.

@strong{Detection Logic:}
@itemize
@item Counts unique palette indices in the stamp
@item Returns true if ≤ 2 unique colors found
@item Suitable for 320A mode (1 bit per pixel)
@item False indicates 320C mode needed (4 colors + transparency)
@end itemize

Used by 320A/C mode ripping to automatically select appropriate graphics mode per stamp."
  (let ((colors (make-hash-table)))
    (destructuring-bind (width height)
        (array-dimensions stamp)
      (dotimes (x width)
        (dotimes (y height)
          (setf (gethash (aref stamp x y) colors) t)))
      (<= (hash-table-count colors) 2))))

(defun limit-region-to-palette (region palette &key (allow-imperfect-p t))
  (let ((output (make-array (array-dimensions region))))
    (destructuring-bind (width height) (array-dimensions region)
      (dotimes (x width)
        (dotimes (y height)
          (setf (aref output x y)
                (if allow-imperfect-p
                    (pixel-into-palette (aref region x y) (coerce palette 'list)
                                        :best-fit-p t)
                    (or (position (aref region x y) palette)
                        (error 'color-not-in-palette-error
                               :pixel (aref region x y)
                               :x x :y y :i nil :image nil
                               :palette palette
                               :image-pixels region)))))))
    output))

(defun png-to-blob-pathname (png-file)
  "Pathname for generated Blob assembly from PNG-FILE (under Source/Blobs/PORT/).

@table @asis
@item PNG-FILE
Blob PNG path; if under @file{Source/Blobs/@var{PORT}/}, output is
@file{Source/Generated/@var{PORT}/Assets/Blob.*.s}; otherwise
@file{Source/Generated/Assets/Blob.*.s}.
@end table"
  (let* ((merged (merge-pathnames png-file))
         (dir (pathname-directory merged))
         (blobs-index (when dir (position "Blobs" dir :test #'string= :key #'string)))
         (port-dir (when (and blobs-index (< (1+ blobs-index) (length dir)))
                     (nth (1+ blobs-index) dir))))
    (make-pathname :directory (if port-dir
                                  (list :relative "Source" "Generated" port-dir "Assets")
                                  (list :relative "Source" "Generated" "Assets"))
                   :name (concatenate 'string "Blob."
                                      (pathname-name merged))
                   :type "s")))

(defun generated-blob-assembly-pathname (png-file)
  "Pathname for TMS9918-family generated Blob assembly from PNG-FILE.

ColecoVision (machine 9918) uses @file{Blob.<stem>.ClcV.s}; SMS, SG-1000, Game Gear,
and VS use @file{Blob.<stem>.s} like @code{png-to-blob-pathname}."
  (let* ((merged (merge-pathnames png-file))
         (base (png-to-blob-pathname png-file)))
    (if (= *machine* 9918)
        (make-pathname :directory (pathname-directory base)
                       :name (format nil "Blob.~a.ClcV" (pathname-name merged))
                       :type "s")
        base)))

(defun blob-rip-tms9918 (png-file)
  "Write TMS9918-family tile-mapped blob assembly from PNG-FILE.

Divides the image into 8×8 character cells.  Deduplicates patterns (max
256 unique).  Each cell gets the dominant foreground color.  Outputs
pattern data (8 bytes per tile), name table, and color table (per
8-tile group, TMS9918 color-table format).

Output path: @file{Source/Generated/@emph{machine}/Assets/Blob.@emph{name}.s}"
  (let* ((out (generated-blob-assembly-pathname png-file))
         (label (assembler-label-name (pathname-name (merge-pathnames png-file))))
         (png (png-read:read-png-file png-file))
         (height (png-read:height png))
         (width (png-read:width png))
         (α (png-read:transparency png))
         (pal (png->palette (png-read:image-data png) α))
         (cols (floor width 8))
         (rows (floor height 8))
         (cells (* cols rows))
         (uniq (make-array 256 :adjustable t :fill-pointer 0))
         (ht (make-hash-table :test 'equal))
         (nametable (make-array cells :element-type '(unsigned-byte 8)))
         (fg-colors (make-array cells :element-type '(unsigned-byte 8))))
    (when (or (< cols 1) (< rows 1))
      (error "TMS9918 blob ~A too small (~D×~D)" png-file width height))
    (loop for cell from 0 below cells
          for row = (floor cell cols) for col = (mod cell cols)
          for sx = (* col 8) for sy = (* row 8)
          for key = (cell-binary-key-from-image pal sx sy 8 8)
          for idx = (gethash key ht)
          do (unless idx
               (setf idx (length uniq))
               (setf (gethash key ht) idx)
               (vector-push-extend key uniq))
             (setf (aref nametable cell) idx)
             (setf (aref fg-colors cell)
                   (dominant-color pal sx sy 8 8 1)))
    ;; Reduce if pattern table overflows
    (when (> (length uniq) 256)
      (warn "TMS9918 blob ~A: ~D unique patterns, reducing to 256 via H-distance..."
            png-file (length uniq))
      (reduce-tile-set uniq nametable :max-slots 256))
    (ensure-directories-exist out)
    (%write-blob-assembly-atomically
     out
     (lambda (stream)
       (format stream ";;; TMS9918 blob: ~A~%" (enough-namestring png-file))
       (format stream ";;; Grid: ~D×~D chars; ~D unique pattern~:P~2%" cols rows (length uniq))
       (format stream "~ASizeX EQU ~D~%" label cols)
       (format stream "~ASizeY EQU ~D~2%" label rows)
       ;; Pattern table: 8 bytes per tile
       (format stream "~APatterns:~%" label)
       (loop for u from 0 below (length uniq)
             for row-bytes = (aref uniq u)
             do (loop for byte in row-bytes
                      do (format stream "  .byte $~2,'0X~%" byte)))
       ;; Name table
       (format stream "~ANameTable:~%" label)
       (loop for i from 0 below cells
             do (format stream "  .byte ~D~%" (aref nametable i)))
       ;; Color table: per 8-tile group (TMS9918: upper nybble=fg, lower=bg)
       (format stream "~AColorTable:~%" label)
       (let ((groups (ceiling (length uniq) 8)))
         (loop for g from 0 below groups
               do (let ((fg 1) (bg 0))
                    ;; Most common fg color in this group of 8 tiles
                    (let ((cfreq (make-array 16 :initial-element 0)))
                      (loop for i from 0 below cells
                            for tile = (aref nametable i)
                            when (<= (* g 8) tile (1- (min (* (1+ g) 8) cells)))
                              do (incf (aref cfreq (aref fg-colors i))))
                      (dotimes (c 16)
                        (when (> (aref cfreq c) (aref cfreq fg))
                          (setf fg c))))
                    (format stream "  .byte ~D  ; group ~D~%"
                            (logior (ash (logand fg #x0F) 4) (logand bg #x0F))
                            g))))))
    (format *trace-output* "~&blob-rip-tms9918: wrote ~a~%" (enough-namestring out))
    t))

(defun %write-blob-assembly-atomically (output-pathname writer)
  "Call WRITER with an output character stream, then rename into OUTPUT-PATHNAME.

WRITER is a function of one argument (the stream).  The assembly is written to
a same-directory unique staging file, then @code{rename-file} installs the final
name so parallel @command{make} jobs never read a truncated blob @file{.s}.

The staging name includes random bits so two concurrent @command{blob-rip-7800}
invocations for the same output never share one @file{*.wip} path: a shared name
plus an initial @code{delete-file} allowed one job to unlink another's staging
file before @code{rename-file}, yielding @code{truename} errors on the missing
@file{#wip} path.

Resolve OUTPUT-PATHNAME against @code{(project-root)} (or cwd) before @code{rename-file}:
SBCL merges a relative destination with @code{*default-pathname-defaults*}, which
@code{with-output-to-file} can leave set to that directory,
producing paths like @file{…/Assets/Source/Generated/…/Blob.*.s} and a failed rename."
  (let* ((output (merge-pathnames output-pathname (uiop:getcwd)))
         (dir (uiop:pathname-directory-pathname output))
         (wip-name (format nil "~A.wip.~36,6,'0R"
                           (pathname-name output)
                           (logxor (ash (get-internal-real-time) 16)
                                   (random #xfffffff))))
         (wip-pathname (merge-pathnames
                        (make-pathname :name wip-name
                                       :type (pathname-type output))
                        dir))
         (ok nil))
    (ensure-directories-exist output)
    (unwind-protect
         (progn
           (with-output-to-file (out wip-pathname :if-exists :supersede
                                                  :external-format :utf-8)
             (funcall writer out))
           (rename-file wip-pathname output)
           (setf ok t))
      (unless ok
        (when (probe-file wip-pathname)
          (ignore-errors (delete-file wip-pathname)))))))

(defun check-height+width-for-blob (height width palette-pixels)
  (assert (zerop (mod width 4)) (width)
          "BLOB ripper requires width mod 4, not ~d (4 × ~{~d + ~d~})"
          width (multiple-value-list (floor width 4)))
  (assert (zerop (mod (1- height) 16)) (height)
          "BLOB ripper requires height mod 16 + 1, not ~d (16 × ~{~d + ~d~})"
          height (multiple-value-list (floor height 16)))
  (format *trace-output* " (~:d×~:d px)" width height)
  (finish-output *trace-output*)
  (assert (= (array-dimension palette-pixels 0) width))
  (assert (= (array-dimension palette-pixels 1) height)))

(defun check-height+width-for-blob-320ac (height width palette-pixels)
  "@cindex dimension validation
@cindex 320A/C mode validation

@table @code
@item Package: skyline-tool
@item Arguments: height (integer), width (integer), palette-pixels (2D array)
@item Returns: nil (signals error if invalid)
@item Side Effects: Outputs dimensions to *trace-output*, signals assertion errors
@end table

Validate dimensions and palette data for 320A/C mode BLOB ripping.

@strong{Requirements:}
@itemize
@item Width must be exactly 320 pixels
@item Height must be (N × 16) + 1 pixels for palette strip
@item Palette pixels array dimensions must match width × height
@end itemize

Signals assertion errors for invalid dimensions."
  (assert (= width 320) (width)
          "320A/C BLOB ripper requires width = 320px, not ~d" width)
  (assert (zerop (mod (1- height) 16)) (height)
          "320A/C BLOB ripper requires height mod 16 + 1, not ~d (16 × ~{~d + ~d~})"
          height (multiple-value-list (floor height 16)))
  (format *trace-output* " (~:d×~:d px)" width height)
  (finish-output *trace-output*)
  (assert (= (array-dimension palette-pixels 0) width))
  (assert (= (array-dimension palette-pixels 1) height)))

(defun NEW-320C-MODE-LOGIC (stamp c2-entries)
  (let ((stamp-colors (remove 0 (all-colors-in-tile stamp))))
    (if (null stamp-colors)
        (list 0 (aref c2-entries 0) (aref c2-entries 1) (aref c2-entries 2))
        (loop for drop from 3 downto 0
              for selected = (loop for i from 0 below 4
                                   when (/= i drop)
                                     collect (aref c2-entries i))
              when (every (lambda (c) (member c selected)) stamp-colors)
                return (cons 0 selected)
              finally (return nil)))))

(defun write-blob-palettes (png output &key (extractor 'extract-palettes) (start-offset 0))
  (fresh-line output)
  (princ "Palette:" output)
  (dolist (*region* '(:ntsc :pal))
    (let ((palettes (funcall extractor
                             (png->palette (png-read:image-data png)))))
      (format output "~%~10t.if TV == ~a
~12t.byte ~a~{~%~12t.byte ~a, ~a, ~a~}
~10t.fi~%"
              *region*
              (atari-colu-string (aref palettes 0 0))
              (append (make-list (* 3 start-offset) :initial-element 0)
                      (mapcan (lambda (pal) (mapcar #'atari-colu-string (coerce (subseq pal 1 4) 'list)))
                              (2a-to-list palettes)))))))

(defun blob/write-span-to-stamp-buffer (span stamp-buffer
                                        &key stamp-offsets serial output id
                                             imperfectp)
  (setf (gethash id stamp-offsets) serial)
  (let ((start (+ (* #x1000 (floor serial #x100))
                  (mod serial #x100))))
    (when (>= start (array-dimension stamp-buffer 0))
      (adjust-array stamp-buffer (+ #x1000 (array-dimension stamp-buffer 0))))
    (format output "~%~10tSpan~x = * + $~4,'0x" id start)
    (dotimes (stamp (length span))
      (let ((stamp-bytes
              (let ((bytes-across (7800-image-to-160ab (elt span stamp)
                                                       :byte-width 1
                                                       :height 16
                                                       :palette #(0 1 2 3)
                                                       :best-fit-p imperfectp)))
                (assert (= 1 (length bytes-across)))
                (car bytes-across))))
        (dotimes (byte 16)
          (let ((i (+ start stamp (* #x100 byte))))
            (assert (let ((b (aref stamp-buffer i)))
                      (or (null b) (zerop b))) ()
                      "Stamp buffer contains ~x at index ~x; serial ~x, stamp ~x"
                      (aref stamp-buffer i) i serial stamp)
            (setf (aref stamp-buffer i)
                  (elt stamp-bytes (- 15 byte)))))))))
(defun convert-stamp-to-palette (stamp pal-index palettes
                                 &key (allow-imperfect-p t))
  (map-region-to-palette
   stamp
   (mapcar #'palette->rgb (coerce (elt (2a-to-list palettes) pal-index) 'list))
   :allow-imperfect-p allow-imperfect-p))

(defun blob/write-spans (spans output &key imperfectp)
  (format output "~2%Spans:~%")
  (let ((stamp-buffer (make-array #x1000 :adjustable t :initial-element 0))
        (stamp-offsets (make-hash-table))
        (serial 0))
    (loop for span being the hash-keys in spans using (hash-value id)
          do (progn
               (if (and (< serial #x100)
                        (>= (+ serial (length span)) #x100))
                   (setf serial #x100))
               (loop
                 (let ((start (+ (* #x1000 (floor serial #x100))
                                (mod serial #x100)))
                       (collision nil))
                   (unless (>= start (array-dimension stamp-buffer 0))
                     (dotimes (stamp (length span))
                       (dotimes (byte 16)
                         (let ((i (+ start stamp (* #x100 byte))))
                           (when (not (zerop (aref stamp-buffer i)))
                             (setf collision t)
                             (return))))))
                   (if collision
                       (incf serial (- #x100 (mod serial #x100)))
                       (return))))
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
  "Write spans for 320A/C mode, handling both monochrome (320A) and color (320C) stamps.

Each span hash value is (id . mode)."
  (format output "~2%Spans:~%")
  (let ((stamp-buffer (make-array #x1000 :adjustable t :initial-element 0))
        (stamp-offsets (make-hash-table))
        (serial 0))
    (loop for span being the hash-keys in spans using (hash-value span-entry)
          for (id . mode) = span-entry
          do (progn
               (if (and (< serial #x100)
                        (>= (+ serial (length span)) #x100))
                   (setf serial #x100))
               ;; Collision avoidance: if this span's scanline 0 would
               ;; overlap with an earlier span's scanline 1 on the same
               ;; page (since MARIA reads scanline N at SpanX + N * #x100),
               ;; advance past the collision to the next page.
               (loop
                 (let ((start (+ (* #x1000 (floor serial #x100))
                                (mod serial #x100)))
                       (collision nil))
                   (unless (>= start (array-dimension stamp-buffer 0))
                     (dotimes (stamp (length span))
                       (dotimes (byte 16)
                         (let ((i (+ start stamp (* #x100 byte))))
                           (when (not (zerop (aref stamp-buffer i)))
                             (setf collision t)
                             (return))))))
                   (if collision
                       (incf serial (- #x100 (mod serial #x100)))
                       (return))))
               (blob/write-span-to-stamp-buffer-320ac span stamp-buffer
                                                       :mode mode
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

(defun convert-4x16-to-320c-bytes (stamp-data)
  "Convert a 4×16 pixel stamp to 16 bytes in 320C format (2 bits per pixel, 4 pixels per byte)."
  (let* ((w (array-dimension stamp-data 0))
         (h (array-dimension stamp-data 1))
         (row-bytes (make-array h :element-type '(unsigned-byte 8))))
    (dotimes (y h)
      (let ((byte 0))
        (dotimes (x (min w 4))
          (setf byte (logior byte (ash (aref stamp-data x y) (* 2 (- 3 x))))))
        (setf (aref row-bytes y) byte)))
    (coerce row-bytes 'list)))

(defun convert-8x16-to-320a-bytes (stamp-data)
  "Convert an 8×16 pixel stamp to 16 bytes in 320A format (1 bit per pixel, 8 pixels per byte)."
  (let* ((w (array-dimension stamp-data 0))
         (h (array-dimension stamp-data 1))
         (row-bytes (make-array h :element-type '(unsigned-byte 8))))
    (dotimes (y h)
      (let ((byte 0))
        (dotimes (x (min w 8))
          (unless (zerop (aref stamp-data x y))
            (setf byte (logior byte (ash 1 (- 7 x))))))
        (setf (aref row-bytes y) byte)))
    (coerce row-bytes 'list)))

(defun combine-4x16-stamps (left right)
  "Combine two adjacent 4×16 stamps into one 8×16 stamp for 320A mode."
  (let ((combined (make-array '(8 16))))
    (dotimes (y 16)
      (dotimes (x 4)
        (setf (aref combined x y) (aref left x y))
        (setf (aref combined (+ 4 x) y) (aref right x y))))
    combined))

(defun stamp-is-320d-p (stamp)
  "Check if a palette-normalized 4×16 stamp fits 320D encoding.
In 320D mode, even positions use the Background or PxC2 registers
(palette indices 0 or 2), odd positions use PxC1 or PxC3 (indices 1 or 3).
The 1-bit encoding maps index > 1 to bit 1.
Returns T if the stamp satisfies this constraint."
  (destructuring-bind (w h) (array-dimensions stamp)
    (dotimes (x w t)
      (dotimes (y h)
        (let ((p (aref stamp x y)))
          (when p
            (if (evenp x)
                (unless (or (= p 0) (= p 2))
                  (return-from stamp-is-320d-p nil))
                (unless (or (= p 1) (= p 3))
                  (return-from stamp-is-320d-p nil)))))))))

(defun convert-4x16-to-320b-bytes (stamp-data)
  "Convert a 4×16 pixel stamp to 16 bytes in 320B format (2 bits per pixel, 4 pixels per byte)."
  (let* ((h (array-dimension stamp-data 1))
         (row-bytes (make-array h :element-type '(unsigned-byte 8))))
    (dotimes (y h)
      (let ((byte 0))
        (dotimes (x 4)
          (setf byte (logior byte (ash (logand (aref stamp-data x y) 3) (* 2 (- 3 x))))))
        (setf (aref row-bytes y) byte)))
    (coerce row-bytes 'list)))

(defun convert-8x16-to-320d-bytes (stamp-data)
  "Convert an 8×16 pixel stamp to 16 bytes in 320D format (1 bit per pixel, 8 pixels per byte).
Each pixel's palette index maps to a bit: index 0 or 1 → 0, index 2+ → 1.
Bit 7 = leftmost pixel (pixel 0 of 8)."
  (let* ((h (array-dimension stamp-data 1))
         (row-bytes (make-array h :element-type '(unsigned-byte 8))))
    (dotimes (y h)
      (let ((byte 0))
        (dotimes (x 8)
          (when (> (aref stamp-data x y) 1)
            (setf byte (logior byte (ash 1 (- 7 x))))))
        (setf (aref row-bytes y) byte)))
    (coerce row-bytes 'list)))

(defun 320a-find-palette-entry (fg-color palettes)
  "Find palette entry (0-7) whose C2 matches FG-COLOR for 320A monochrome mode.
Returns the palette entry index, or 0 if no match."
  (dotimes (p 8 0)
    (when (= fg-color (aref palettes p 2))
      (return p))))

(defun 320c-palette-for-column (stamp last-palette palettes imperfectp column zone)
  "Select palette group (0 or 4) for a 320C stamp, using NEW-320C-MODE-LOGIC."
  (declare (ignore zone))
  (let ((cands (if last-palette
                   (list (if (< last-palette 4) 0 4))
                   '(0 4))))
    (block found
      (dolist (base cands)
        (let ((c2 (vector (aref palettes base 2)
                          (aref palettes (1+ base) 2)
                          (aref palettes (+ base 2) 2)
                          (aref palettes (+ base 3) 2))))
          (when (NEW-320C-MODE-LOGIC stamp c2)
            (return-from found base))))
      (first cands))))

(defun blob/write-span-to-stamp-buffer-320ac (span stamp-buffer
                                              &key mode stamp-offsets serial output id
                                                   imperfectp)
  "Write a span of stamps for 320A/C mode, converting each stamp according to MODE.

When MODE is :320a, each element of SPAN is an 8�~V16 pixel array (two combined 4px stamps);
when MODE is :320c, each element is a 4�~V16 pixel array."
  (declare (ignore imperfectp))
  (setf (gethash id stamp-offsets) serial)
  (let ((start (+ (* #x1000 (floor serial #x100))
                  (mod serial #x100))))
    (when (>= start (array-dimension stamp-buffer 0))
      (adjust-array stamp-buffer (+ #x1000 (array-dimension stamp-buffer 0))))
    (format output "~%~10tSpan~x = * + $~4,'0x" id start)
    (dotimes (stamp (length span))
      (let* ((stamp-data (elt span stamp))
             (bytes (ecase mode
                      (:320c (convert-4x16-to-320c-bytes stamp-data))
                      (:320a (convert-8x16-to-320a-bytes stamp-data)))))
        (dotimes (byte 16)
          (let ((i (+ start stamp (* #x100 byte))))
            (assert (let ((b (aref stamp-buffer i)))
                      (or (null b) (zerop b))) ()
                    "Stamp buffer already contained ~x at index ~x; serial ~x, stamp ~x"
                    (aref stamp-buffer i) i serial stamp)
            (setf (aref stamp-buffer i)
                  (elt bytes (- 15 byte)))))))))
(defun blob-rip-7800 (png-file &optional (imperfectp$ nil))
  "Rip a Bitmap Large Object Block from PNG-FILE

automatically selecting the appropriate Atari 7800 graphics mode based on image width.

@cindex BLOB ripping
@cindex graphics mode auto-detection
@cindex 160A/B mode
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
@item 160A/B Mode
Standard 160-pixel wide graphics mode with 4-color or 13-color palettes
@item 320A/C Mode
Standard 320-pixel wide graphics mode with 2-color or 5-color palettes
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
  (let* ((png (png-read:read-png-file png-file))
         (width (png-read:width png))
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

160A
@itemize
@item 2 pixels per byte (4 bits per pixel)
@item Up to 13 colors (background + 4 palettes × 3 colors each)
@item Variable width (multiple of 2 pixels)
@item Height multiple of 16 + 1 pixels (palette strip)
@end itemize

Pass --imperfect to allow imperfect palette matches instead of signaling errors."
  (let* ((imperfectp (and imperfectp$ (not (emptyp imperfectp$))))
         (*region* (or *region* :ntsc))
         (png (png-read:read-png-file png-file))
         (height (png-read:height png))
         (width (png-read:width png))
         (palette-pixels (png->palette (png-read:image-data png)))
         (output-pathname (png-to-blob-pathname png-file))
         (palettes (extract-palettes palette-pixels))
         (palettes-list (2a-to-lol palettes))
         (stamps (extract-4×16-stamps palette-pixels))
         (zones (floor height 16))
         (columns (floor width 4))
         (spans (make-hash-table :test 'equalp))
         (stamp-counting 0)
         (next-span-id 0))
    (format *trace-output* "accepting ~:[only perfect palette matches~;imperfect palette matches~]… " imperfectp)
    (check-height+width-for-blob height width palette-pixels)
    (print-thumbnail-image png-file)
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
       (blob/write-spans spans output :imperfectp imperfectp)))
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
         (palette-pixels (png->palette (png-read:image-data png)))
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
           (columns (floor width 4))    ; 320 / 4 = 80 columns
           (spans (make-hash-table :test 'equalp))
           (stamp-counting 0)
           (next-span-id 0))
      (print-thumbnail-image png-file)
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
         (write-blob-palettes png output :extractor 'extract-palettes-320ac)
         (format output "~%Zones:~%~10t.byte ~d~10t; zone count" zones)
         (dotimes (zone zones)
           (format output "~2&Zone~d:" zone)
           (let ((col 0)
                 (span nil)
                 (last-palette nil)
                 (last-mode nil))
             (flet ((collect-span ()
                      (when span
                        (push (list col span last-palette last-mode) zone-spans)
                        (setf span nil last-palette nil last-mode nil))))
               (loop while (< col columns)
                     for stamp = (aref stamps col zone)
                     for stamp-mode = (if (stamp-is-monochrome-p stamp) :320a :320c)
                     do (when (= (mod col 20) 0)
                          (format *trace-output* " col ~d/~d…" col columns)
                          (force-output *trace-output*))
                        (cond
                          ;; 320A mode: combine two adjacent monochrome stamps
                          ((and (eql stamp-mode :320a)
                                (< (1+ col) columns)
                                (stamp-is-monochrome-p (aref stamps (1+ col) zone)))
                           (let* ((fg-color (car (remove 0 (all-colors-in-tile stamp))))
                                  (pal-entry (320a-find-palette-entry fg-color palettes))
                                  (left-normalized (limit-region-to-palette
                                                    stamp '(0 1)
                                                    :allow-imperfect-p t))
                                  (right-stamp (aref stamps (1+ col) zone))
                                  (right-normalized (limit-region-to-palette
                                                     right-stamp '(0 1)
                                                     :allow-imperfect-p t))
                                  (combined (combine-4x16-stamps
                                             left-normalized right-normalized)))
                             (cond
                               ((null span)
                                (setf span (list combined)
                                      last-palette pal-entry
                                      last-mode :320a))
                               ((and (= pal-entry last-palette)
                                     (eql :320a last-mode)
                                     (< (length span) 31))
                                (appendf span (list combined)))
                               (t
                                (collect-span)
                                (setf span (list combined)
                                      last-palette pal-entry
                                      last-mode :320a)))
                             (format *trace-output* " 320A")
                             (incf col 2)))
                          ;; Blank stamp — end current span
                          ((blank-stamp-p stamp (aref palettes 0 0))
                           (when span
                             (collect-span))
                           (incf col 1))
                          ;; 320C mode (or isolated monochrome forced to 320C)
                          (t
                           (let* ((palette (320c-palette-for-column
                                            stamp last-palette palettes imperfectp col zone))
                                  (c2-base (if (< palette 4) 0 4))
                                  (c2-entries (vector (aref palettes c2-base 2)
                                                      (aref palettes (1+ c2-base) 2)
                                                      (aref palettes (+ c2-base 2) 2)
                                                      (aref palettes (+ c2-base 3) 2)))
                                  (group-pal (if (< palette 4) 0 4))
                                  (limit-chosen (NEW-320C-MODE-LOGIC stamp c2-entries))
                                  (limit-pal (or limit-chosen
                                                 (list 0 (aref c2-entries 0)
                                                       (aref c2-entries 1)
                                                       (aref c2-entries 2))))
                                  (use-imp (or imperfectp (null limit-chosen)))
                                  (paletted (limit-region-to-palette
                                             stamp limit-pal
                                             :allow-imperfect-p use-imp)))
                             (cond
                               ((null span)
                                (setf span (list paletted)
                                      last-palette group-pal
                                      last-mode :320c))
                               ((and (= group-pal last-palette)
                                     (eql :320c last-mode)
                                     (< (length span) 31))
                                (appendf span (list paletted)))
                               (t
                                (collect-span)
                                (setf span (list paletted)
                                      last-palette group-pal
                                      last-mode :320c)))
                             (incf col 1))))
                     finally
                        (collect-span)))))
         (let ((spans-this-zone (sort (nreverse zone-spans) #'< :key #'first))
               (first-320c-header t))
           (setf zone-spans nil)
           (dolist (entry spans-this-zone)
             (let* ((x (first entry))
                    (span (second entry))
                    (pal (third entry))
                    (mode (fourth entry))
                    (header (if (and (eql mode :320c) first-320c-header)
                                (progn (setf first-320c-header nil) "DLAltHeader")
                                "DLHeader"))
                    (existing (gethash span spans))
                    (id (if existing
                            (car existing)
                            (let ((new-id next-span-id))
                              (incf next-span-id)
                              (cond
                                ((and (< stamp-counting #x100)
                                      (< (+ stamp-counting (length span)) #x100))
                                 (incf stamp-counting (length span)))
                                ((and (< stamp-counting #x100)
                                      (>= (+ stamp-counting (length span)) #x100))
                                 (setf stamp-counting #x100))
                                (t (incf stamp-counting)))
                              (setf (gethash span spans) (cons new-id mode))
                              new-id)))
                    (pos (if (eql mode :320a)
                             (* 2 (- x (* 2 (length span))))
                             (* 2 (- x (length span))))))
               (format output "~%~10t.~a Span~x, ~d, ~d, ~d"
                       header id pal (length span) pos)))
           (format output "~%~10t.DLEnd")
           (blob/write-spans-320ac spans output :imperfectp imperfectp))))
      (format *trace-output* " … done!~%"))))

(defun check-height+width-for-blob-320bd (height width palette-pixels)
  (assert (= width 320) (width)
          "320B/D BLOB ripper requires width = 320px, not ~d" width)
  (assert (zerop (mod (1- height) 16)) (height)
          "320B/D BLOB ripper requires height mod 16 + 1, not ~d (16 × ~{~d + ~d~})"
          height (multiple-value-list (floor height 16)))
  (format *trace-output* " (~:d×~:d px)" width height)
  (finish-output *trace-output*)
  (assert (= (array-dimension palette-pixels 0) width))
  (assert (= (array-dimension palette-pixels 1) height)))

(defun blob/write-span-to-stamp-buffer-320bd (span stamp-buffer
                                              &key mode stamp-offsets serial output id
                                                   imperfectp)
  (declare (ignore imperfectp))
  (setf (gethash id stamp-offsets) serial)
  (let ((start (+ (* #x1000 (floor serial #x100))
                  (mod serial #x100))))
    (when (>= start (array-dimension stamp-buffer 0))
      (adjust-array stamp-buffer (+ #x1000 (array-dimension stamp-buffer 0))))
    (format output "~%~10tSpan~x = * + $~4,'0x" id start)
    (dotimes (stamp (length span))
      (let* ((stamp-data (elt span stamp))
             (bytes (ecase mode
                      (:320b (convert-4x16-to-320b-bytes stamp-data))
                      (:320d (convert-8x16-to-320d-bytes stamp-data)))))
        (dotimes (byte 16)
          (let ((i (+ start stamp (* #x100 byte))))
            (assert (let ((b (aref stamp-buffer i)))
                      (or (null b) (zerop b))) ()
                    "Stamp buffer contains ~x at index ~x; serial ~x, stamp ~x"
                    (aref stamp-buffer i) i serial stamp)
            (setf (aref stamp-buffer i)
                  (elt bytes (- 15 byte)))))))))

(defun blob/write-spans-320bd (spans output &key imperfectp)
  (format output "~2%Spans:~%")
  (let ((stamp-buffer (make-array #x1000 :adjustable t :initial-element 0))
        (stamp-offsets (make-hash-table))
        (serial 0))
    (loop for span being the hash-keys in spans using (hash-value span-entry)
          for (id . mode) = span-entry
          do (progn
               (if (and (< serial #x100)
                        (>= (+ serial (length span)) #x100))
                   (setf serial #x100))
               (loop
                 (let ((start (+ (* #x1000 (floor serial #x100))
                                (mod serial #x100)))
                       (collision nil))
                   (unless (>= start (array-dimension stamp-buffer 0))
                     (dotimes (stamp (length span))
                       (dotimes (byte 16)
                         (let ((i (+ start stamp (* #x100 byte))))
                           (when (not (zerop (aref stamp-buffer i)))
                             (setf collision t)
                             (return))))))
                   (if collision
                       (incf serial (- #x100 (mod serial #x100)))
                       (return))))
               (blob/write-span-to-stamp-buffer-320bd span stamp-buffer
                                                       :mode mode
                                                       :stamp-offsets stamp-offsets
                                                       :serial serial
                                                       :output output
                                                       :id id
                                                       :imperfectp imperfectp)
               (incf serial (length span))))
    (format *trace-output* " writing 320B/D stamps … ")
  (format output "~2%;;; Binary stamp data follows.~%")
  (hex-dump-bytes stamp-buffer output)
  (format output "~2%~10t.bend~%")
  (format output "~2%;;; This size marker is the estimated amount of ROM that this
;;; blob may take up, used for allocation purposes.
;;; $SIZE$~x~%"
          (+ #x20
             (* 4 (hash-table-count spans))
             (length stamp-buffer)))))

(defun blob-rip-7800-320bd (png-file &optional (imperfectp$ nil))
  (let* ((*machine* 7800)
         (*region* :ntsc)
         (png (png-read:read-png-file png-file))
         (height (png-read:height png))
         (width (png-read:width png))
         (palette-pixels (png->palette (png-read:image-data png)))
         (output-pathname (png-to-blob-pathname png-file))
         (imperfectp (or (eql :imperfect imperfectp$)
                         (equal imperfectp$ "--imperfect"))))
    (format *trace-output* "accepting ~:[only perfect palette matches~;imperfect palette matches~]… " imperfectp)
    (check-height+width-for-blob-320bd height width palette-pixels)
    (let* ((zone-spans nil)
           (palettes (extract-palettes-320ac palette-pixels))
           (palettes-list (2a-to-lol palettes))
           (bg-color (aref palettes 0 0))
           (group-palettes
             (make-array (list 2 5) :initial-contents
                         (list (list bg-color
                                     (aref palettes 0 2)
                                     (aref palettes 1 2)
                                     (aref palettes 2 2)
                                     (aref palettes 3 2))
                               (list bg-color
                                     (aref palettes 4 2)
                                     (aref palettes 5 2)
                                     (aref palettes 6 2)
                                     (aref palettes 7 2)))))
           (group-palettes-list (2a-to-lol group-palettes))
           (stamps (extract-4×16-stamps palette-pixels))
           (zones (floor height 16))
           (columns (floor width 4))
           (spans (make-hash-table :test 'equalp))
           (stamp-counting 0)
           (next-span-id 0))
      (print-thumbnail-image png-file)
      (format *trace-output* " generating 320B/D drawing lists in ~a… " (enough-namestring output-pathname))
      (force-output *trace-output*)
      (format *trace-output* " zones=~d, stamps=~d×~d~%" zones columns zones)
      (force-output *trace-output*)
      (%write-blob-assembly-atomically
       output-pathname
       (lambda (output)
         (format output ";;; Bitmap Large Object Block for Atari 7800 (320B/D mode)
;;; Derived from source file ~a. This is a generated file.~3%
Blob_~a:~10t.block~2%"
                 (enough-namestring png-file)
                 (assembler-label-name (pathname-name png-file)))
         (format output "~%Mode:~10t.byte Mode320BD")
         (write-blob-palettes png output :extractor 'extract-palettes-320ac)
         (format output "~%Zones:~%~10t.byte ~d~10t; zone count" zones)
         (dotimes (zone zones)
           (format output "~2&Zone~d:" zone)
           (let ((col 0)
                 (span nil)
                 (last-palette nil)
                 (last-mode nil))
             (flet ((collect-span ()
                      (when span
                        (push (list col span last-palette last-mode) zone-spans)
                        (setf span nil last-palette nil last-mode nil))))
                (loop while (< col columns)
                      for stamp = (aref stamps col zone)
                      do (when (= (mod col 20) 0)
                           (format *trace-output* " col ~d/~d…" col columns)
                           (force-output *trace-output*))
                                  (let* ((resolve-palette
                                    (lambda (p)
                                      (if (>= p 100)
                                          (elt group-palettes-list (- p 100))
                                          (elt palettes-list p))))
                                  (match
                                    (or (when (and last-palette
                                                   (tile-fits-palette-p
                                                    stamp
                                                    (funcall resolve-palette last-palette)))
                                          (list last-palette
                                                (funcall resolve-palette last-palette)
                                                nil))
                                        ;; Try group matching (320C: BG + 4 C2 values)
                                        (let ((group (best-palette
                                                      stamp group-palettes
                                                      :allow-imperfect-p imperfectp
                                                      :x col :y zone)))
                                          (when group
                                            (let* ((c2-base (if (zerop group) 0 4))
                                                   (c2-ents (vector (aref palettes c2-base 2)
                                                                    (aref palettes (1+ c2-base) 2)
                                                                    (aref palettes (+ c2-base 2) 2)
                                                                    (aref palettes (+ c2-base 3) 2)))
                                                   (limit-chosen
                                                     (NEW-320C-MODE-LOGIC stamp c2-ents)))
                                              (list (+ 100 group)
                                                    (or limit-chosen
                                                        (list 0 (aref c2-ents 0)
                                                                 (aref c2-ents 1)
                                                                 (aref c2-ents 2)))
                                                    (null limit-chosen)))))
                                        ;; Fall back to individual palette (320B/D)
                                        (let ((idx (best-palette
                                                    stamp palettes
                                                    :allow-imperfect-p imperfectp
                                                    :x col :y zone)))
                                          (list idx (elt palettes-list idx) nil))))
                                  (palette (first match))
                                  (limit-pal (second match))
                                  (approximate-p (third match))
                                  (paletted (limit-region-to-palette
                                             stamp limit-pal
                                             :allow-imperfect-p (or imperfectp approximate-p)))
                                 (stamp-mode (if (stamp-is-320d-p paletted) :320d :320b)))
                           (cond
                            ;; 320D mode: combine two adjacent stamps
                            ((and (eql stamp-mode :320d)
                                  (< (1+ col) columns)
                                    (let ((next-paletted (limit-region-to-palette
                                                          (aref stamps (1+ col) zone)
                                                          limit-pal
                                                          :allow-imperfect-p imperfectp)))
                                      (stamp-is-320d-p next-paletted))
                                    (< (length span) 31))
                               (let* ((next-stamp (aref stamps (1+ col) zone))
                                      (next-paletted (limit-region-to-palette
                                                      next-stamp limit-pal
                                                      :allow-imperfect-p imperfectp))
                                    (combined (combine-4x16-stamps paletted next-paletted)))
                               (cond
                                 ((null span)
                                  (setf span (list combined)
                                        last-palette palette
                                        last-mode :320d))
                                 ((and (= palette last-palette)
                                       (eql :320d last-mode))
                                  (appendf span (list combined)))
                                 (t
                                  (collect-span)
                                  (setf span (list combined)
                                        last-palette palette
                                        last-mode :320d)))
                               (format *trace-output* " 320D")
                               (incf col 2)))
                            ;; Blank stamp — end current span
                            ((blank-stamp-p stamp (aref palettes 0 0))
                             (when span
                               (collect-span))
                             (incf col 1))
                            ;; 320B mode
                            (t
                             (cond
                               ((null span)
                                (setf span (list paletted)
                                      last-palette palette
                                      last-mode :320b))
                               ((and (= palette last-palette)
                                     (eql :320b last-mode)
                                     (< (length span) 31))
                                (appendf span (list paletted)))
                               (t
                                (collect-span)
                                (setf span (list paletted)
                                      last-palette palette
                                      last-mode :320b)))
                             (incf col 1))))
                     finally
                        (collect-span)))))
         (let ((spans-this-zone (sort (nreverse zone-spans) #'< :key #'first))
               (first-320d-header t))
           (setf zone-spans nil)
           (dolist (entry spans-this-zone)
             (let* ((x (first entry))
                    (span (second entry))
                    (pal (third entry))
                    (mode (fourth entry))
                    (header (if (and (eql mode :320d) first-320d-header)
                                (progn (setf first-320d-header nil) "DLAltHeader")
                                "DLHeader"))
                    (existing (gethash span spans))
                    (id (if existing
                            (car existing)
                            (let ((new-id next-span-id))
                              (incf next-span-id)
                              (cond
                                ((and (< stamp-counting #x100)
                                      (< (+ stamp-counting (length span)) #x100))
                                 (incf stamp-counting (length span)))
                                ((and (< stamp-counting #x100)
                                      (>= (+ stamp-counting (length span)) #x100))
                                 (setf stamp-counting #x100))
                                (t (incf stamp-counting)))
                              (setf (gethash span spans) (cons new-id mode))
                              new-id)))
                    (pos (if (eql mode :320d)
                             (* 2 (- x (* 2 (length span))))
                             (* 2 (- x (length span))))))
               (format output "~%~10t.~a Span~x, ~d, ~d, ~d"
                       header id pal (length span) pos)))
           (format output "~%~10t.DLEnd")
           (blob/write-spans-320bd spans output :imperfectp imperfectp))))
      (format *trace-output* " … done!~%"))))

(defun write-7800-binary (binary-out bytes-lists)
  (with-output-to-file (binary binary-out
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
    (let ((page-length (length (first bytes-lists))))
      (unless (<= 0 page-length #x100)
        (error "Page length is nonsense, must be 0-256 ($0-$100) but got ~:d ($~:*~x)" page-length))
      (format *trace-output* "~&~A: Writing ~:D pages, each of which is ~:D bytes (out of 256 possible)~
~@[, last section has ~:D bytes free though~]; total file size should be ~:d ($~:*~x) byte~:p…"
              binary-out (* (floor page-length #x100)
                            (if (<= page-length #x100)
                                (length bytes-lists)
                                #x100))
              (if (<= page-length #x100)
                  page-length
                  #x100)
              (mod page-length #x100)
              (* (length bytes-lists)
                 #x100
                 (ceiling page-length #x100)))
      (finish-output *trace-output*)
      (dolist (bytes-list bytes-lists)
        (dolist (byte bytes-list)
          (write-byte byte binary))
        (when (< page-length #x100)
          (dotimes (i (- #x100 page-length))
            (write-byte 0 binary))))
      (format *trace-output* " done.~%"))))

(defun interleave-7800-bytes (bytes-lists)
  "Interleave and reverse bytes.
Each element of BYTES-LISTS is one bank row; return a list of rows (each row is
one page for write-7800-binary). Empty input yields an empty list."
  (when (null bytes-lists)
    (return-from interleave-7800-bytes '()))
  (loop for j below (apply #'max (mapcar #'length bytes-lists))
        collect (loop for i from (1- (length bytes-lists)) downto 0
                      collect (if (< j (length (elt bytes-lists i)))
                                  (elt (elt bytes-lists i) j)
                                  0))))

(defgeneric parse-7800-object (mode png &key width height palette))

(defun 7800-image-to-160ab (image &key byte-width height palette best-fit-p)
  "Convert image to combined Atari 7800 160A/B graphics format.

Converts a pixel image to 160A mode bytes for the Atari 7800. In 160A mode,
each pixel is 2 bits (4 colors) and pixels are packed 4 per byte.

@table @asis
@item IMAGE
2D array of pixel indices
@item BYTE-WIDTH
Width of image in bytes (each byte = 4 pixels)
@item HEIGHT
Height of image in pixels
@item PALETTE
Color palette array (optional)
@item BEST-FIT-P
If true, use closest color match; if false, signal error for invalid colors
@item Returns
List of byte lists, one per column
@end table

@xref{fun:7800-image-to-320a}, @xref{fun:7800-image-to-320c}."
  (let ((bytes-across (list)))
    (dotimes (b byte-width)
      (let ((bytes (list)))
        (dotimes (y height)
          (let* ((byte-pixels (extract-region image
                                              (* b 4) y
                                              (* (1+ b) 4) (1+ y)))
                 (indices (pixels-into-palette byte-pixels palette
                                               :x0 (* b 4) :y0 y
                                               :best-fit-p best-fit-p)))
            (push (logior
                   (ash (aref indices 0) 6)
                   (ash (aref indices 1) 4)
                   (ash (aref indices 2) 2)
                   (aref indices 3))
                  bytes)))
        (push (reverse bytes) bytes-across)))
    (reverse bytes-across)))

(defun 7800-image-to-160a (image &key byte-width height palette best-fit-p)
  "Convert image to Atari 7800 160A graphics format.

Converts a pixel image to 160A mode bytes for the Atari 7800. In 160A mode,
each pixel is 2 bits (4 colors) and pixels are packed 4 per byte.

@table @asis
@item IMAGE
2D array of pixel indices
@item BYTE-WIDTH
Width of image in bytes (each byte = 4 pixels)
@item HEIGHT
Height of image in pixels
@item PALETTE
Color palette array (optional)
@item BEST-FIT-P
If true, use closest color match; if false, signal error for invalid colors
@item Returns
List of byte lists, one per column
@end table

@xref{fun:7800-image-to-320a}, @xref{fun:7800-image-to-320c}."
  (let ((bytes-across (list)))
    (dotimes (b byte-width)
      (let ((bytes (list)))
        (dotimes (y height)
          (let* ((byte-pixels (extract-region image
                                              (* b 4) y
                                              (* (1+ b) 4) (1+ y)))
                 (indices (pixels-into-palette byte-pixels palette
                                               :x0 (* b 4) :y0 y
                                               :best-fit-p best-fit-p)))
            (push (logior
                   (ash (aref indices 0) 6)
                   (ash (aref indices 1) 4)
                   (ash (aref indices 2) 2)
                   (aref indices 3))
                  bytes)))
        (push (reverse bytes) bytes-across)))
    (reverse bytes-across)))

(defun compile-art-7800 (index-out index-in &optional (region (or *region* :ntsc)))
  "Compile 7800 art assets from INDEX-IN to binary at INDEX-OUT.
  Parses a 7800 art index file, converts the referenced PNG assets into
  interleaved 7800-format bytes (bitplanes for Maria), and writes the
  resulting binary file.
  @table @asis
  @item INDEX-OUT
  Output path for the compiled binary
  @item INDEX-IN
  Input path for the 7800 art index file
  @item REGION
  Video region (:ntsc or :pal) (default: :ntsc)
  @item Side Effects
  Sets *machine* to 7800 and *region* to REGION during compilation
  @end table
  @xref{fun:read-7800-art-index}, @xref{fun:interleave-7800-bytes}."
  (let ((*machine* 7800)
        (*region* region)
        (name (pathname-name index-out)))
    (write-7800-binary (make-pathname
                        :directory (list :relative "Object" (machine-directory-name) "Assets") 
                        :name (format nil "~a.~a"
                                      name
                                      (string-upcase (string region)))
                        :type "o")
                       (interleave-7800-bytes
                        (parse-into-7800-bytes
                         (read-7800-art-index index-in))))
    (with-output-to-file (index index-out :if-exists :supersede)
      (format index ";;; This is a generated file, from ~a" index-in)
      (format index "
~10t.if TV == NTSC
~12t.binary \"~a.NTSC.o\"
~10t.else
~12t.binary \"~a.PAL.o\"
~10t.fi~2%"
              name name))))

(defun display-maria-art (stream &key dump mode address colors width (unit #x10)
                                      var-colors)
  (flet ((peek (offset)
           (if (< (+ address offset) (length dump))
               (aref dump (+ address offset))
               #xff)))
    (clim:formatting-table (stream :x-spacing 0 :y-spacing 0)
      (dotimes (y #x10)
        (clim:formatting-row (stream)
          (ecase mode
            (:160a (dotimes (byte width)
                     (let* ((bits (peek (+ (* (- #x0f y) #x100)
                                           byte))))
                       (clim:formatting-cell (stream)
                         (print-wide-pixel (elt colors
                                                (ash (logand #b11000000 bits) -6))
                                           stream :unit unit))
                       (clim:formatting-cell (stream)
                         (print-wide-pixel (elt colors
                                                (ash (logand #b00110000 bits) -4))
                                           stream :unit unit))
                       (clim:formatting-cell (stream)
                         (print-wide-pixel (elt colors
                                                (ash (logand #b00001100 bits) -2))
                                           stream :unit unit))
                       (clim:formatting-cell (stream)
                         (print-wide-pixel (elt colors
                                                (logand #b00000011 bits))
                                           stream :unit unit)))))
            (:160b (dotimes (byte width)
                     (let* ((bits (peek (+ (* (- #x0f y) #x100)
                                           byte)))
                            (left-pixel-c (ash (logand #b11000000 bits) -6))
                            (right-pixel-c (ash (logand #b00110000 bits) -4))
                            (left-pixel-p (ash (logand #b00001100 bits) -2))
                            (right-pixel-p (logand #b00000011 bits))
                            (left-color (logior (ash left-pixel-p 2) left-pixel-c))
                            (right-color (logior (ash right-pixel-p 2) right-pixel-c)))
                       (clim:formatting-cell (stream)
                         (cond
                           ((and var-colors (member left-color '(4 8 12)))
                            (print-wide-pixel
                             (elt colors (mod (elt var-colors (mod (1- (/ left-color 4)) 3)) #x10))
                             stream :unit unit))
                           ((member left-color '(4 8 12))
                            (print-wide-pixel (mod (elt colors 0) #x100)
                                              stream :unit unit))
                           (t
                            (print-wide-pixel (mod (elt colors left-color) #x100)
                                              stream :unit unit))))
                       (clim:formatting-cell (stream)
                         (cond
                           ((and var-colors (member right-color '(4 8 12)))
                            (print-wide-pixel
                             (elt colors (mod (elt var-colors (mod (1- (/ right-color 4)) 3)) #x10))
                             stream :unit unit))
                           ((member right-color '(4 8 12))
                            (print-wide-pixel (mod (elt colors 0) #x100)
                                              stream :unit unit))
                           (t
                            (print-wide-pixel (mod (elt colors right-color) #x100)
                                              stream :unit unit)))))))))))))
(defun extract-palette-from-bottom (palette-pixels)
  "Extract palette colors from the bottom row of the image"
  (let* ((height (array-dimension palette-pixels 0))
         (width (array-dimension palette-pixels 1))
         (last-row (1- height))
         (palette-colors (list)))
    ;; Extract colors from the bottom row
    (dotimes (x width)
      (let ((color (aref palette-pixels last-row x)))
        (unless (member color palette-colors :test #'equal)
          (push color palette-colors))))
    (reverse palette-colors)))
(defun grab-7800-palette (mode png)
  "Extract the palette values for mode MODE from graphic PNG"
  (when (eql :320a mode)
    (return-from grab-7800-palette nil))
  (let* ((palette-size (ecase mode
                         (:160a 32)
                         (:160b 16)
                         (:320b 4)
                         (:320c 8)
                         (:320d 8)))
         (last-row (1- (array-dimension png 1)))
         (palette-strip (extract-region png
                                        0 last-row
                                        palette-size (1+ last-row))))
    (let ((palette (loop for i below palette-size
                         collect (aref palette-strip i 0))))
      (if (tty-xterm-p)
          (format *trace-output* "~&Palette detected:~%~{ ~5t~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}~^;  ~45t~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}~^;~}"
                  (mapcar #'palette-to-ansi-pairs palette))
          (format *trace-output* "~&Palette detected: ~{$~2,'0x~^, ~}" palette))
      palette)))
(defun palette-register-name (i rel)
  (cond
    ((zerop i) "Background")
    ((<= 1 i 3) (format nil "P~dC~d"
                        rel
                        i))
    ((= 4 i) "VarColor1")
    ((<= 5 i 7) (format nil "P~dC~d"
                        (+ 1 rel)
                        (- i 4)))
    ((= 8 i) "VarColor2")
    ((<= 9 i 11) (format nil "P~dC~d"
                         (+ 2 rel)
                         (- i 8)))
    ((= 12 i) "VarColor3")
    ((<= 13 i 15) (format nil "P~dC~d"
                          (+ 3 rel)
                          (- i 12)))
    (t nil)))
(defun parse-into-7800-bytes (art-index)
  (let ((bytes (list)))
    (dolist (art-item art-index (nreverse bytes))
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&~A: parsing in mode ~A (start at $~2,'0x)… "
                png-name mode (length bytes))
        (let* ((png (png-read:read-png-file png-name))
               (palette-pixels (png->palette (png-read:image-data png)
                                             (png-read:transparency png)))
               (palette (grab-7800-palette mode palette-pixels))
               (mode-aspect (if (eql (char (string-trim " " mode) 0) #\1)
                                2 1)))
          (print-thumbnail-image png-name *trace-output* mode-aspect)
          (appendf bytes
                   (parse-7800-object mode palette-pixels :width width-px :height height-px
                                                          :palette palette)))
        (format *trace-output* " … Done. (ends at $~2,'0x)" (1- (length bytes)))))))

(defun read-7800-art-index (index-in)
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while (and line (plusp (length line)) (not (char= #\; (char line 0))))
            do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page)
                                        line)))
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
