(in-package :skyline-tool)

(defun compile-5200-mode-e-bitmap (image-pixels &key (png-file (make-pathname :name "tmp5200" :type "png"))
                                                     (target-dir "Object/5200/")
                                                     output-pathname
                                                     block-label
                                                     (height (array-dimension image-pixels 1))
                                                     (width (array-dimension image-pixels 0))
                                                     (compressp (< 512 (* height width)))
                                                     (color-per-line-p t)
                                                     base-palette
                                                     lenient-palette-p)
  (let* ((base-dir (if (typep target-dir 'pathname)
                       (merge-pathnames target-dir (project-root))
                       (merge-pathnames (pathname (or target-dir "Object/5200/"))
                                        (project-root))))
         (out-file-name (or output-pathname
                            (merge-pathnames
                             (make-pathname :name
                                            (pathname-name png-file)
                                            :type "s")
                             base-dir)))
         (label-name (or block-label
                         (assembler-label-name (pathname-name png-file)))))
    (ensure-directories-exist out-file-name)
    (format *trace-output* "~% Ripping Mode ~a pixmap graphics from ~D×~D image…"
            (if (< height 97) "D/E" "E")
            width height)
    (finish-output *trace-output*)
    (with-output-to-file (source-file out-file-name :if-exists :supersede)
      (assert (= height (array-dimension image-pixels 1)))
      (assert (= width (array-dimension image-pixels 0)))
      (multiple-value-bind (shape colors) (mode-e-interpret image-pixels
                                                            :base-palette base-palette
                                                            :color-per-line-p color-per-line-p
                                                            :lenient-palette-p lenient-palette-p)
        (assert (= (length shape) (/ (* height width) 4)))
        (assert (= (length colors) (if color-per-line-p height 1)))
        (format source-file ";;; -*- fundamental -*-
;;; Compiled pixmap data from ~a
;;; Edit the original (probably ~a),
;;; editing this file is futile.

~a:	.block
~10tLength = ~d ; bytes total for this data ~:[(stored uncompressed)~;(after decompression)~]
~10tHeight = ~d
~10tWidth = ~d
~{~%;;; ~{~a~}~}

Shape:
~%~10t.text x\"~{~2,'0x~}\"
CoLu:
~{~%~10t;; ~{~15a ~15a ~15a ~15a~}~}
~%~10t.text x\"~{~2,'0x~}\"
 .bend
"
                (enough-namestring png-file)
                (enough-namestring (make-pathname :defaults png-file :type "xcf"))
                label-name
                (+ 2 (length shape) (length colors))
                compressp
                height width
                (mapcar (lambda (row) (mapcar #'bit-pairs-to-art row))
                        (rows-of-width shape width))
                (if compressp
                    (coerce (zx7-compress (make-fillable-vector shape)
                                          :base-name (concatenate 'string
                                                                  (pathname-name png-file)
                                                                  ".Shape"))
                            'list)
                    shape)
                (mapcar (lambda (line) (mapcar #'atari-colu-string line)) colors)
                (if (and compressp color-per-line-p)
                    (coerce (zx7-compress (make-fillable-vector (flatten colors))
                                          :base-name (concatenate 'string
                                                                  (pathname-name png-file)
                                                                  ".Colors"))
                            'list)
                    (flatten colors))))
      (format *trace-output* "~% Done writing to ~A" out-file-name)
      t)))

(defun compile-gtia-player (png-file out-dir
                            height width image-pixels)
  (let* ((base-dir (merge-pathnames (pathname (or out-dir "Object/5200/"))
                                    (project-root)))
         (out-file-name (merge-pathnames
                         (make-pathname :name
                                        (pathname-name png-file)
                                        :type "s")
                         base-dir)))
    (ensure-directories-exist out-file-name)
    (format *trace-output* "~% Ripping GTIA Player graphics from ~D×~D image"
            width height)
    (finish-output *trace-output*)
    (with-output-to-file (source-file out-file-name
                                      :if-exists :supersede)
      (multiple-value-bind (shape colors) (tia-player-interpret image-pixels)
        (format source-file ";;; -*- fundamental -*-
;;; Compiled sprite data from ~a
;;; Edit the original (probably Source/Art/~:*~a.xcf),
;;; editing this file is futile.

~a:	.block
 Height = ~d
 Width = ~d
Shape:~{~a~}
;CoLu:~{~%	;.byte ~{CoLu(~a, $~1x)~}~}
 .bend
"
                (pathname-name png-file)
                (assembler-label-name (pathname-base-name png-file))
                height width
                (if (and (mod height 16) (> height 200))
                    (mapcar #'byte-and-art (reverse-16 shape))
                    (mapcar #'byte-and-art (reverse-7-or-8 shape)))
                (mapcar #'atari-colu colors)))
      (format *trace-output* "~% Done writing to ~A" out-file-name)
      t)))

(defun pretty-mob-data-listing-vic2 (mob)
  (mapcar #'bytes-and-art
          (group-into-3
           (map 'list #'char-code mob))))

(defun mob-index+bitmap+color-sets (more-mobs)
  (loop for mob = (subseq more-mobs 0 63)
        for more on more-mobs by (curry #'nthcdr 64)
        for i from 0
        collect (list i
                      (pretty-mob-data-listing-vic2 mob)
                      (char-code (last-elt mob)))))

(defun compile-mob (png-file out-dir height width image-nybbles)
  (let ((out-file (merge-pathnames
                   (make-pathname :name
                                  (pathname-name png-file)
                                  :type "s")
                   out-dir)))
    (format *trace-output* "~% Ripping MOBs from ~D×~D sprite" width height)
    (with-output-to-file (binary-file out-file
                                      :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
      (multiple-value-bind (mobs index) (gather-mobs image-nybbles height width)
        (assert (>= 6 (length index)) nil
                "There can be at most 6 (non-empty) MOBs in a sprite; ~
got ~:D MOB~:P"
                (length index))
        (format *trace-output* "~%Writing ~:D MOB~:P" (length index))
        (when (< 1 (length index))
          (warn "MOBs not stacked vertically won't work yet. ~
Proceed with caution."))
        (format binary-file ";;; -*- fundamental -*-
;;; Compiled sprite data from ~a
;;; Edit the original (probably art/sprites/~:*~a.xcf),
;;; editing this file is futile.

 .byte ~d	; length of index
 .byte ~{$~x~^, ~}	; MOBs in sprite
 .align 64
~{~{
 ;; MOB ~x data
~{~a~}
 .byte ~d	; Sprite distinct color
~}~}"
                (pathname-name png-file)
                (length index)
                (mapcar #'car index)  ; TODO: #1226 capture relative positioning
                (mob-index+bitmap+color-sets mobs)))
      (format *trace-output* "~% Done writing to ~A" out-file))))

(defgeneric compile-font-generic (machine-type format source-file-base-name font-input)
  (:method (machine-type format source-file-base-name font-input)
    (warn "No handler for converting art into ~a format to create ~a" machine-type source-file-base-name)
    (with-output-to-file (source-file (make-source-file-name source-file-base-name)
                                      :if-exists :supersede)
      (format source-file ";;; -*- asm -*-
;;; TODO: #1243: #1227 write the function to generate this file's contents
 * = 0 ~% brk~%"))))

(defun tia-font-interpret (pixels x y)
  (loop for byte from 4 downto 0
        collecting
        (reduce #'logior
                (loop for bit from 0 to 3
                      collect
                      (if (plusp (aref pixels (+ bit (* x 4)) (+ byte (* y 5))))
                          (expt 2 (- 3 bit))
                          0)))))

(defun antic-font-interpret (pixels x y)
  (loop for byte from 0 below 8
        collecting
        (reduce #'logior
                (loop for bit from 0 below 8
                      collect
                      (if (plusp (aref pixels (+ bit (* x 8)) (+ byte (* y 8))))
                          (expt 2 (- 7 bit))
                          0)))))

(defun png->bits (png-file)
  (let ((height (png-read:height png-file))
        (width (png-read:width png-file))
        (rgb (png-read:image-data png-file))
        (α (png-read:transparency png-file)))
    (check-type height (integer 0 *))
    (check-type width (integer 0 *))
    (check-type rgb array)
    (check-type α (or null array))
    (destructuring-bind (w h bpp) (array-dimensions rgb)
      (unless (and (= h height) (= w width) (= bpp 4))
        (error "WTF? File size mismatches contents"))
      (let ((image (make-array (list width height) :element-type '(unsigned-byte 1))))
        (loop for y from 0 below height
              do (loop for x from 0 below width
                       do (setf (aref image x y)
                                (cond ((and α (< 128 (aref α x y))) 0)
                                      ((> (* 3 128)
                                          (+ (aref rgb x y 0) (aref rgb x y 1) (aref rgb x y 2)))
                                       1)
                                      (t 0)))))
        image))))

(defun tia-font-guide (source-file pixels chars-width)
  (dotimes (line 8)
    (dotimes (row 5)
      (terpri source-file)
      (princ ";;; " source-file)
      (loop for i from (+ (* line 6) 0) to (min 47
                                                (+ (* line 6) 5))
            for column = (mod i 12)
            for x = (mod i chars-width)
            for y = (floor i chars-width)
            do (princ
                (subseq
                 (elt (mapcar #'bits-to-art
                              (mapcar (lambda (byte)
                                        (format nil "~2,8,'0r" byte))
                                      (tia-font-interpret pixels x y)))
                      (- 4 row))
                 4)
                source-file)
            do (princ #\space source-file)))
    (terpri source-file)))

(defun antic-font-guide (source-file pixels chars-width)
  (dotimes (line 8)
    (dotimes (row 8)
      (terpri source-file)
      (princ ";;; " source-file)
      (loop for i from (* line 8) below (* (1+ line) 8)
            for column = (mod i 8)
            for x = (mod i chars-width)
            for y = (floor i chars-width)
            do (princ
                (subseq
                 (elt (mapcar #'bits-to-art
                              (mapcar (lambda (byte)
                                        (format nil "~2,8,'0r" byte))
                                      (antic-font-interpret pixels x y)))
                      row)
                 8)
                source-file)
            do (princ #\space source-file)))
    (terpri source-file)))

(defun tia-font-write (source-file pixels chars-width bit-shift)
  (loop for i from 0 to 47
        for x = (mod i chars-width)
        for y = (floor i chars-width)
        do (format source-file "~%	;; char #~x ~:[(right)~;(left)~]~{~a~}"
                   i (= 4 bit-shift)
                   (mapcar #'byte-and-art
                           (mapcar (rcurry #'ash bit-shift)
                                   (tia-font-interpret pixels x y))))))

(defun antic-font-write (source-file pixels)
  (loop with chars-width = (floor (array-dimension pixels 0) 8)
        with chars-height = (floor (array-dimension pixels 1) 8)
        with char-count = (* chars-width chars-height)
        for i from 0 below char-count
        for x = (mod i chars-width)
        for y = (floor i chars-width)
        do (format source-file "~%	;; char #~d ~:* $~2,'0x (“~c” or “~a”) ~{~a~}"
                   i
                   (code-char (+ #x20 i))
                   (substitute #\Space #\_
                               (string-capitalize (char-name (code-char (+ #x20 i)))))
                   (mapcar #'byte-and-art
                           (antic-font-interpret pixels x y)))))

(defmethod compile-font-generic ((machine-type (eql 2600))
                                 format source-file-base-name font-input)
  (let* ((png-image (png-read:read-png-file font-input))
         (chars-width (/ (png-read:width png-image) 4))
         (pixels (png->bits png-image)))
    (with-output-to-file (source-file (make-source-file-name source-file-base-name)
                                      :if-exists :supersede)
      (format source-file ";;; -*- asm -*-
;;; Font data compiled from ~a
;;; This is a generated file; editing it would be futile~2%"
              font-input)
      (assert (= 48 (* chars-width (/ (png-read:height png-image) 5))))
      (format source-file "~%;;;~|~%TIAFont:
;; Overview: (font follows, inverted, each char repeated for each nybble)")
      (tia-font-guide source-file pixels chars-width)
      (format source-file "~%;;;~|~%TIAFontLeft:")
      (tia-font-write source-file pixels chars-width 4)
      (format source-file "~%;;;~|~%TIAFontRight:~%")
      (tia-font-write source-file pixels chars-width 0)
      (format source-file "~2%;;; end of file.~%")
      (format *trace-output* "~&Wrote ~a (from ~a)" source-file-base-name font-input))))

(defmethod compile-font-generic ((machine-type (eql 5200))
                                 format source-file-base-name font-input)
  (let* ((png-image (png-read:read-png-file font-input))
         (chars-width (/ (png-read:width png-image) 8))
         (pixels (png->bits png-image))
         (char-count (* chars-width (/ (png-read:height png-image) 8)))
         (source-file-name (make-source-file-name source-file-base-name "Assets")))
    (with-output-to-file (source-file source-file-name :if-exists :supersede)
      (format source-file ";;; -*- asm -*-
;;; Font data compiled from ~a
;;; This is a generated file; editing it would be futile
;;; ~d characters in this font~2%"
              font-input char-count)
      (assert (member char-count '(64 256)))
      (format source-file "~%;;;~|~%AnticFont:")
      (antic-font-write source-file pixels)
      (format source-file "~2%;;; end of file.~%")
      (format *trace-output* "~&Wrote ~a (from ~a)" (enough-namestring source-file-name) font-input))))

(defun compile-font-command (source-file-name font-input)
  "Create SOURCE-FILE-NAME from FONT-INPUT (PNG)"
  (destructuring-bind (obj genr ass source-file-base-name) (split-sequence #\/ source-file-name)
    (destructuring-bind (font s) (split-sequence #\. source-file-base-name)
      (assert (equal s "s"))
      (assert (equal obj "Source"))
      (assert (equal genr "Generated"))
      (assert (equal ass "Assets"))
      (let ((*machine* (or (when (not (eql :unknown *machine*)) *machine*) 7800)))
        (compile-font-generic *machine* nil font font-input)))))

(defun compile-font-8×8 (png-file out-dir height width image-nybbles)
  "Compile 8×8 font with deduplication: unique char definitions + mapping table.

Shared by 7800, 5200, Lynx, VIC-II, VDC, ClcV, and Intv.  Outputs
(defun bitmaps-for-tia-merged-tiles (merged-tiles)
  (check-type merged-tiles hash-table)
  (let* ((tiles (sort-hash-table-by-values merged-tiles))
         (raw-tile-count (array-dimension *tia-tiles* 0))
         (tile-bitmaps (make-array (list (length tiles) 7))))
    (loop
      for tile in tiles
      for i from 0
      do (dotimes (line 7)
           (destructuring-bind (left right big-endian-p) tile
             (assert (<= left raw-tile-count))
             (assert (<= right raw-tile-count))
             (let ((byte (logior (ash (aref *tia-tiles* left line) 4)
                                 (aref *tia-tiles* right line))))
               (setf (aref tile-bitmaps i line)
                     (if big-endian-p
                         byte
                         (reverse-byte byte)))))))
    tile-bitmaps))

(defun write-tia-bitmaps-scan-line (tile-bitmaps scan-line)
  (check-type tile-bitmaps (array t (* 7)))
  (check-type scan-line (integer 0 6))
  (format t "~%~|~%TilesScan~d:
 ;; ~:(~:*~:r~) three scan-lines (of 7 triples) in each group of 21"
          (1+ scan-line))
  (format t "~%	.text x\"~{~2,'0x~}\""
          (loop
            for i from 0 below (array-dimension tile-bitmaps 0)
            collect (let ((byte (aref tile-bitmaps i scan-line)))
                      (check-type byte (integer 0 255))
                      byte))))

(defun write-tia-tiles-trailer (tile-count)
  (check-type tile-count (integer 2 255))
  (format t "
 TilesEnd = *

 TileCount = ~d"
          tile-count))

(defun write-tia-tile-bitmaps-interleaved (merged-tiles)
  (check-type merged-tiles hash-table)
  (format t "~%~|~%Tiles:
 ;; Tile bitmap data is interleaved by scan-line within each
 ;; seven-triple-line grouping.~%")
  (let ((tile-bitmaps (bitmaps-for-tia-merged-tiles merged-tiles)))
    (check-type tile-bitmaps (array t (* 7)))
    (dotimes (scan-line 7)
      (write-tia-bitmaps-scan-line tile-bitmaps scan-line)))
  (write-tia-tiles-trailer (hash-table-count merged-tiles)))

(defconstant +tia-tile-limit+ 128
  "The maximum distinct tile-pairs allowed in one memory bank for the 2600.")

(defvar *merged-tiles*)
(defvar *tile-counter*)

(defun color-average (colors)
  (let ((colors (remove-if #'null colors)))
    (if colors
        (list (round (mean (mapcar #'first colors)))
              (round (mean (mapcar #'second colors)))
              (round (mean (mapcar #'third colors))))
        (list 0 0 0))))

(defun collect-foreground-color/tia (tiles)
  (assert (= 7 (array-dimension *tia-pf-colors* 1)))
  (assert (= (array-dimension *tia-pf-colors* 0)
             (array-dimension *tia-tiles* 0)))
  (assert (every (curry #'> (array-dimension *tia-pf-colors* 0))
                 tiles)
          (tiles) "Tiles referenced (~{~a~^, ~}) which are not known to the colors table"
          (remove-if (curry #'> (array-dimension *tia-pf-colors* 0))
                     tiles))
  (maptimes (line 7)
            (color-average
             (remove-if #'null
                        (mapcar #'palette->rgb
                                (mapcar (lambda (tile)
                                          (aref *tia-pf-colors* tile line))
                                        tiles))))))

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
  (let* ((png (png-read:read-png-file
               (let ((pathname (parse-namestring pathname$)))
                 (make-pathname
                  :name (pathname-name pathname)
                  :type (pathname-type pathname)
                  :defaults #p"./Source/Art/"))))
         (height (png-read:height png))
         (width (png-read:width png))
         (*machine* 7800))
    (png->palette height width
                  (png-read:image-data png))))

(defun extract-4×16-stamps (image)
  (let* ((rows (floor (1- (array-dimension image 1)) 16))
         (columns (floor (array-dimension image 0) 4))
         (output (make-array (list columns rows))))
    (dotimes (row rows)
      (dotimes (column columns)
        (let ((stamp (extract-region image (* column 4) (* row 16)
                                     (+ (* column 4) 3) (+ (* row 16) 15))))
          (assert (= 4 (array-dimension stamp 0)))
          (assert (= 16 (array-dimension stamp 1)))
          (setf (aref output column row) stamp))))
    output))

