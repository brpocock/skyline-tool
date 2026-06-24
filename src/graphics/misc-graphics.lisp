(in-package :skyline-tool)

(defun extract-region (original left top right bottom)
  "Copy a rectangular region from ORIGINAL, inclusive of LEFT and TOP, exclusive of RIGHT and BOTTOM.

   ORIGINAL is  indexed as  (column row).  The returned  array preserves
   that column/row (x,y) layout."
  (let* ((height (- bottom top))
         (width (- right left))
         (element-type (if (arrayp original)
                           (array-element-type original)
                           '(unsigned-byte 8)))
         (copy (make-array (list width height) :element-type element-type)))
    (dotimes (y height copy)
      (dotimes (x width copy)
        (setf (aref copy x y) (aref original (+ left x) (+ top y)))))))

(defun mob->mono-bits (mob)
  (mapcar #'code-char
          (loop for y from 0 to 20
                append
                (loop for col from 0 to 2
                      for col-offset = (* 8 col)
                      collecting
                      (reduce #'logior
                              (loop for x from 0 to 7
                                    for pixel = (aref mob (+ col-offset x) y)
                                    collecting (case pixel
                                                 (#xff 0)
                                                 (otherwise (expt 2 (- 7 x))))))))))

(defun mob->multi-bits (mob)
  (mapcar #'code-char
          (loop for y from 0 to 20
                append
                (loop for col from 0 to 2
                      for col-offset = (* 8 col)
                      collecting
                      (reduce #'logior
                              (loop for x from 0 to 3
                                    for pixel = (aref mob (+ col-offset (* 2 x)) y)
                                    collecting (* (expt 2 (* 2 (- 3 x)))
                                                  (case pixel
                                                    (8 1) (9 2) (#xff 0)
                                                    (otherwise 3)))))))))

(defun tile->color (tile)
  (remove-duplicates
   (remove-if (curry #'= #xff)
              (loop for y from 0 to 7
                    appending
                    (do-collect (x to 7)
                      (aref tile x y))))))

(defun fat-bits (array)
  (destructuring-bind (width height) (array-dimensions array)
    (do-collect (row upto (1- height))
      (do-collect (col upto (1- width))
        (let ((px (aref array col row)))
          (if (= #xff px)
              #\Space
              #\@))))))

(defun image-colors (palette-image
                     &optional
                       (height (array-dimension palette-image 1))
                       (width (array-dimension palette-image 0)))
  "Return the set of distinct colors in use in the paletted image"
  (remove-duplicates
   (remove-if #'null
              (loop for y from 0 to (1- height)
                    appending (loop for x from 0 to (1- width)
                                    collecting (aref palette-image x y))))))

(defun mob-colors (mob)
  (image-colors mob 21 24))

(defun ensure-monochrome (mob)
  (let ((all-colors (mob-colors mob)))
    (unless (= 1 (length all-colors))
      (warn "MOB data is hi-res and not monochrome (using ~D; saw ~{~D~^, ~})"
            (car all-colors) all-colors))
    (code-char (car all-colors))))

(defun ensure-1+chrome (mob)
  (let ((all-colors (remove-if (rcurry #'member '(9 10))
                               (mob-colors mob))))
    (unless (or (null all-colors)
                (= 1 (length all-colors)))
      (warn "MOB data has more than 1 distinct color after brown & orange ~
\(using ~D; saw ~{~D~^, ~})"
            (car all-colors) all-colors))
    (code-char (logior #x80 (or (car all-colors) 0)))))

(defun mob-empty (mob)
  (every (curry #'= #xff)
         (loop for col from 0 upto 23
               append (loop
                        for row from 0 upto 20
                        collect (aref mob col row)))))

(defun mob-hires (mob)
  "Returns T if any two adjacent pixels don't match"
  (not (every #'identity
              (loop for col from 0 upto 11
                    append (loop
                             for row from 0 upto 20
                             collect (= (aref mob (* col 2) row)
                                        (aref mob (1+ (* col 2)) row)))))))

(defun gather-mobs (image-nybbles height width)
  (let (mobs index)
    (loop
      for y-mob from 0 below (/ height 21)
      for y₀ = (* y-mob 21)
      do (loop for x-mob from 0 below (/ width 24)
               for x₀ = (* x-mob 24)
               for mob-data = (extract-region image-nybbles x₀ y₀ (+ x₀ 23) (+ y₀ 20))
               do
                  (cond
                    ((mob-empty mob-data)
                     (format *trace-output*
                             "~% • Found empty MOB (relative ~D,~D)"
                             x-mob y-mob))
                    ((mob-hires mob-data)
                     (appendf mobs (append (mob->mono-bits mob-data)
                                           (cons (ensure-monochrome mob-data) nil)))
                     (appendf index (cons (cons x₀ y₀) nil))
                     (format *trace-output*
                             "~% • Found a hi-res MOB (relative ~D,~D)"
                             x-mob y-mob))
                    (t (appendf mobs (append (mob->multi-bits mob-data)
                                             (cons (ensure-1+chrome mob-data) nil)))
                       (appendf index (cons (cons x₀ y₀) nil))
                       (format *trace-output*
                               "~% • Found a multicolor MOB (relative ~D,~D)"
                               x-mob y-mob)))))
    (values mobs index)))


(defun bit-pairs-to-art (byte)
  (check-type byte (integer 0 #xff))
  (let ((bit-pairs (format nil "~4,4,'0r" byte)))
    (assert (every (lambda (char) (find char "0123")) bit-pairs))
    (substitute
     #\⬜ #\0
     (substitute
      #\🟥 #\1
      (substitute
       #\🟩 #\2
       (substitute
        #\🟦 #\3
        (make-array 4 :element-type 'character
                      :initial-contents bit-pairs)))))))

(defun bytes-and-art (bytes)
  (let* ((binary (mapcar (curry #'format nil "~2,8,'0r") bytes))
         (blocks (mapcar #'bits-to-art binary)))
    (format nil "~%	.byte ~{%~a~^, ~}	 ; ~{~a~^·~}" binary blocks)))

(defun byte-and-art (byte)
  (let* ((binary (format nil "~8,'0b" byte))
         (blocks (bits-to-art binary)))
    (format nil "~%	.byte %~a	; ~a" binary blocks)))

(defun assembler-label-name (string)
  (let ((result (cl-change-case:pascal-case string)))
    (when (search "Brp" result)
      (setf result (cl-ppcre:regex-replace-all "Brp" result "BRP")))
    (when (search "Aa" result)
      (setf result (cl-ppcre:regex-replace-all "Aa" result "AA")))
    (when (search "Zph" result)
      (setf result (cl-ppcre:regex-replace-all "Zph" result "ZPH")))
    result))

(defun pathname-base-name (pathname)
  (subseq (pathname-name pathname)
          0 (position #\. (pathname-name pathname))))

;;; ── Generic Tile Core ───────────────────────────────────────────
;;; Platform-agnostic cell extraction, deduplication, H-distance
;;; reduction, and color-frequency helpers.  Used by Intv, C64, ClcV,
;;; TMS9918, font compilers, and tileset pipelines.

(defun cell-key-from-image (palette-pixels sx sy w h)
  "Extract a W×H rectangular cell key from PALETTE-PIXELS at (SX,SY).

Returns a list of H row-lists, each containing W palette indices.
This is the canonical tile key format used by all dedup/reduce functions."
  (loop for y from sy below (+ sy h)
        collect (loop for x from sx below (+ sx w)
                      collect (aref palette-pixels x y))))

(defun cell-binary-key-from-image (palette-pixels sx sy w h)
  "Extract a color-invariant binary tile key: each row byte has
bit 7–(7−W+1) set for every non-zero palette index in that column.

Returns a list of H row-bytes.  Cells with identical shape but
different colors produce the same key, sharing one character
definition when the platform supports per-instance color selection
(e.g. VIC-II color RAM, TMS9918 color table, C64 color nybble)."
  (loop for y from sy below (+ sy h)
        collect (loop for x from sx below (+ sx w)
                      sum (if (zerop (aref palette-pixels x y))
                              0
                              (ash 1 (- (1- w) (- x sx)))))))

(defun cell-keys-from-image (palette-pixels width height cell-w cell-h)
  "Extract all CELL-W×CELL-H cells from PALETTE-PIXELS (WIDTH×HEIGHT).

Returns two values: a list of cell keys (top-to-bottom, left-to-right)
and a list of (cols rows).  Rows/cols that don't form complete cells are
cropped."
  (let* ((cols (floor width cell-w))
         (rows (floor height cell-h))
         (keys nil))
    (loop for row from 0 below rows
          do (loop for col from 0 below cols
                   for sx = (* col cell-w)
                   for sy = (* row cell-h)
                   do (push (cell-key-from-image palette-pixels sx sy cell-w cell-h) keys)))
    (values (nreverse keys) (list cols rows))))

(defun tile-hamming-distance (key-a key-b)
  "Hamming distance between two tile keys (lists of bytes/rows).
Each row is a byte value; rows are compared position-by-position.
For multi-row keys, the sum of per-row Hamming distances is returned."
  (loop for a in key-a
        for b in key-b
        sum (logcount (logxor a b))))

(defun tile-invert (key)
  "Return KEY with each row byte bitwise-complemented (XOR #xFF).
This is an involution: (tile-invert (tile-invert key)) = key."
  (mapcar (lambda (b) (logxor b #xFF)) key))

(defun color-frequencies (palette-pixels sx sy w h)
  "Return a hash table of palette-index → count for a W×H region at (SX,SY).
Index 0 (transparent/background) is excluded from counting."
  (let ((freq (make-hash-table)))
    (loop for y from sy below (+ sy h)
          do (loop for x from sx below (+ sx w)
                   for c = (aref palette-pixels x y)
                   unless (zerop c)
                     do (incf (gethash c freq 0))))
    freq))

(defun dominant-color (palette-pixels sx sy w h &optional (default 1))
  "Return the most frequent non-zero palette index in a W×H cell.
Returns DEFAULT when the cell is empty."
  (let ((freq (color-frequencies palette-pixels sx sy w h))
        (best default)
        (best-n 0))
    (loop for c being the hash-keys of freq
          for n = (gethash c freq)
          when (> n best-n)
            do (setf best c best-n n))
    best))

(defun deduplicate-cells (cell-keys &key max-unique (ht (make-hash-table :test 'equalp))
                                         (uniq (make-array (or max-unique 256)
                                                           :adjustable t :fill-pointer 0)))
  "Deduplicate CELL-KEYS (list of tile keys) into a unique set.

Returns three values:
  UNIQ      — adjustable vector (fill-pointer = count of unique keys)
  HT        — hash table mapping key → slot index
  SLOT-MAP  — vector where slot-map[i] = index into UNIQ for cell-key i

When MAX-UNIQUE is given and exceeded, extends UNIQ beyond it and warns
(the caller should invoke @code{reduce-tile-set} afterward)."
  (let ((slot-map (make-array (length cell-keys) :element-type '(unsigned-byte 16))))
    (loop for i from 0 below (length cell-keys)
          for key = (nth i cell-keys)
          for idx = (gethash key ht)
          do (unless idx
               (when (and max-unique (not (>= (length uniq) max-unique)))
                 nil)  ;; still have room
               (setf idx (length uniq))
               (setf (gethash key ht) idx)
               (vector-push-extend key uniq))
             (setf (aref slot-map i) idx))
    (when (and max-unique (> (length uniq) max-unique))
      (warn "deduplicate-cells: ~D unique cells exceed limit ~D" (length uniq) max-unique))
    (values uniq ht slot-map)))

(defun reduce-tile-set (uniq slot-map &key max-slots extra-keys
                                           (extra-key-count (length extra-keys))
                                           (hamming #'tile-hamming-distance)
                                           (invert-fn #'tile-invert)
                                           (invert-ok-p t)
                                           (verbose t))
  "Reduce UNIQ (adjustable vector of tile keys) and SLOT-MAP (array
of slot indices) to at most MAX-SLOTS unique entries.

Merges the most similar key pairs progressively by Hamming distance
until the limit is met.  When INVERT-OK-P is true, also considers
the bitwise complement of each key (halving storage for inverted twins).

EXTRA-KEYS is a vector of external candidate keys for substitution
(e.g. GROM cards).  When a slot is replaced by an extra-key, all
its references in SLOT-MAP become extra-key references.

Returns NIL; modifies UNIQ and SLOT-MAP in place."
  (when (<= (length uniq) max-slots)
    (return-from reduce-tile-set nil))
  (when (and extra-keys (zerop extra-key-count))
    (return-from reduce-tile-set nil))
  (let ((n-extra (length extra-keys)))
    (loop while (> (length uniq) max-slots)
          for n-uniq = (length uniq)
          for best-dist = (1+ (* 8 (length (aref uniq 0))))  ;; > max possible
          for best-a = nil  for best-b = nil
          for best-kind = nil  for best-flip = nil
          do
             ;; Pairwise within uniq
             (loop for a from 0 below n-uniq
                   for key-a = (aref uniq a)
                   do (loop for b from (1+ a) below n-uniq
                            for key-b = (aref uniq b)
                            for same-dist = (funcall hamming key-a key-b)
                            do (when (< same-dist best-dist)
                                 (setf best-dist same-dist best-a a best-b b
                                       best-kind :uniq-uniq best-flip nil))
                               (when invert-ok-p
                                 (let ((flip-dist (funcall hamming
                                                           (funcall invert-fn key-a) key-b)))
                                   (when (< flip-dist best-dist)
                                     (setf best-dist flip-dist best-a a best-b b
                                           best-kind :uniq-uniq best-flip t))))))
             ;; uniq vs extra-keys
             (when (plusp n-extra)
               (loop for a from 0 below n-uniq
                     for key-a = (aref uniq a)
                     do (loop for e from 0 below n-extra
                              for e-key = (aref extra-keys e)
                              for same-dist = (funcall hamming key-a e-key)
                              do (when (< same-dist best-dist)
                                   (setf best-dist same-dist best-a a best-b e
                                         best-kind :uniq-extra best-flip nil))
                                 (when invert-ok-p
                                   (let ((flip-dist
                                           (funcall hamming key-a
                                                    (funcall invert-fn e-key))))
                                     (when (< flip-dist best-dist)
                                       (setf best-dist flip-dist best-a a best-b e
                                             best-kind :uniq-extra best-flip t)))))))
             ;; Apply merge
             (ecase best-kind
               (:uniq-uniq
                ;; Merge slot best-b into best-a
                (when verbose
                  (warn "reduce-tile-set: merging slot ~D into ~D (dist=~D)~@[ flip~]"
                        best-b best-a best-dist best-flip))
                (loop for i from 0 below (length slot-map)
                      for s = (aref slot-map i)
                      do (cond
                           ((= s best-b)
                            (setf (aref slot-map i) best-a))
                           ((> s best-b)
                            (decf (aref slot-map i)))))
                ;; Remove best-b from uniq
                (loop for i from best-b below (1- n-uniq)
                      do (setf (aref uniq i) (aref uniq (1+ i))))
                (vector-pop uniq))
               (:uniq-extra
                ;; Replace slot best-a with extra-key best-b
                (when verbose
                  (warn "reduce-tile-set: replacing slot ~D with extra-key ~D (dist=~D)~@[ flip~]"
                        best-a best-b best-dist best-flip))
                ;; Mark as extra-key reference (negative index = extra reference)
                (loop for i from 0 below (length slot-map)
                      for s = (aref slot-map i)
                      do (when (= s best-a)
                           (setf (aref slot-map i) (- -1 best-b))))  ;; negative = extra
                ;; Remove dead slot from uniq
                (loop for i from best-a below (1- n-uniq)
                      do (setf (aref uniq i) (aref uniq (1+ i))))
                (vector-pop uniq))))
    nil)
  
(defun compile-tileset-64 (png-file out-dir height width image-nybbles)
    "Write VIC-II tileset to OUT-DIR: char data (2048 bytes) + color RAM (256 bytes).

Each 16×16 tile is four 8×8 character cells (TL, TR, BL, BR).  Cells with
0–1 distinct non-background colors emit monochrome bitmaps; cells with 2+
distinct colors emit VIC-II multicolor 2-bit-pair data.  Color RAM is always
emitted.

Output file: @file{tiles.@emph{name}.s}

@table @asis
@item PNG-FILE
Source PNG path (for comments only).
@item OUT-DIR
Directory for the @file{.s} output.
@item HEIGHT, WIDTH
Pixel dimensions of the PNG.
@item IMAGE-NYBBLES
2D palette-pixel array from @code{png->palette}.
@end table"
    (let ((out-file (merge-pathnames
                     (make-pathname :name (concatenate 'string "tiles."
                                                       (pathname-name png-file))
                                    :type "s")
                     out-dir))
          (cell-count 256)
          (multi-cells 0))
      (ensure-directories-exist (directory-namestring out-file))
      (with-output-to-file (src-file out-file :if-exists :supersede)
        (format src-file ";;; -*- asm -*-~%")
        (format src-file ";;; VIC-II tileset compiled from ~A~%" png-file)
        (format src-file ";;; ~D×~D px → ~D char cells, 4 per 16×16 tile~2%"
                width height cell-count)
        ;; Char data: 8 bytes per cell
        (format src-file "~ATilesetChars:  ;; ~D cells × 8 bytes~%"
                (pathname-name png-file) cell-count)
        (loop for cell from 0 below cell-count
              for x-cell = (tile-cell-vic2-x cell width)
              for y-cell = (tile-cell-vic2-y cell width)
              for tile-data = (extract-region image-nybbles x-cell y-cell
                                              (+ 7 x-cell) (+ 7 y-cell))
              for colors = (tile->color tile-data)
              for n-colors = (length colors)
              for multi-p = (> n-colors 1)
              do (when multi-p (incf multi-cells))
                 (if multi-p
                     ;; Multicolor: 4 big pixels per row, 2-bit pairs
                     (let ((cmap (vic2-cell-multicolor-map tile-data colors)))
                       (loop for y from 0 below 8
                             for byte = 0
                             do (loop for x-pair from 0 below 4
                                      for b0 = (aref tile-data (* x-pair 2) y)
                                      for b1 = (aref tile-data (1+ (* x-pair 2)) y)
                                      for pair = (cond
                                                   ((and (zerop b0) (zerop b1)) 0)
                                                   ((and (= b0 b1) (position b0 cmap))
                                                    (position b0 cmap))
                                                   (t (or (position b0 cmap)
                                                          (position b1 cmap)
                                                          0)))
                                      do (setf byte (logior (ash byte 2) pair)))
                                (format src-file "    .byte $~2,'0X~%" byte)))
                     ;; Monochrome: 1 bit per pixel, 8 bytes per cell
                     (loop for y from 0 below 8
                           for byte = (loop for x from 0 below 8
                                            sum (if (zerop (aref tile-data x y))
                                                    0
                                                    (ash 1 (- 7 x))))
                           do (format src-file "    .byte $~2,'0X~%" byte)))
                 ;; Color RAM: 1 byte per cell
                 (format src-file "~2%~ATilesetColorRAM:  ;; ~D cells~%"
                         (pathname-name png-file) cell-count)
                 (loop for cell from 0 below cell-count
                       for x-cell = (tile-cell-vic2-x cell width)
                       for y-cell = (tile-cell-vic2-y cell width)
                       for tile-data = (extract-region image-nybbles x-cell y-cell
                                                       (+ 7 x-cell) (+ 7 y-cell))
                       for colors = (tile->color tile-data)
                       for n-colors = (length colors)
                       for multi-p = (> n-colors 1)
                       for cram = (if multi-p
                                      (let ((cmap (vic2-cell-multicolor-map tile-data colors)))
                                        (logior (ash (or (nth 1 cmap) 0) 4)
                                                (logand (or (nth 2 cmap) 0) #x0F)))
                                      (or (first colors) 0))
                       do (format src-file "    .byte $~2,'0X~%" cram))
                 ;; Constants
                 (format src-file "~2%~ATilesetMultiCells   EQU ~D~%"
                         (pathname-name png-file) multi-cells))
        (format *error-output* "~&Wrote VIC-II tileset (~D cells, ~D multicolor) to ~A."
                cell-count multi-cells out-file)))))

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

(defun color-average (colors)
  (let ((colors (remove-if #'null colors)))
    (if colors
        (list (round (mean (mapcar #'first colors)))
              (round (mean (mapcar #'second colors)))
              (round (mean (mapcar #'third colors))))
        (list 0 0 0))))

(defun compile-art (index-out &rest png-files)
  "Compiles PNG image files into binary graphics data for INDEX-OUT.

@cindex graphics compilation
@cindex PNG processing
@cindex sprite compilation

@table @code
@item Package: skyline-tool
@item Arguments: index-out (pathname designator), &rest png-files (pathname designators)
@item Returns: nil
@item Side Effects: Updates graphics index file and generates binary graphics data
@end table

This function processes PNG image files, converting them into the binary format required by the MARIA graphics processor. The compilation process includes:

@itemize
@item PNG image loading and validation
@item Color palette extraction and optimization
@item Graphics data compression and formatting
@item Index file updates for asset management
@item Platform-specific optimizations (7800, 5200, etc.)
@end itemize

Multiple PNG files can be processed in a single call, with all output directed to the specified index file.

@strong{Supported Formats:}
PNG images with indexed color or RGB color modes.

@strong{Output:}
Binary graphics data and updated asset index for game engine loading.

@strong{Example:}
@example
(compile-art #p\"Object/Assets/Sprites.index\"
             #p\"Source/Art/Player.png\"
             #p\"Source/Art/Enemies.png\")
@end example"
  (let ((*machine* (or (when (every #'digit-char-p (first png-files))
                         (prog1
                             (parse-integer (first png-files))
                           (setf png-files (rest png-files))))
                       5200)))
    (dolist (file png-files)
      (dispatch-png file index-out))))

(defun def->tile-id (tile-definition x y)
  (destructuring-bind (tag x₀ y₀ x₁ y₁) tile-definition
    (declare (ignore tag x₁ y₁))
    (let ((set-width (reduce #'max (mapcar #'fourth *tileset*))))
      (+ x₀ x (* set-width (+ y₀ y))))))

(defun limit-region-to-palette (region palette &key (allow-imperfect-p t))
  (let ((output (make-array (array-dimensions region))))
    (destructuring-bind (width height) (array-dimensions region)
      (dotimes (x width output)
        (dotimes (y height output)
          (setf (aref output x y)
                (if allow-imperfect-p
                    (pixel-into-palette (aref region x y) (coerce palette 'list)
                                        :best-fit-p t)
                    (or (position (aref region x y) palette)
                        (error 'color-not-in-palette-error
                               :pixel (aref region x y)
                               :x x :y y :i nil :image nil
                               :palette palette
                               :image-pixels region)))))))))

(defun list-chomp (n list)
  (if (< (length list) n)
      (append list (loop repeat (- n (length list)) collect 0))
      (subseq list 0 n)))

(defun map-region-to-palette (region palette &key allow-imperfect-p)
  (let ((output (make-array (array-dimensions region) :element-type '(unsigned-byte 8))))
    (dotimes (x (array-dimension region 0) output)
      (dotimes (y (array-dimension region 1) output)
        (setf (aref output x y) (palette-reference (aref region x y) palette
                                                   :allow-imperfect-p allow-imperfect-p))))))

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

(defun print-ansi-pixel (color stream)
  (format stream (apply #'ansi-color-pixel
                        (etypecase color
                          (integer (elt (machine-palette *machine*) color))
                          (cons color)))))

(defun print-clim-pixel (color stream &key shortp (unit #x10))
  (setf unit (or unit #x10))
  (clim:with-output-as-presentation (stream color 'palette-color)
    (clim:with-room-for-graphics (stream)
      (setf (clim:medium-ink stream) (apply #'clim:make-rgb-color
                                            (mapcar (lambda (c) (/ c 255.0))
                                                    (elt (machine-palette 7800) color))))
      (clim:draw-rectangle* stream 0 0
                            (* (if shortp 1 3/2) unit 2)
                            (* (if shortp 1 3/2) unit) :filled t)
      (setf (clim:medium-ink stream) clim:+foreground-ink+))))

(defun print-wide-pixel (color stream &key shortp unit with-index-p)
  (cond
    #+mcclim
    ((typep stream 'clim:sheet)
     (print-clim-pixel color stream :shortp shortp :unit unit))
    ((and (not (typep stream 'string-stream))
          (tty-xterm-p))
     (when with-index-p
       (format stream "$~2,'0x " color))
     (print-ansi-pixel color stream))
    (t (if (consp color)
           (format stream " #~{~2,'0x~2,'0x~2,'0x~} " color)
           (format stream "[~2,'0x]" color)))))

(defun print-clim-color (color stream)
  (clim:with-output-as-presentation (stream color 'palette-color)
    (clim:with-room-for-graphics (stream :height 24)
      (print-wide-pixel color stream :shortp t)
      (format stream " Color $~2,'0x = ~a #~2,'0x~2,'0x~2,'0x"
              color (atari-colu-string color)
              (elt (elt (machine-palette 7800) color) 0)
              (elt (elt (machine-palette 7800) color) 1)
              (elt (elt (machine-palette 7800) color) 2)))))

(defun print-machine-palette (stream)
  (format stream "~2&Machine palette:")
  (dotimes (i #x100)
    (when (zerop (mod i #x10))
      (terpri stream))
    (print-wide-pixel i stream :unit 8))
  (force-output stream))

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
(defun tile-art-value (tile-info)
  (let ((tile (or (getf tile-info :art)
                  (if (getf tile-info :wall) "WALL" "FLOOR"))))
    (let ((candidates (remove-if-not (lambda (def)
                                       (equalp (string (car def)) tile))
                                     *tileset*)))
      (unless candidates
        (error "Undefined tile art: ~A~%Wanted one of: ~S"
               tile
               (sort (mapcar #'string (remove-duplicates (mapcar #'car *tileset*)
                                                         :test #'equalp))
                     #'string<)))
      (let ((candidates (loop for each on
                                       (remove-if-not (lambda (def)
                                                        (destructuring-bind (tag x₀ y₀ x₁ y₁) def
                                                          (declare (ignore tag))
                                                          (and (= x₀ x₁) (= y₀ y₁))))
                                                      (reverse *tileset*))
                              by #'cdr appending each)))
        (let ((chosen (nth (random (length candidates)) candidates)))
          (def->tile-id chosen 0 0))))))

(defun tile->bits (tile)
  (do-collect (y to 7)
    (reduce #'logior
            (loop for x from 0 to 7
                  collecting (if (zerop (aref tile x y))
                                 0
                                 (expt 2 (- 7 x)))))))

(defun tile-control-value (tile)
  (logand (if (getf tile :wall) #x80 0)
          (if (getf tile :swim) #x40 0)))

(defun tile-hash (left right big-endian-p)
  (logior (ash left 8) (ash right 16) (if big-endian-p 1 0)))

