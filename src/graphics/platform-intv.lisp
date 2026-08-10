(in-package :skyline-tool)


(defun intv-tile-row-bytes (palette-pixels sx sy)
  "Return 8-byte row bitmaps for one 8×8 tile at (SX,SY); white = palette index 7."
  (loop for y from 0 below 8
        collect (let ((byte 0))
                  (loop for x from 0 below 8
                        for palette-index = (aref palette-pixels (+ sx x) (+ sy y))
                        do (when (= palette-index 7)
                             (setf byte (logior byte (ash 1 (- 7 x))))))
                  byte)))

(defun intv-invert-tile-row-bytes (key)
  "Intv alias for @code{tile-invert}.  Returns KEY with each row byte XORed #xFF."
  (tile-invert key))

(defvar *intv-grom-bytes-cache* nil
  "Cached 2048-byte GROM image (256×8 bytes) or NIL if unavailable.")

(defun intv-grom-bin-path ()
  "Return pathname to the Intellivision GROM binary, or NIL if not found.

Checks @file{Tools/Intv/grom.bin} (project-level) first, then falls back
to the jzIntv emscripten @file{minigrom.bin}.

The file is 2048 bytes: 256 GROM cards × 8 row bytes each, same layout as
@code{intv-tile-row-bytes} for monochrome art."
  (flet ((try (rel)
           (let ((p (merge-pathnames rel (asdf:system-source-directory :skyline-tool))))
             (when (probe-file p) p))))
    (or (try #p"../Tools/Intv/grom.bin")
        (try #p"../Tools/jzIntv/src/emscripten/minigrom.bin"))))

(defun intv-grom-bytes ()
  "Load and cache Intellivision GROM bytes (2048), or NIL if file missing.

@itemize @bullet
@item Side Effects: Sets @code{*intv-grom-bytes-cache*} on first load (including to NIL).
@end itemize"
  (or *intv-grom-bytes-cache*
      (setf *intv-grom-bytes-cache*
            (let ((p (intv-grom-bin-path)))
              (cond
                ((not p)
                 (warn "Intellivision GROM (../Tools/jzIntv/src/emscripten/minigrom.bin) not found; blob tiles will not match default GROM cards")
                 nil)
                (t
                 (with-open-file (stream p :element-type '(unsigned-byte 8))
                   (let ((buf (make-array 2048 :element-type '(unsigned-byte 8))))
                     (assert (= 2048 (read-sequence buf stream)) (p)
                             "Expected 2048-byte GROM at ~A" p)
                     buf))))))))

(defun intv-grom-key-to-card-map (grom-bytes)
  "Build @code{equal} hash: tile row-byte list → GROM card index (0–255).

If two GROM cards share the same bitmap, the lower card index wins."
  (let ((ht (make-hash-table :test 'equal)))
    (loop for card from 0 below 256
          for base = (* card 8)
          for key = (loop for row from 0 below 8
                          collect (aref grom-bytes (+ base row)))
          do (unless (gethash key ht)
               (setf (gethash key ht) card)))
    ht))

(defun intv-grom-lookup (key grom-map)
  "Look up KEY in GROM-MAP, also trying the bitwise-inverted tile.

Returns @code{(values card-index invert-p found-p)}.  When the inverted
tile matches, @code{invert-p} is @code{T} and @code{card-index} is the
original (non-inverted) GROM card.  When neither matches,
@code{found-p} is @code{NIL}.

@table @asis
@item KEY
8-byte row-bitmap list from @code{intv-tile-row-bytes}.
@item GROM-MAP
@code{equal} hash from @code{intv-grom-key-to-card-map}.
@end table"
  (multiple-value-bind (card found) (gethash key grom-map)
    (if found
        (values card nil t)
        (let ((inv-key (intv-invert-tile-row-bytes key)))
          (multiple-value-bind (inv-card inv-found) (gethash inv-key grom-map)
            (if inv-found
                (values inv-card t t)
                (values nil nil nil)))))))

(defun compile-blob-intv-screen (png-file output-path palette-pixels width height)
  "Write  OUTPUT-PATH assembly:  deduplicated  GRAM 8×8  cards +  row-major
tile map.

Each cell  in the WIDTH×HEIGHT  image is  one 8×8 tile.  Trailing pixels
that do  not form  a complete  8×8 tile  are cropped  from the  right or
bottom edge. Each cell resolves  GROM-first: tiles that match a built-in
GROM   card   (from   bundled   @file{minigrom.bin})   use   that   card
index (@code{$0000}–@code{$00FF})  and consume  no GRAM slot  (no bitmap
upload   at   runtime).   Other   identical   tiles   share   one   GRAM
definition  (@code{$0100}–@code{$013F}).  At  most  64  unique  non-GROM
tiles (Intv GRAM); @code{*_GRAM_DATA} is emitted only for those slots.

Intv  @emph{map} tilesets  should  reuse this  GROM/GRAM  dedup per  8×8
quadrant; each logical  16×16 map tile is four such  cells (2×2: TL, TR,
BL,  BR),  each   storing  its  own  card  ID.   Tileset  records  carry
per-quadrant color-stack values with pattern  refs — no separate palette
assets  (contrast   7800  tileset   palette  blobs).  Map   export  uses
@code{compile-map-intv-screen} for STIC color-stack tiles.

@code{*_TILE_MAP} entries: @code{$0000}–@code{$00FF} = GROM card number;
@code{$0100}–@code{$013F} = GRAM slot 0–63 (see @code{*_TILE_MAP_GRAM_BASE}).

@table @asis
@item PNG-FILE
Source PNG path (for comments only)
@item OUTPUT-PATH
Destination @file{.s} file
@item PALETTE-PIXELS
2D array from @code{png->palette}
@item WIDTH @itemx HEIGHT
Pixel dimensions; partial trailing tile edges are cropped down to multiples of
8.
@end table"
  (check-type output-path (or pathname string))
  (let* ((original-width width)
         (original-height height))
    (setf width (- width (mod width 8)))
    (setf height (- height (mod height 8)))
    (when (or (< width 8) (< height 8))
      (error "Intellivision blob ~A dimensions must include at least one complete 8×8 tile, got ~D×~D"
             png-file original-width original-height))
    (when (or (/= width original-width) (/= height original-height))
      (warn "Intellivision blob ~A cropped from ~D×~D to ~D×~D to fit 8×8 tiles"
            png-file original-width original-height width height)))
  (let* ((cols (/ width 8))
         (rows (/ height 8))
         (total-cells (* cols rows))
         (uniq (make-array 64 :adjustable t :fill-pointer 0))
         (nuniq (length uniq))
         (ht (make-hash-table :test 'equal))
         (tile-ids (make-array total-cells :element-type '(unsigned-byte 16)))
         (grom-bytes (intv-grom-bytes))
         (grom-map (intv-grom-key-to-card-map grom-bytes))
         (grom-cells 0)
         (idx 0))
    (dotimes (row rows)
      (dotimes (col cols)
        (let* ((key (intv-tile-row-bytes palette-pixels (* col 8) (* row 8))))
          (multiple-value-bind (grom-id invert found)
              (intv-grom-lookup key grom-map)
            (if found
                (progn
                  (setf (aref tile-ids idx) (logior grom-id (if invert #x8000 0)))
                  (incf grom-cells))
                (multiple-value-bind (gram-id invert)
                    (intv-gram-lookup-or-allocate key uniq ht)
                  (setf (aref tile-ids idx)
                        (logior (+ #x100 gram-id) (if invert #x8000 0))))))
          (incf idx))))
    (when (> nuniq 56)
      (error "Intellivision blob ~A has ~D unique GRAM cards (max 56 after reduction)"
             png-file nuniq))
    (when (> nuniq 56)
      (warn "Intellivision blob ~A reducing from ~D to 56 GRAM cards by Hamming-distance merging..."
            png-file nuniq)
      (intv-reduce-gram-set uniq tile-ids :max-slots 56 :grom-bytes grom-bytes)
      (setf nuniq (length uniq))
      (setf grom-cells (loop for packed across tile-ids
                             count (< (logand packed #x7FFF) #x100))))
    (when (> nuniq 56)
      (warn "Intellivision blob ~A: GRAM reduction could not bring card count below 56 (~D unique remain); MOB GRAM is compromised"
            png-file nuniq))
    (when (>= nuniq 56)
      (warn "Intellivision blob ~A uses ~D unique GRAM cards, overlapping the MOB reservation (slots 56–63)"
            png-file nuniq)))
  (ensure-directories-exist (merge-pathnames output-path))
  (with-output-to-file (src (merge-pathnames output-path) :if-exists :supersede
                                                          :external-format :utf-8)
    (format src ";;; Intellivision blob: tile-mapped screen + GRAM cards~%")
    (format src ";;; Source: ~A~%" png-file)
    (format src ";;; Grid: ~D×~D tiles (~D×~D px); ~D tile~:P use GROM; ~D unique GRAM card~:P~2%"
            cols rows width height grom-cells nuniq)
    (format src ";;; TILE_MAP: $0000-$00FF = GROM card# ; $0100-$013F = GRAM slot + *_TILE_MAP_GRAM_BASE~%")
    (format src "~A_TILE_COLS EQU ~D~%" lab cols)
    (format src "~A_TILE_ROWS EQU ~D~%" lab rows)
    (format src "~A_TILE_MAP_GRAM_BASE EQU $0100~%" lab)
    (format src "~A_UNIQUE_GRAM_CARDS EQU ~D~2%" lab nuniq)
    ;; Reserve the top 8 GRAM slots (56–63) for MOB sprites; map tiles use 0–55.
    (format src "~A_GRAM_MAP_SLOTS_MAX EQU 56~%" lab)
    (format src "~A_GRAM_MOB_SLOT_BASE EQU 56~2%" lab)
    (format src "~A_GRAM_DATA:~%" lab)
    (loop for u from 0 below nuniq
          for card = (aref uniq u)
          do (progn
               (format src "    ;; GRAM slot ~D~%" u)
               (let ((bytes-list (reverse card)))
                 (loop for i from 0 below 4
                       for byte-first = (nth (* i 2) bytes-list)
                       for byte-second = (nth (+ (* i 2) 1) bytes-list)
                       for word = (logior (ash byte-first 8) byte-second)
                       for low10 = (logand word #x3FF)
                       for high10 = (logand (ash word -10) #x3FF)
                       do (format src "    DECLE   $~3,'0X~%" low10)
                          (format src "    DECLE   $~3,'0X~%" high10))))
             (format src "~A_TILE_CSTK:~%" lab)
             (dotimes (i total-cells)
               (let* ((raw-id (aref tile-ids i))
                      (invert (logtest raw-id #x8000))
                      (card-id (logand raw-id #x7FFF))
                      (color (intv-dominant-stic-color
                              palette-pixels
                              (* (mod i cols) 8)
                              (* (floor i cols) 8)))
                      (cstk (intv-cstk-word card-id color invert)))
                 (format src "    DECLE   $~4,'0X~%" cstk)))
             (format src "~A_TILE_MAP:~%" lab)
             (dotimes (i total-cells)
               (format src "    DECLE   $~4,'0X~%" (logand (aref tile-ids i) #x7FFF)))
             (format *trace-output* "~&Wrote Intellivision blob (~D GROM cells, ~D unique GRAM tiles) to ~A."
                     grom-cells nuniq (enough-namestring output-path)))))

(defun compile-blob-intv (png-file output-file)
  "Compile BLOB PNG-FILE to OUTPUT-FILE assembly (tile map + GRAM card data).

Blobs are tile-mapped screens: the image is a grid of 8×8 cells. Each cell
stores a GROM (@code{$0000}–@code{$00FF}) or GRAM (@code{$0100}+) card ID;
@code{*_GRAM_DATA} holds bitmaps only for non-GROM patterns (see
@code{compile-blob-intv-screen})."
  (check-type png-file (or pathname string))
  (check-type output-file (or pathname string))
  (let* ((png (png-read:read-png-file png-file))
         (height (png-read:height png))
         (width (png-read:width png))
         (α (png-read:transparency png))
         (palette-pixels (png->palette (png-read:image-data png)
                                       α)))
    (compile-blob-intv-screen png-file output-file palette-pixels width height)))

(defun intv-dominant-stic-color (palette-pixels sx sy)
  "Return dominant STIC color index (0–15) for 8×8 cell at (SX,SY).

Ignores palette index 0 (background). Defaults to @code{7} (white) when empty."
  (let ((counts (make-array 16 :element-type '(unsigned-byte 16) :initial-element 0)))
    (loop for y from 0 below 8
          do (loop for x from 0 below 8
                   for idx = (aref palette-pixels (+ sx x) (+ sy y))
                   when (plusp idx)
                     do (incf (aref counts idx))))
    (let ((best 7)
          (best-count 0))
      (dotimes (c 16)
        (when (> (aref counts c) best-count)
          (setf best c)
          (setf best-count (aref counts c))))
      best)))

;;; ── Generic Tile Core ───────────────────────────────────────────
(defun assemble-intv-rom (source-files output-file)
  "Assemble Intellivision ROM from SOURCE-FILES to OUTPUT-FILE using as1600.

SOURCE-FILES is a list of assembly source paths.  The first file is the
main entry given to as1600 (it may INCLUDE the rest).  Writes the
binary to OUTPUT-FILE and a companion @file{.cfg} sidecar beside it.

as1600 is located at @file{bin/as1600} or @file{Tools/jzIntv/bin/as1600}
relative to the project root.

@table @asis
@item Side Effects
Invokes @command{as1600}.  Requires the jzIntv SDK to be built (@command{make
-f Source/Build/Intv.mak bin/as1600}).
@end table"
  (check-type source-files list)
  (check-type output-file (or pathname string))
  (let* ((main-source (first source-files))
         (as1600 #p"bin/as1600"))
    (unless (probe-file as1600)
      (error "as1600 not found at bin/as1600; build jzIntv SDK first"))
    (format *trace-output* "~&Assembling Intellivision ROM: ~A -o ~A…~%" main-source output-file)
    (ensure-directories-exist output-file)
    (uiop:run-program (list (namestring as1600)
                            (namestring (merge-pathnames main-source))
                            "-o" (enough-namestring (merge-pathnames output-file))
                            "-l" (enough-namestring (make-pathname :type "lst" :defaults output-file)))
                      :output *trace-output*
                      :error-output *trace-output*)
    output-file))

(defun compile-art-intv (index-out index-in)
  "Compile Intellivision art assets from index file"
  (let ((*machine* 2609)
        (art-index (read-intv-art-index index-in)))
    (format *trace-output* "~&Compiling Intellivision art from ~A to ~A…" index-in index-out)
    (with-output-to-file (out index-out :if-exists :supersede :if-does-not-exist :create)
      (format out ";;; Intellivision Art Assets compiled from ~A~%;;; Generated automatically~2%" index-in)
      (dolist (art-item art-index)
        (destructuring-bind (png-file width height) art-item
          (format *trace-output* "~&Processing art asset: ~A (~Dx~D)…" png-file width height)
          ;; For Intellivision, art assets are typically converted to GRAM cards
          ;; Use the existing GRAM compilation function
          (let ((gram-file (merge-pathnames
                            (make-pathname :name (pathname-name png-file) :type "s")
                            (directory-namestring index-out))))
            (compile-gram-intv png-file (directory-namestring gram-file)
                               :width width :height height))))
      (format out "~%;;; End of Intellivision art assets~%"))
    (format *trace-output* "Intellivision art compilation complete.")))

(defun compile-gram-intv (png-file out-dir &key height width palette-pixels)
  "Compile GRAM cards from PNG image PNG-FILE to OUT-DIR.

@table @code
@item Package: skyline-tool
@item Arguments: png-file (pathname designator), out-dir (pathname designator), &key height (integer), width (integer), palette-pixels (array)
@item Returns: nil
@item Side Effects: Writes compiled GRAM card data to assembly file
@end table

Compiles PNG image into GRAM card data for Intellivision.
Outputs assembly file with DECLE statements for GRAM card data.
Each 8×8 pixel GRAM card is stored as 4 16-bit words (packing 2 bytes per word).
Image must be monochrome (black=0, white=1 pixel values).
All cards in the source image are output as one file."
  (check-type png-file (or pathname string))
  (check-type out-dir (or pathname string))
  (let* ((palette-pixels (or palette-pixels
                             (let* ((png (png-read:read-png-file png-file))
                                    (png-height (png-read:height png))
                                    (png-width (png-read:width png))
                                    (α (png-read:transparency png)))
                               (png->palette (png-read:image-data png)
                                             α))))
         (array-width (array-dimension palette-pixels 0))
         (array-height (array-dimension palette-pixels 1))
         (width (floor (or width array-width)))
         (height (floor (or height array-height))))
    ;; Validate dimensions: ensure at least one 8×8 card
    (assert (>= width 8) (width) "Width must be at least 8 (for at least one card), got ~D" width)
    (assert (>= height 8) (height) "Height must be at least 8 (for at least one card), got ~D" height)
    ;; Validate dimensions are within array bounds
    (assert (<= width array-width)
            (width palette-pixels)
            "Width ~D exceeds array width ~D"
            width array-width)
    (assert (<= height array-height)
            (height palette-pixels)
            "Height ~D exceeds array height ~D"
            height array-height)
    ;; Check if monochrome (only black=0 and white=7 palette indices)
    (let ((colors (image-colors palette-pixels height width)))
      (unless (subsetp colors '(0 7) :test '=)
        (warn "GRAM image ~A is not monochrome (found palette indices: ~{~D~^, ~}); treating non-black/non-white pixels as black"
              png-file colors))
      (let ((out-file (merge-pathnames
                       (make-pathname :name
                                      (pathname-name png-file)
                                      :type "s")
                       out-dir))
            (cards-across (floor (/ width 8)))
            (cards-down (floor (/ height 8))))
        (ensure-directories-exist (directory-namestring out-file))
        (with-output-to-file (src-file out-file :if-exists :supersede)
          (format src-file ";;; GRAM cards compiled from ~A~%;;; Generated for Intellivision~%;;; Each card: 8×8 pixels = 8 bytes = 4 16-bit DECLE values~%~%"
                  png-file)
          ;; Process each 8×8 card
          (loop for card-y from 0 below cards-down
                do (loop for card-x from 0 below cards-across
                         for card-index = (+ (* card-y cards-across) card-x)
                         do (let ((start-x (* card-x 8))
                                  (start-y (* card-y 8))
                                  (card-bytes '()))
                              ;; Extract 8 bytes (one per row)
                              (loop for y from 0 below 8
                                    for byte = 0
                                    do (loop for x from 0 below 8
                                             for palette-index = (aref palette-pixels (+ start-x x) (+ start-y y))
                                             ;; White (palette index 7) = bit 1, black (0) or other = bit 0
                                             do (when (= palette-index 7)
                                                  (setf byte (logior byte (ash 1 (- 7 x))))))
                                       (push byte card-bytes))
                              ;; Pack bytes into 16-bit words (2 bytes per DECLE, 4 DECLE per card)
                              ;; Big-endian: most significant byte first
                              (let ((bytes-list (reverse card-bytes)))
                                (loop for i from 0 below 4
                                      for byte-first = (nth (* i 2) bytes-list)  ; First byte (high byte)
                                      for byte-second = (nth (+ (* i 2) 1) bytes-list)  ; Second byte (low byte)
                                      for word = (logior (ash byte-first 8) byte-second)
                                      do (format src-file "    DECLE   $~4,'0X~%" word))))))
          (format *trace-output* "~% Wrote GRAM card data to ~A." out-file))))))

(defun compile-intv-sprite (png-file output-dir &key height width palette-pixels)
  "Compile Intellivision sprite (MOB data)

In Intellivision terminology, sprites are called MOBs (Moving Object Blocks).
This function compiles sprite graphics into MOB data format, similar to GRAM
compilation but for sprites that can be positioned anywhere on screen."
  (check-type png-file (or pathname string))
  (check-type output-dir (or pathname string))
  (let* ((palette-pixels (or palette-pixels
                             (let* ((png (png-read:read-png-file png-file))
                                    (png-height (png-read:height png))
                                    (png-width (png-read:width png))
                                    (α (png-read:transparency png)))
                               (png->palette (png-read:image-data png)
                                             α))))
         (array-width (array-dimension palette-pixels 0))
         (array-height (array-dimension palette-pixels 1))
         (width (floor (or width array-width)))
         (height (floor (or height array-height))))
    ;; Validate dimensions: ensure at least one 8×8 sprite
    (assert (>= width 8) (width) "Width must be at least 8 (for at least one sprite), got ~D" width)
    (assert (>= height 8) (height) "Height must be at least 8 (for at least one sprite), got ~D" height)
    ;; Validate dimensions are within array bounds
    (assert (<= width array-width)
            (width palette-pixels)
            "Width ~D exceeds array width ~D"
            width array-width)
    (assert (<= height array-height)
            (height palette-pixels)
            "Height ~D exceeds array height ~D"
            height array-height)
    ;; Check if monochrome (only black=0 and white=7 palette indices)
    (let ((colors (image-colors palette-pixels height width)))
      (unless (subsetp colors '(0 7) :test '=)
        (warn "Sprite image ~A is not monochrome (found palette indices: ~{~D~^, ~}); ~
treating non-black/non-white pixels as black"
              png-file colors))
      (let ((out-file (merge-pathnames
                       (make-pathname :name
                                      (pathname-name png-file)
                                      :type "s")
                       output-dir))
            (sprites-across (floor (/ width 8)))
            (sprites-down (floor (/ height 8))))
        (ensure-directories-exist (directory-namestring out-file))
        (with-output-to-file (src-file out-file :if-exists :supersede)
          (format src-file ";;; MOB sprites compiled from ~A
;;; Generated for Intellivision
;;; Each sprite: 8×8 pixels = 8 bytes = 4 16-bit DECLE values~%~%"
                  png-file)
          ;; Process each 8×8 sprite
          (loop for sprite-y from 0 below sprites-down
                do (loop for sprite-x from 0 below sprites-across
                         for sprite-index = (+ (* sprite-y sprites-across) sprite-x)
                         do (let ((start-x (* sprite-x 8))
                                  (start-y (* sprite-y 8))
                                  (sprite-bytes '()))
                              ;; Extract 8 bytes (one per row)
                              (loop for y from 0 below 8
                                    for byte = 0
                                    do (loop for x from 0 below 8
                                             for palette-index = (aref palette-pixels (+ start-x x) (+ start-y y))
                                             ;; White (palette index 7) = bit 1, black (0) or other = bit 0
                                             do (when (= palette-index 7)
                                                  (setf byte (logior byte (ash 1 (- 7 x))))))
                                       (push byte sprite-bytes))
                              ;; Pack bytes into 16-bit words (2 bytes per DECLE, 4 DECLE per sprite)
                              ;; Big-endian: most significant byte first
                              (let ((bytes-list (reverse sprite-bytes)))
                                (loop for i from 0 below 4
                                      for byte-first = (nth (* i 2) bytes-list)  ; First byte (high byte)
                                      for byte-second = (nth (+ (* i 2) 1) bytes-list)  ; Second byte (low byte)
                                      for word = (logior (ash byte-first 8) byte-second)
                                      do (format src-file "    DECLE   $~4,'0X~%" word)))))))
        (format *trace-output* "~% Wrote MOB sprite data to ~A." out-file)))))

