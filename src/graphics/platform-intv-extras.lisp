(in-package :skyline-tool)

(defun compile-map-intv-screen (map-name output-path width height tile-grid tileset-records
                                &key spawn-table stic-override-grid stic-override-table)
  "Write OUTPUT-PATH assembly for an Intellivision map: 4-word header, quadrant
CSTK DECLEs, STIC override table/indices, and spawn records.

TILESET-RECORDS is a vector of BACKTAB color-stack words (four per global tile
id, 0-based), each embedding a GROM (@code{$0000}–@code{$00FF}) or GRAM
(@code{$0100}+) card index plus STIC color as produced by
@code{compile-tileset-intv-screen}. Empty tile id 0 maps to four zero words.

Header layout (4 DECLEs): width, height, spawn-count, spawn-ptr.
Spawn data uses DECLE (one word per byte) for direct runtime addressing.
@code{MapCompiledHeaderWords EQU 4} in @file{Constants.s}; quadrant data
starts at @code{MAP_QUADRANTS} (offset 4 from @code{MAP_HEADER}).

STIC-OVERRIDE-GRID and STIC-OVERRIDE-TABLE, when supplied, emit
@code{MAP_STIC_TABLE} and @code{MAP_STIC_INDICES} labels for future
@code{PlotMapViewport} override patching (@pxref{Asset Formats Intellivision
STIC Region Overrides}); the runtime stub does not apply them yet.

@table @asis
@item MAP-NAME
Canonical map name string for comments.
@item OUTPUT-PATH
Pathname for the generated @file{Map.*.s} assembly file.
@item WIDTH, HEIGHT
Logical 16×16 tile dimensions.
@item TILE-GRID
2-D array of tile IDs, @code{(aref tile-grid x y 0)}.
@item TILESET-RECORDS
Vector of pre-encoded BACKTAB CSTK words (4 per tile ID, or empty).
@item SPAWN-TABLE
List of @code{(x y kind ref-id)} spawn entries.
@item STIC-OVERRIDE-GRID
2-D array of 8-byte override slot indices per logical tile (may be nil).
@item STIC-OVERRIDE-TABLE
List of 8-byte override entries (may be nil).
@end table"
  (check-type output-path (or pathname string))
  (let* ((lab (substitute #\_ #\. (pathname-name (merge-pathnames output-path))))
         (quadrants (make-array (* width height 4) :element-type '(unsigned-byte 16)))
         (spawns (or spawn-table nil))
         (stic-table (or stic-override-table nil))
         (stic-grid stic-override-grid)
         (has-stic (and stic-table stic-grid
                        (some (lambda (entry)
                                (some (lambda (b) (not (zerop b))) entry))
                              stic-table))))
    (dotimes (y height)
      (dotimes (x width)
        (let* ((tile-id (or (aref tile-grid x y 0) 0))
               (base (* tile-id 4)))
          (dotimes (q 4)
            (setf (aref quadrants (+ (* (+ (* y width) x) 4) q))
                  (if (and tileset-records (< (+ base q) (length tileset-records)))
                      (aref tileset-records (+ base q))
                      0))))))
    (ensure-directories-exist (merge-pathnames output-path))
    (with-output-to-file (src (merge-pathnames output-path) :if-exists :supersede
                                                            :external-format :utf-8)
      (format src ";;; Intellivision map: ~A (~D×~D logical tiles)~%" map-name width height)
      (format src ";;; Header: [width, height, spawn_count, spawn_ptr] (~
MapCompiledHeaderWords=4)~%")
      (format src "~A_MAP_WIDTH EQU ~D~%" lab width)
      (format src "~A_MAP_HEIGHT EQU ~D~%" lab height)
      (format src "~A_MAP_SPAWN_COUNT EQU ~D~%" lab (length spawns))
      (format src "~A_MAP_HEADER:~%" lab)
      (if (zerop (length spawns))
          (format src "    DECLE   ~D, ~D, 0, 0~%" width height)
          (format src "    DECLE   ~D, ~D, ~D, ~A_MAP_SPAWNS~%"
                  width height (length spawns) lab))
      (format src "~A_MAP_QUADRANTS:~%" lab)
      (dotimes (i (* width height 4))
        (format src "    DECLE   $~4,'0X~%" (aref quadrants i)))
      (when (zerop (length spawns))
        (format src "~A_MAP_SPAWNS:~%" lab))
      (when (plusp (length spawns))
        (format src "~A_MAP_SPAWNS:~%" lab)
        (dolist (entry spawns)
          (destructuring-bind (sx sy kind ref-id) entry
            (format src "    DECLE   ~D, ~D, ~D, ~D, ~D~%"
                    sx sy
                    (ecase kind
                      (:character 0)
                      (:object 1))
                    (ldb (byte 8 0) ref-id)
                    (ldb (byte 8 8) ref-id)))))
      ;; STIC override sections (runtime stub TODO; emitted for future use)
      (if has-stic
          (progn
            (format src "~A_MAP_STIC_COUNT EQU ~D~%" lab (length stic-table))
            (format src "~A_MAP_STIC_TABLE:~%" lab)
            (dolist (entry stic-table)
              (format src "    DECLE   ~{~D~^, ~}~%"
                      (coerce entry 'list)))
            (format src "~A_MAP_STIC_INDICES:~%" lab)
            (dotimes (y height)
              (dotimes (x width)
                (let ((idx (if (and stic-grid
                                    (< x (array-dimension stic-grid 0))
                                    (< y (array-dimension stic-grid 1)))
                               (aref stic-grid x y)
                               0)))
                  (format src "    DECLE   ~D~%" idx)))))
          (progn
            (format src "~A_MAP_STIC_COUNT EQU 0~%" lab)
            (format src "~A_MAP_STIC_TABLE:~%" lab)
            (format src "~A_MAP_STIC_INDICES:~%" lab)))
      (format *trace-output* "~&Wrote Intellivision map ~A (~Dx~D, ~D spawn~:p) to ~A."
              map-name width height (length spawns) (enough-namestring output-path)))))

(defun compile-tileset-intv-screen (png-file output-path palette-pixels width height)
  "Write OUTPUT-PATH assembly: per-logical-tile quadrant CSTK + deduplicated GRAM.

Each 16×16 map tile is four 8×8 quadrants (TL, TR, BL, BR). Each quadrant
resolves GROM-first to card @code{$0000}–@code{$00FF} or a shared GRAM slot
@code{$0100}+; the emitted CSTK word embeds card index and STIC color.
@code{*_GRAM_DATA} holds bitmaps only for non-GROM patterns (same encoding as
@code{compile-blob-intv-screen})."
  (check-type output-path (or pathname string))
  (let* ((original-width width)
         (original-height height))
    (setf width (- width (mod width 16)))
    (setf height (- height (mod height 16)))
    (when (or (< width 16) (< height 16))
      (error "Intellivision tileset ~A must include at least one 16×16 tile, got ~D×~D"
             png-file original-width original-height))
    (when (or (/= width original-width) (/= height original-height))
      (warn "Intellivision tileset ~A cropped from ~D×~D to ~D×~D for 16×16 tiles"
            png-file original-width original-height width height)))
  (let* ((tile-cols (/ width 16))
         (tile-rows (/ height 16))
         (tile-count (* tile-cols tile-rows))
         (uniq (make-array 64 :adjustable t :fill-pointer 0))
         (ht (make-hash-table :test 'equal))
         (records (make-array (* tile-count 4) :element-type '(unsigned-byte 16)))
         (lab (substitute #\_ #\. (pathname-name (merge-pathnames output-path))))
         (grom-bytes (intv-grom-bytes))
         (grom-map (when grom-bytes (intv-grom-key-to-card-map grom-bytes))))
    (let ((idx 0))
      (dotimes (row tile-rows)
        (dotimes (col tile-cols)
          (dolist (q (list (list (* col 16) (* row 16))
                           (list (+ (* col 16) 8) (* row 16))
                           (list (* col 16) (+ (* row 16) 8))
                           (list (+ (* col 16) 8) (+ (* row 16) 8))))
            (destructuring-bind (sx sy) q
              (multiple-value-bind (card cstk)
                  (intv-quadrant-card-and-cstk palette-pixels sx sy grom-map uniq ht)
                (declare (ignore card))
                (setf (aref records idx) cstk)
                (incf idx)))))))
    (let ((nuniq (length uniq)))
      (when (> nuniq 64)
        (error "Intellivision tileset ~A has ~D unique GRAM cards (max 64 after reduction)"
               png-file nuniq))
      (when (> nuniq 56)
        (warn "Intellivision tileset ~A reducing from ~D to 56 GRAM cards by Hamming-distance merging..."
              png-file nuniq)
        ;; Build card-ids array from CSTK records for the reduction pass
        (let ((card-ids (make-array (length records) :element-type '(unsigned-byte 16))))
          (loop for i from 0 below (length records)
                for cstk = (aref records i)
                do (setf (aref card-ids i)
                         (logior (intv-cstk-to-card-id cstk)
                                 (if (logtest cstk #x4000) #x8000 0))))
          (intv-reduce-gram-set uniq card-ids :max-slots 56 :grom-bytes grom-bytes)
          ;; Rebuild CSTK records from the updated card-ids
          (loop for i from 0 below (length records)
                for orig-cstk = (aref records i)
                for new-packed = (aref card-ids i)
                for new-card = (logand new-packed #x7FFF)
                for new-invert = (logtest new-packed #x8000)
                for color = (logand orig-cstk 7)
                do (setf (aref records i)
                         (intv-cstk-word new-card color new-invert)))))
      (setf nuniq (length uniq))
      (when (> nuniq 56)
        (warn "Intellivision tileset ~A: GRAM reduction could not bring card count below 56 (~D unique remain); MOB GRAM is compromised"
              png-file nuniq)))
    (when (>= nuniq 56)
      (warn "Intellivision tileset ~A uses ~D unique GRAM cards, overlapping the MOB reservation (slots 56–63)"
            png-file nuniq))
    (ensure-directories-exist (merge-pathnames output-path))
    (with-output-to-file (src (merge-pathnames output-path) :if-exists :supersede
                                                            :external-format :utf-8)
      (format src ";;; Intellivision map tileset: quadrant CSTK + GRAM cards~%")
      (format src ";;; Source: ~A~%" png-file)
      (format src ";;; Logical tiles: ~D×~D (~D×~D px); ~D unique GRAM card~:P~2%"
              tile-cols tile-rows width height nuniq)
      (format src "~A_TILE_COLS EQU ~D~%" lab tile-cols)
      (format src "~A_TILE_ROWS EQU ~D~%" lab tile-rows)
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
                         do (format src "    DECLE   $~3,'0X~%" low10)
                            (format src "    DECLE   $~3,'0X~%" high10)))))
      (format src "~A_QUADRANT_CSTK:~%" lab)
      (dotimes (i (* tile-count 4))
        (format src "    DECLE   $~4,'0X~%" (aref records i)))
      (format *trace-output* "~&Wrote Intellivision tileset (~D logical tiles, ~D GRAM) to ~A."
              tile-count nuniq (enough-namestring output-path)))))

(defun intv-cstk-to-card-id (cstk)
  "Extract the unified card ID from a BACKTAB CSTK word.
Returns @code{$0000}–@code{$00FF} for GROM or @code{$0100}+ for GRAM
(without the invert bit $4000)."
  (if (logtest cstk #x1000)
      (+ #x100 (ash (logand cstk #x1F8) -3))
      (ash (logand cstk #xFF8) -3)))

(defun intv-cstk-word (card-id color invert)
  "Return the 16-bit BACKTAB CSTK word for CARD-ID, COLOR, and INVERT flag.

CARD-ID is the unified card space: @code{$0000}–@code{$00FF} for GROM
cards, @code{$0100}–@code{$013F} for GRAM slots 0–63.  COLOR is the
STIC palette index 0–15.  When INVERT is non-nil, bit 14 (@code{$4000})
is set.

@table @asis
@item CARD-ID
Unified card index: @code{< $100} = GROM, @code{>= $100} = GRAM slot
@code{(- card-id #x100)}.
@item COLOR
STIC foreground color 0–15.
@item INVERT
When non-nil, sets the hardware invert bit (@code{$4000}).
@end table"
  (let ((base (if (>= card-id #x100)
                  (+ #x1000 (* (- card-id #x100) 8) color)
                  (+ (* card-id 8) color))))
    (logior base (if invert #x4000 0))))

(defun intv-gram-lookup-or-allocate (key uniq ht)
  "Look up or allocate a GRAM slot for KEY, deduplicating against the inverted tile.

Returns @code{(values gram-id invert-p)}.  If KEY is already in HT,
returns its slot with @code{invert-p = NIL}.  If the bitwise-complement
of KEY is in HT, returns that slot with @code{invert-p = T} (one GRAM
slot serves both orientations).  Otherwise allocates a new slot for KEY
and returns it with @code{invert-p = NIL}.

@table @asis
@item KEY
8-byte row-bitmap list from @code{intv-tile-row-bytes}.
@item UNIQ
Adjustable vector of allocated keys (fill-pointer = slot count).
@item HT
@code{equal} hash table mapping KEY to slot index.
@end table"
  (multiple-value-bind (gram-id found) (gethash key ht)
    (if found
        (values gram-id nil)
        (let* ((inv-key (intv-invert-tile-row-bytes key))
               (inv-id (gethash inv-key ht)))
          (if inv-id
              (values inv-id t)
              (progn
                (setf gram-id (length uniq))
                (setf (gethash key ht) gram-id)
                (vector-push-extend key uniq)
                (values gram-id nil)))))))

(defun intv-quadrant-card-and-cstk (palette-pixels sx sy grom-map uniq ht)
  "Return (values CARD-ID CSTK-WORD) for one 8×8 quadrant at (SX,SY).

GROM-first: matching @file{minigrom.bin} patterns yield card @code{$0000}–@code{$00FF}
(no GRAM slot); otherwise allocates a shared GRAM slot @code{$0100}+. CSTK-WORD
embeds the card index, color, and invert bit for BACKTAB. Reuses
@code{intv-grom-lookup} and @code{intv-gram-lookup-or-allocate} from
@code{compile-blob-intv-screen}."
  (let* ((key (intv-tile-row-bytes palette-pixels sx sy))
         (color (intv-dominant-stic-color palette-pixels sx sy)))
    (if grom-map
        (multiple-value-bind (grom-id invert found)
            (intv-grom-lookup key grom-map)
          (if found
              (values grom-id (intv-cstk-word grom-id color invert))
              (multiple-value-bind (gram-id invert)
                  (intv-gram-lookup-or-allocate key uniq ht)
                (values (+ #x100 gram-id)
                        (intv-cstk-word (+ #x100 gram-id) color invert)))))
        (multiple-value-bind (gram-id invert)
            (intv-gram-lookup-or-allocate key uniq ht)
          (values (+ #x100 gram-id)
                  (intv-cstk-word (+ #x100 gram-id) color invert))))))

(defun intv-reduce-gram-set (uniq tile-ids &key max-slots grom-bytes)
  "Reduce UNIQ (adjustable vector of tile keys, fill-pointer = slot count)
and TILE-IDS array so GRAM uses at most MAX-SLOTS unique cards.

Merges the most similar card pairs (pixel-by-pixel Hamming distance),
including GROM substitutions when GROM-BYTES is non-NIL.  Returns NIL;
modifies both UNIQ and TILE-IDS in place.

When merging GRAM slot B into A: entries that referenced B are remapped to
A, preserving or flipping the invert bit ($8000) to minimize visual
difference.  When a GRAM slot is replaced by a GROM card, all its entries
become GROM references.

@table @asis
@item UNIQ
Adjustable vector (fill-pointer = used slot count) of 8-byte canonical
tile keys.
@item TILE-IDS
Array of packed card+invert values (bit 15 = invert, bits 14–0 = card ID).
@item MAX-SLOTS
Hard limit for UNIQ fill-pointer after reduction.
@item GROM-BYTES
2048-byte GROM image or NIL (skip GROM substitution).
@end table"
  (when (<= (length uniq) max-slots)
    (return-from intv-reduce-gram-set nil))
  (let* ((grom-keys (when grom-bytes
                      (let ((keys (make-array 256)))
                        (loop for card from 0 below 256
                              for base = (* card 8)
                              do (setf (aref keys card)
                                       (loop for row from 0 below 8
                                             collect (aref grom-bytes (+ base row)))))
                        keys)))))
  (loop while (> (length uniq) max-slots)
        for n-gram = (length uniq)
        for best-dist = 65
        for best-a = nil
        for best-b = nil
        for best-kind = nil
        for best-flip = nil
        do
           ;; Pairwise GRAM vs GRAM
           (loop for a from 0 below n-gram
                 for key-a = (aref uniq a)
                 do (loop for b from (1+ a) below n-gram
                          for key-b = (aref uniq b)
                          for same-dist = (tile-hamming-distance key-a key-b)
                          for flip-dist = (tile-hamming-distance
                                           (tile-invert key-a) key-b)
                          do (when (< same-dist best-dist)
                               (setf best-dist same-dist best-a a best-b b
                                     best-kind :gram-gram best-flip nil))
                             (when (< flip-dist best-dist)
                               (setf best-dist flip-dist best-a a best-b b
                                     best-kind :gram-gram best-flip t))))
           ;; GRAM vs GROM (all 256 cards)
           (when grom-keys
             (loop for a from 0 below n-gram
                   for key-a = (aref uniq a)
                   do (loop for grom-card from 0 below 256
                            for grom-key = (aref grom-keys grom-card)
                            for same-dist = (tile-hamming-distance key-a grom-key)
                            for flip-dist = (tile-hamming-distance
                                             key-a (tile-invert grom-key))
                            do (when (< same-dist best-dist)
                                 (setf best-dist same-dist best-a a best-b grom-card
                                       best-kind :gram-grom best-flip nil))
                               (when (< flip-dist best-dist)
                                 (setf best-dist flip-dist best-a a best-b grom-card
                                       best-kind :gram-grom best-flip t)))))
           ;; Apply the best merge found this round
           (ecase best-kind
             (:gram-gram
              ;; Merge slot best-b into slot best-a.  If best-flip is T,
              ;; flip the invert bit for all remapped entries.
              (warn "Intellivision GRAM overflow reduction: merging slot ~D into ~D (dist=~D)~@[; flipping invert~]"
                    best-b best-a best-dist best-flip)
              (let ((hi-slot best-b)
                    (target-slot best-a))
                ;; Update tile-ids: remap hi-slot -> target-slot
                (loop for i from 0 below (length tile-ids)
                      for packed = (aref tile-ids i)
                      for card-id = (logand packed #x7FFF)
                      for slot = (when (>= card-id #x100) (- card-id #x100))
                      do (cond
                           ((eql slot hi-slot)
                            (let ((old-invert (logtest packed #x8000))
                                  (new-invert (if best-flip (not old-invert) old-invert)))
                              (setf (aref tile-ids i)
                                    (logior (+ #x100 target-slot)
                                            (if new-invert #x8000 0)))))
                           ((and slot (> slot hi-slot))
                            (let ((invert-bit (logand packed #x8000)))
                              (setf (aref tile-ids i)
                                    (logior (+ #x100 (1- slot)) invert-bit))))))
                ;; Remove hi-slot from uniq, shifting higher entries down
                (loop for i from hi-slot below (1- n-gram)
                      do (setf (aref uniq i) (aref uniq (1+ i))))
                (vector-pop uniq)))
             (:gram-grom
              ;; Replace all uses of GRAM slot best-a with GROM card best-b.
              ;; If best-flip is T, set the invert bit for the GROM entries.
              ;; Entries that previously used slot best-a with invert get the
              ;; opposite.
              (warn "Intellivision GRAM overflow reduction: replacing slot ~D with GROM card ~D (dist=~D)~@[; flipping invert~]"
                    best-a best-b best-dist best-flip)
              (let ((dead-slot best-a)
                    (grom-card best-b))
                (loop for i from 0 below (length tile-ids)
                      for packed = (aref tile-ids i)
                      for card-id = (logand packed #x7FFF)
                      for slot = (when (>= card-id #x100) (- card-id #x100))
                      do (cond
                           ((eql slot dead-slot)
                            (let ((old-invert (logtest packed #x8000))
                                  (new-invert (if best-flip (not old-invert) old-invert)))
                              (setf (aref tile-ids i)
                                    (logior grom-card
                                            (if new-invert #x8000 0)))))
                           ((and slot (> slot dead-slot))
                            (let ((invert-bit (logand packed #x8000)))
                              (setf (aref tile-ids i)
                                    (logior (+ #x100 (1- slot)) invert-bit))))))
                ;; Remove dead slot from uniq
                (loop for i from dead-slot below (1- n-gram)
                      do (setf (aref uniq i) (aref uniq (1+ i))))
                (vector-pop uniq))
              ;; Update grom-cells count is tracked by the caller; we just
              ;; emit a warning.  The GRAM→GROM substitution reduces unique
              ;; GRAM count by 1.
              nil))))

(defun intv-tile-hamming-distance (key-a key-b)
  "Intv alias for @code{tile-hamming-distance}."
  (tile-hamming-distance key-a key-b))

(defun read-intv-art-index (index-in)
  "Read Intellivision art index file and return list of (png-name width-px height-px)"
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading Intellivision art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while (and line (plusp (length line)) (not (char= #\; (char line 0))))
            do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page) line)))
                 (cond
                   ((emptyp line) nil)
                   ((char= #\# (char line 0)) nil)
                   (t (destructuring-bind (png-name dimensions)
                          (split-sequence #\Space line :remove-empty-subseqs t :test #'char=)
                        (destructuring-bind (width-px height-px)
                            (split-sequence #\× dimensions :test #'char=)
                          (push (list (make-pathname :defaults index-in
                                                     :name (subseq png-name 0
                                                                   (position #\. png-name :from-end t))
                                                     :type "png")
                                      (parse-integer width-px)
                                      (parse-integer height-px))
                                png-list))))))))
    (format *trace-output* " done. Got ~:D PNG files to read." (length png-list))
    (reverse png-list)))

