(in-package :skyline-tool)

(defun average-rgb-via-xyz (colors)
  "Average a list of RGB triples in CIE XYZ space, return (r g b).

  COLORS is a list of (r g b) triples, each component 0-255.
  Returns a single (r g b) triple with rounded integer components.
  An empty list produces black (0 0 0)."
  (if (null colors)
      (list 0 0 0)
      (let ((n (length colors))
            (sum-x 0.0) (sum-y 0.0) (sum-z 0.0))
        (dolist (c colors)
          (destructuring-bind (r g b) c
            (multiple-value-bind (x y z) (dufy:rgb-to-xyz r g b)
              (incf sum-x x)
              (incf sum-y y)
              (incf sum-z z))))
        (multiple-value-bind (r g b)
            (dufy:xyz-to-rgb (/ sum-x n) (/ sum-y n) (/ sum-z n))
          (list (max 0 (min 255 (round r)))
                (max 0 (min 255 (round g)))
                (max 0 (min 255 (round b)))))))

(defun region-pixel-colors (palette-pixels x y width height)
  "Collect pixel RGB colors from a rectangular region.

  PALETTE-PIXELS is a 2D array of palette indices or nil.
  X, Y are the top-left corner; WIDTH, HEIGHT is the region size.
  Transparent (nil) pixels are reported as black, so that images drawn
  against a transparent background still separate into light and dark
  quadrants.  Returns a list of (r g b) triples from the machine palette."
  (let* ((machine-colors (machine-palette))
         (colors nil))
    (dotimes (v height)
      (dotimes (u width)
        (let ((pixel (aref palette-pixels
                           (min (1- (array-dimension palette-pixels 0)) (+ x u))
                           (min (1- (array-dimension palette-pixels 1)) (+ y v)))))
          (push (if pixel (nth pixel machine-colors) (list 0 0 0)) colors))))
    (nreverse colors)))

(defun tile-pixel-colors (tile-image)
  "Collect all non-nil pixel RGB colors from a single tile.

  TILE-IMAGE is a 2D array of palette indices or nil.
  Returns a list of (r g b) triples from the machine palette."
  (let* ((w (array-dimension tile-image 0))
         (h (array-dimension tile-image 1))
         (machine-colors (machine-palette))
         (colors nil))
    (dotimes (y h)
      (dotimes (x w)
        (let ((pixel (aref tile-image x y)))
          (when pixel
            (push (nth pixel machine-colors) colors)))))
    (nreverse colors)))

(defun tile-effective-rgb-colors (tile-image palette-table palette-index)
  "Collect RGB colors from TILE-IMAGE mapped through the effective palette.

  PALETTE-TABLE is an 8×4 array of machine palette indices
  \(from @code{extract-palettes}).  PALETTE-INDEX (0-7) is the tile's
  assigned palette.  Returns a list of (r g b) triples where each pixel
  is mapped to its nearest color within the effective palette."
  (let* ((machine-colors (machine-palette))
         (palette (loop for c below 4 collect (aref palette-table palette-index c)))
         (palette-rgbs (mapcar (lambda (idx) (nth idx machine-colors)) palette))
         (colors nil))
    (destructuring-bind (w h) (array-dimensions tile-image)
      (dotimes (y h)
        (dotimes (x w)
          (let ((pixel (aref tile-image x y)))
            (when pixel
              (let ((pos (position pixel palette)))
                (push (if pos
                          (nth pos palette-rgbs)
                          (destructuring-bind (r g b) (nth pixel machine-colors)
                            (find-nearest-in-palette palette-rgbs r g b)))
                      colors)))))))
    (nreverse colors)))

(defun cell-quadrant-averages (palette-pixels x y w h)
  "Compute average XYZ colors and lightness for 4 quadrants in cell.
Returns values: nw-avg ne-avg se-avg sw-avg nw-light ne-light se-light sw-light

Every quadrant samples at least one pixel: when the cell is smaller
than 2×2 pixels the quadrants overlap the cell's pixels, and the right
and bottom edges are aligned to the cell edge (so sampling never reads
outside the cell)."
  (let* ((half-w (max 1 (floor w 2)))
         (half-h (max 1 (floor h 2)))
         (right (+ x half-w))
         (bottom (+ y half-h))
         (nw (region-pixel-colors palette-pixels x y half-w half-h))
         (ne (region-pixel-colors palette-pixels right y half-w half-h))
         (se (region-pixel-colors palette-pixels right bottom half-w half-h))
         (sw (region-pixel-colors palette-pixels x bottom half-w half-h)))
    (let ((nw-avg (average-rgb-via-xyz nw))
          (ne-avg (average-rgb-via-xyz ne))
          (se-avg (average-rgb-via-xyz se))
          (sw-avg (average-rgb-via-xyz sw)))
      (let* ((nw-light (rgb-hsl-lightness nw-avg))
             (ne-light (rgb-hsl-lightness ne-avg))
             (se-light (rgb-hsl-lightness se-avg))
             (sw-light (rgb-hsl-lightness sw-avg)))
        (values nw-avg ne-avg se-avg sw-avg nw-light ne-light se-light sw-light))))))

(defun tileset-tile-count (tileset)
  "Number of complete tiles in TILESET, derived from image dimensions.
Matches @code{extract-8×16-tiles} so tile-id x,y positions are correct."
  (let ((image (tileset-image tileset)))
    (* (floor (array-dimension image 0) 8)
       (floor (array-dimension image 1) 16))))

(defun resolve-gid (gid base-tileset decal-tileset)
  "Return (values TILESET LOCAL-TILE-ID) for GID across two tilesets."
  (flet ((in-range-p (ts)
           (and ts
                (let ((first (tileset-gid ts))
                      (last (+ (tileset-gid ts)
                               (tileset-tile-count ts) -1)))
                  (<= first gid last)))))
    (cond
      ((in-range-p base-tileset)
       (values base-tileset (- gid (tileset-gid base-tileset))))
      ((in-range-p decal-tileset)
       (values decal-tileset (- gid (tileset-gid decal-tileset))))
      (t (values nil 0)))))

(defun print-mini-tile-map (tileset &optional (stream *trace-output*))
  "Print a scaled-down ANSI thumbnail of TILESET, filling the terminal.

  The whole tileset image is sampled into evenly-divided character
  cells (via %compute-ansi-sizing), not one glyph per tile, so even a
  small tileset fills the available terminal space."
  (let* ((image (tileset-image tileset))
         (w (array-dimension image 0))
         (h (array-dimension image 1)))
    (format stream "~&Tileset image (~D×~D tiles):~%"
            (floor w 8) (floor h 16))
    (multiple-value-bind (rw rh cols rows)
        (%compute-ansi-sizing w h)
      (%print-thumbnail-cells image stream rw rh cols rows :ansi-p t)))))

(defun rgb-hsl-lightness (rgb)
  "Return HSL lightness (0.0-1.0) for RGB triple (r g b), each 0-255."
  (if (null rgb)
      0.0
      (destructuring-bind (r g b) rgb
        (multiple-value-bind (h s l)
            (dufy:rgb-to-hsl r g b)
          (declare (ignore h s))
          l))))

(defun print-mini-map (width height gid-grid base-tileset decal-tileset
                       &optional (stream *trace-output*))
  "Print a mini-map of the tile grid to STREAM, scaled to fill terminal.

  The map is assembled into a full pixel image (8×16 px per tile),
  then rendered with the same ANSI quadrant sampling as the other
  thumbnails.  Maps are 160B mode, so each pixel is 2:1 wide;
  %compute-ansi-sizing applies that aspect correction."
  (let* ((base-image (tileset-image base-tileset))
         (decal-image (when decal-tileset (tileset-image decal-tileset)))
         (pixel-w (* width 8))
         (pixel-h (* height 16))
         (pixels (make-array (list pixel-w pixel-h) :initial-element nil)))
    (flet ((blit-tile (image tid map-x map-y)
             (let* ((tiles-across (floor (array-dimension image 0) 8))
                    (tile-u (mod tid tiles-across))
                    (tile-v (floor tid tiles-across)))
               (dotimes (v 16)
                 (dotimes (u 8)
                   (let ((p (aref image (+ (* tile-u 8) u) (+ (* tile-v 16) v))))
                     (when p
                       (setf (aref pixels (+ (* map-x 8) u) (+ (* map-y 16) v)) p))))))))
      (dotimes (map-y height)
        (dotimes (map-x width)
          (multiple-value-bind (ts tid)
              (resolve-gid (aref gid-grid map-x map-y) base-tileset decal-tileset)
            (when ts
              (if (eql ts base-tileset)
                  (blit-tile base-image tid map-x map-y)
                  (blit-tile decal-image tid map-x map-y))))))
      (multiple-value-bind (rw rh cols rows)
          (%compute-ansi-sizing pixel-w pixel-h 2)
        (format stream "~&Mini-map (~D×~D tiles):~%" width height)
        (%print-thumbnail-cells pixels stream rw rh cols rows :ansi-p t)))))

;; ANSI terminal output for thumbnails (quadrant-based, 16 patterns)
(defun print-ansi-cell-pattern (palette-pixels x y w h stream)
  "Print a single ANSI cell using 4-quadrant algorithm.
Divides cell into 4 quadrants, averages each in XYZ, uses median
lightness to classify, and renders with 16 quadrant-drawing characters."
  (let* ((all-colors (region-pixel-colors palette-pixels x y w h))
         (unique-colors (remove-duplicates all-colors :test #'equal)))
    (when (<= (length unique-colors) 1)
      (let ((color (or (first unique-colors) (list 0 0 0))))
        (princ (ansi-color-rgb (first color) (second color) (third color) nil) stream)
        (princ #\Space stream)
        (return-from print-ansi-cell-pattern)))
    (multiple-value-bind (nw-avg ne-avg se-avg
                          sw-avg nw-light ne-light se-light sw-light)
        (cell-quadrant-averages palette-pixels x y w h)
      (let* ((lightnesses (list nw-light ne-light se-light sw-light))
             (sorted-lights (sort (copy-list lightnesses) #'<))
             (median (second sorted-lights))
             (all-quads (list nw-avg ne-avg se-avg sw-avg))
             (light-quads (remove-if-not (lambda (q) (> (rgb-hsl-lightness q) median))
                                         all-quads))
             (dark-quads (remove-if (lambda (q) (> (rgb-hsl-lightness q) median))
                                    all-quads)))
        (when (null light-quads)
          (let* ((sums (mapcar (lambda (q) (+ (first q) (second q) (third q))) all-quads))
                 (max-index (position (apply #'max sums) sums)))
            (setf light-quads (list (nth max-index all-quads)))))
        (when (null dark-quads)
          (let* ((sums (mapcar (lambda (q) (+ (first q) (second q) (third q))) all-quads))
                 (min-index (position (apply #'min sums) sums)))
            (setf dark-quads (list (nth min-index all-quads)))))
        (let* ((light-avg (average-rgb-via-xyz light-quads))
               (dark-avg (average-rgb-via-xyz dark-quads))
               (bits (+ (if (<= nw-light median) 8 0)
                        (if (<= ne-light median) 4 0)
                        (if (<= se-light median) 1 0)
                        (if (<= sw-light median) 2 0)))
               (glyphs " ▗▖▄▝▐▞▟▘▚▌▙▀▜▛█")
               (fg dark-avg)
               (bg light-avg)
               (char (aref glyphs bits)))
          (princ (ansi-color-rgb (first fg) (second fg) (third fg) t) stream)
          (princ (ansi-color-rgb (first bg) (second bg) (third bg) nil) stream)
          (princ char stream))))))
