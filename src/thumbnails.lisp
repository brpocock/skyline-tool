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
                (max 0 (min 255 (round b))))))))

(defun region-pixel-colors (palette-pixels x y width height)
  "Collect all non-nil pixel RGB colors from a rectangular region.

PALETTE-PIXELS is a 2D array of palette indices or nil.
X, Y are the top-left corner; WIDTH, HEIGHT is the region size.
Returns a list of (r g b) triples from the machine palette."
  (let* ((machine-colors (machine-palette))
         (colors nil))
    (dotimes (dy height)
      (dotimes (dx width)
        (let ((pixel (aref palette-pixels (+ x dx) (+ y dy))))
          (when pixel
            (push (nth pixel machine-colors) colors)))))
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

(defun compute-tile-average-cache (tileset)
  "Precompute the average XYZ color for every tile in TILESET.

Each tile's colors are mapped through its effective palette before
averaging.  Returns a vector of (r g b) triples indexed by local tile-id."
  (let* ((image (tileset-image tileset))
         (palette-table (extract-palettes image))
         (tiles-across (floor (array-dimension image 0) 8))
         (tiles-down (floor (array-dimension image 1) 16))
         (total-tiles (* tiles-across tiles-down))
         (cache (make-array total-tiles)))
    (dotimes (tile-id total-tiles cache)
      (let* ((tx (mod tile-id tiles-across))
             (ty (floor tile-id tiles-across))
             (pal-idx (aref (tileset-palettes tileset) tile-id))
             (tile (extract-region image
                                   (* tx 8) (* ty 16)
                                   (+ (* tx 8) 8) (+ (* ty 16) 16)))
             (colors (tile-effective-rgb-colors tile palette-table pal-idx)))
        (setf (aref cache tile-id) (average-rgb-via-xyz colors))))))

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
  "Print a mini-tile-map of a TILESET to STREAM (default *trace-output*).

Each tile is displayed as 2×2 shaded grayscale pixels (4 characters
across × 2 rows per tile).  Pixel darkness is the XYZ-luminance of
the quadrant's average colour.  Output is top-to-bottom rows,
left-to-right within each row."
  (let* ((image (tileset-image tileset))
         (tile-width 8)
         (tile-height 16)
         (tiles-across (floor (array-dimension image 0) tile-width))
         (tiles-down (floor (array-dimension image 1) tile-height))
         (total-tiles (* tiles-across tiles-down))
         (half-w (floor tile-width 2))
         (half-h (floor tile-height 2)))
    (format stream "~&Mini-tile-map (~D×~D tiles, 2×2 px each):~%" tiles-across tiles-down)
    #+mcclim
    (when (typep stream 'clim:sheet)
      (%print-clim-pixels image stream)
      (return-from print-mini-tile-map))
    (dotimes (ty tiles-down)
      (dotimes (qy 2)
        (dotimes (tx tiles-across)
          (let ((x0 (* tx tile-width))
                (y0 (* ty tile-height)))
            (dotimes (qx 2)
              (let ((sx (+ x0 (* qx half-w)))
                    (sy (+ y0 (* qy half-h))))
                (multiple-value-bind (light dark light-count dark-count)
                    (%region->two-populations image sx sy half-w half-h)
                  (if (null dark)
                      (print-wide-pixel light stream)
                      (let ((char (%darkness-char dark-count (+ light-count dark-count)))
                            (ansi-p (and (not (typep stream 'string-stream))
                                         (tty-xterm-p))))
                        (if ansi-p
                            (%ansi-two-color-cell dark light char stream)
                            (format stream "~c~c" char char)))))))))
        (terpri stream)))
    (finish-output stream)))

(defun print-mini-blob-view (palette-pixels &optional (stream *trace-output*))
  "Print a mini-blob (scaled-down) view of an image to STREAM.

For 160A/B modes (width ≤ 160): each 8×16 pixel region → one pixel.
For 320A/B/C/D modes (width > 160): each 16×16 pixel region → one pixel.

PALETTE-PIXELS is a 2D array of palette indices (from png->palette).
The bottom palette-strip row (if present) is included in the display."
  (let* ((width (array-dimension palette-pixels 0))
         (height (array-dimension palette-pixels 1))
         (region-w (if (> width 160) 16 8))
         (region-h 16)
         (cols (floor width region-w))
         (rows (floor height region-h)))
    (format stream "~&Mini-blob view (~D×~D regions, ~D×~Dpx each):~%"
            cols rows region-w region-h)
    (dotimes (ry rows)
      (dotimes (rx cols)
        (let ((sx (* rx region-w))
              (sy (* ry region-h)))
          (print-wide-pixel
           (average-rgb-via-xyz
            (region-pixel-colors palette-pixels sx sy region-w region-h))
           stream)))
      (terpri stream))
    (finish-output stream)))

(defun print-mini-map (width height gid-grid base-tileset decal-tileset
                       &optional (stream *trace-output*))
  "Print a mini-map of the tile grid to STREAM (default *trace-output*).

Each grid cell is one print-wide-pixel whose color is the average (in
CIE XYZ) of all pixels in the tile referenced by that cell, mapped
through the tile's effective palette."
  (let* ((base-cache (compute-tile-average-cache base-tileset))
         (decal-cache (and decal-tileset (compute-tile-average-cache decal-tileset))))
    (format stream "~&Mini-map (~D×~D tiles):~%" width height)
    (dotimes (y height)
      (dotimes (x width)
        (let* ((gid (aref gid-grid x y))
               (avg (multiple-value-bind (ts tid) (resolve-gid gid base-tileset decal-tileset)
                      (cond
                        ((eql ts base-tileset)
                         (if (< tid (length base-cache))
                             (aref base-cache tid)
                             (list 0 0 0)))
                        ((and decal-cache (< tid (length decal-cache)))
                         (aref decal-cache tid))
                        (t (list 0 0 0))))))
          (print-wide-pixel avg stream)))
      (terpri stream))
    (finish-output stream)))
