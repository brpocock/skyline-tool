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

(defun print-mini-tile-map (tileset &optional (stream *trace-output*))
  "Print a mini-tile-map of a TILESET to STREAM (default *trace-output*).

Each tile is displayed as one print-wide-pixel whose color is the
average (in CIE XYZ) of all non-nil pixels in that tile.
Output is top-to-bottom rows, left-to-right within each row."
  (let* ((image (tileset-image tileset))
         (tile-width 8)
         (tile-height 16)
         (tiles-across (floor (array-dimension image 0) tile-width))
         (tiles-down (floor (1- (array-dimension image 1)) tile-height)))
    (format stream "~&Mini-tile-map (~D×~D tiles):~%" tiles-across tiles-down)
    (dotimes (ty tiles-down)
      (dotimes (tx tiles-across)
        (let* ((sx (* tx tile-width))
               (sy (* ty tile-height))
               (tile (extract-region image sx sy
                                     (+ sx tile-width) (+ sy tile-height)))
               (colors (tile-pixel-colors tile))
               (avg (average-rgb-via-xyz colors)))
          (print-wide-pixel avg stream)))
      (terpri stream))
    (finish-output stream)))

(defun print-maptile-mini-view (tile-grid base-tileset &optional (stream *trace-output*))
  "Print a miniature view of a map tile grid to STREAM (default *trace-output*).

Each tile in the map grid is displayed as one print-wide-pixel whose
color is the average (in CIE XYZ) of all non-nil pixels in that tile.
Output is top-to-bottom rows, left-to-right within each row."
  (let* ((map-width (array-dimension tile-grid 0))
         (map-height (array-dimension tile-grid 1))
         (tileset-image (tileset-image base-tileset))
         (tiles-per-row (floor (array-dimension tileset-image 0) 8)))
    (format stream "~&Mini-map (~D×~D tiles):~%" map-width map-height)
    (dotimes (y map-height)
      (dotimes (x map-width)
        (let ((tile-id (aref tile-grid x y 0)))
          (if (zerop tile-id)
              (print-wide-pixel (list 0 0 0) stream)
              (let* ((tx (mod (1- tile-id) tiles-per-row))
                     (ty (floor (1- tile-id) tiles-per-row))
                     (sx (* tx 8))
                     (sy (* ty 16))
                     (tile (extract-region tileset-image sx sy (+ sx 8) (+ sy 16)))
                     (colors (tile-pixel-colors tile))
                     (avg (average-rgb-via-xyz colors)))
                (print-wide-pixel avg stream)))))
      (terpri stream))
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
