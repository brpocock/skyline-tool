(in-package :skyline-tool)

(defmacro dovector ((var seq) &body body)
  `(loop for ,var across ,seq do (progn ,@body) finally (return (values))))

(defclass level ()
  ((decals :accessor level-decals :initarg :decals)
   (grid :accessor level-grid :initarg :grid)
   (objects :accessor level-objects :initarg :objects)
   (name :reader level-name :initarg :name)))

(defvar *screen-ticker* 0)

(defclass grid/tia ()
  ((tiles :reader grid-tiles :initarg :tiles)
   (colors :reader grid-row-colors :initarg :colors)
   (background-color :reader grid-background-color :initarg :background-color)
   (id :reader grid-id :initform (incf *screen-ticker*))))

(defgeneric list-grid-row-colors (grid))
(defgeneric list-grid-tiles (grid))

(defmethod list-grid-row-colors ((grid grid/tia))
  (coerce (grid-row-colors grid) 'list))

(defun list-grid-row-palette-colors (grid)
  (mapcar (curry #'apply #'rgb->palette)
          (apply #'concatenate 'list (list-grid-row-colors grid))))

(defmethod list-grid-tiles ((grid grid/tia))
  (let (list
        (tiles (grid-tiles grid)))
    (dotimes (y 8)
      (dotimes (x 4)
        (push (aref tiles x y) list)))
    (nreverse list)))

(defun assocdr (key alist &optional (errorp t))
  (or (second (assoc key alist :test #'equalp))
      (when errorp
        (error "Could not find ~a in ~s" key alist))))

(defun pin (n min max)
  (min max (max min n)))

(defun plist-keys (plist)
  (loop for (key value) on plist by #'cddr
        collecting key))

(defun plist-values (plist)
  (loop for (key value) on plist by #'cddr
        collecting value))

(defun parse-tile-animation-set (&rest tilesets)
  (let ((animations (list)))
    (flet ((add-animation (sequence)
             (pushnew sequence animations
                      :key #'plist-keys
                      :test (lambda (a b)
                              #+ () (format *trace-output* "~&//=? ~s ⩭ ~s" a b)
                              (some (lambda (el) (member el b)) a)))))
      (dolist (tileset tilesets)
        (dolist (tile-data (cddr (xmls:parse-to-list
                                  (read-file-into-string
                                   (namestring (tileset-pathname tileset))))))
          (when (equal "tile" (car tile-data))
            (let (#+ () (tile-id (parse-integer (assocdr "id" (second tile-data)))))
              (dolist (animation (cddr tile-data))
                (when (equal "animation" (car animation))
                  (let ((sequence (list)))
                    (dolist (frame (cddr animation))
                      (assert (equal "frame" (car frame)))
                      (let ((frame-tile (parse-integer (assocdr "tileid" (second frame))))
                            (duration (/ (parse-integer (assocdr "duration" (second frame)))
                                         1000)))
                        (push frame-tile sequence)
                        (push duration sequence)))
                    ;; TODO If  the sequence  uses a  frame already  defined as
                    ;; a part of another sequence, omit it.
                    (add-animation (reverse sequence))))))))))
    animations))

(defun split-into-bytes (tile-collision-bitmap)
  (let ((width (array-dimension tile-collision-bitmap 0))
        (bytes (list)))
    (dotimes (y (array-dimension tile-collision-bitmap 1))
      (dotimes (byte (floor width 8))
        (let ((value 0))
          (dotimes (bit 8)
            (let ((x (+ bit (* 8 byte))))
              (when (< x width)
                (when (plusp (aref tile-collision-bitmap x y))
                  (setf value (logior value (ash 1 bit)))))))
          (push value bytes))))
    (reverse bytes)))

(declaim (ftype (function ((unsigned-byte 8)
                           (unsigned-byte 8)
                           (unsigned-byte 8)
                           (unsigned-byte 8))
                          (integer 0 (#.(expt 2 32))))
                32-bit-word))
(defun 32-bit-word (b0 b1 b2 b3)
  (declare (optimize (speed 3) (debug 0)))
  (logior b0 (ash b1 8) (ash b2 16) (ash b3 24)))

(defun bytes-to-32-bits (bytes)
  (loop for i from 0 by 4 below (length bytes)
        collecting (apply #'32-bit-word (coerce (subseq bytes i (+ i 4)) 'list))))

(defun split-grid-to-rows (width height words)
  (let ((output (make-array (list width height) :element-type 'integer))
        (i 0))
    (dotimes (y height)
      (dotimes (x width)
        (setf (aref output x y) (elt words i))
        (incf i)))
    output))

(defun parse-layer (layer)
  (let ((data (xml-match "data" layer)))
    (assert data)
    (assert (equal "base64" (assocdr "encoding" (second data))))
    (assert (stringp (third data)))
    (let* ((width (parse-integer (assocdr "width" (second layer))))
           (height (parse-integer (assocdr "height" (second layer)))))
      (split-grid-to-rows width height
                          (bytes-to-32-bits
                           (cl-base64:base64-string-to-usb8-array (third data)))))))

(defun find-tile-by-number (number tileset
                            &key decal-tileset decal2-tileset x y layer)
  "Find a tile by NUMBER among TILESETS"
  (let ((tilesets (remove-if #'null (list tileset decal-tileset decal2-tileset))))
    (loop for tileset in tilesets
          for id = (- number (tileset-gid tileset))
          when (<= (tileset-gid tileset)
                   number
                   (1- (+ (tileset-gid tileset)
                          (array-dimension (tile-attributes tileset) 0))))
            return (values id
                           (coerce (loop for b below 6
                                         collect (aref (tile-attributes tileset)
                                                       id b))
                                   'vector)
                           (position tileset tilesets))
          finally
             (progn (cerror "Continue, using tile $7f"
                            "Can't find tile with ID ~d ($~2,'0x) ~:[~2*~;at (~d, ~d) on layer “~a”~] in linked tileset~p:~{
 • ~s~^, nor ~}"
                            number number
                            (or x y layer) x y layer
                            number tilesets)
                    (return (values #x7f #(0 0 0 0 0 0) -1))))))

(defun assign-attributes (attr attr-table)
  "Assign attibutes ATTR into ATTR-TABLE, reusing existing entry if possible"
  (or (position attr attr-table :test #'equalp)
      (prog1 (length attr-table)
        (setf (cdr (last attr-table)) (cons attr nil)))))

(defun object-covers-tile-p (x y object &key tile-width)
  "Returns generally true if the OBJECT is over tile at X, Y"
  (let* ((obj-x1 (parse-number (assocdr "x" (second object))))
         (obj-y1 (parse-number (assocdr "y" (second object))))
         (obj-x2 (1- (+ obj-x1 (parse-number (or (assocdr "width" (second object) nil) "1")))))
         (obj-y2 (1- (+ obj-y1 (parse-number (or (assocdr "height" (second object) nil) "1")))))
         (cell-x1 (* x tile-width)) (cell-x2 (+ cell-x1 (1- tile-width)))
         (cell-y1 (* y 16)) (cell-y2 (+ cell-y1 15)))
    (and (<= cell-x1 obj-x2)
         (<= obj-x1  cell-x2)
         (<= cell-y1 obj-y2)
         (<= obj-y1  cell-y2))))

(defun find-effective-attributes (tileset x y objects attributes
                                  exits enemies &key tile-width)
  "Find the effective attributes for the tile X Y using TILESET, OBJECTS, ATTRIBUTES, EXITS and ENEMIES."
  (declare (ignore enemies)) ; TODO
  (let ((effective-objects (remove-if-not (lambda (el)
                                            (and (equal "object" (car el))
                                                 (object-covers-tile-p x y el :tile-width tile-width)))
                                          objects)))
    (dolist (object effective-objects)
      #+ () (format *trace-output* "~&Object covering ~d, ~d" x y)
      (setf attributes (add-attribute-values (tileset-palettes tileset)
                                             object attributes
                                             :exits exits :x x :y y
                                             :tile-width tile-width))))
  attributes)

(defun add-alt-tile-attributes (tile-attributes alt-tile-attributes)
  "Adds ALT-TILE-ATTRIBUTES into TILE-ATTRIBUTES"
  (let ((wall-bits (logand #x0f (aref alt-tile-attributes 0))))
    (setf (aref tile-attributes 0) (logior (ash wall-bits 4) (aref tile-attributes 0))
          (aref tile-attributes 1) (logior #x01 (aref tile-attributes 1)))))

(defun mark-palette-transitions (grid attributes-table)
  "On GRID, mark tiles who start a span with a new palette in ATTRIBUTES-TABLE"
  (dotimes (y (array-dimension grid 1))
    #+ () (terpri *trace-output*)
    (dotimes (x (array-dimension grid 0))
      #+ () (format *trace-output* "~d " (tile-effective-palette grid x y attributes-table))
      (if (zerop x)
          (setf (aref grid x y 0) (logior #x80 (aref grid x y 0)))
          (let ((palette-left (tile-effective-palette grid (1- x) y attributes-table))
                (palette-self (tile-effective-palette grid x y attributes-table)))
            (unless (= palette-left palette-self)
              (setf (aref grid x y 0) (logior #x80 (aref grid x y 0))))))))
  #+ () (terpri *trace-output*))

(defun properties->plist (properties.xml)
  "Convert properties from XML in PROPERTIES.XML into a plist of keyword→value pairs"
  (loop for (property alist) in (cddr properties.xml)
        for name = (cadr (assoc "name" alist :test #'equal))
        for value = (cadr (assoc "value" alist :test #'equal))
        appending (list (make-keyword (string-upcase name)) value)))

(defun ensure-minifont (string)
  "If STRING is empty, returns NIL; if a string, convert to minifont; if bytes, return untouched."
  (if (zerop (length string))
      nil
      (etypecase (elt string 0)
        (character (unicode->minifont string))
        ((unsigned-byte 8) string))))

(defun decal-properties->binary (object)
  "Convert the decal properties of OBJECT into a binary number; push into ENEMIES as needed"
  (remove-if
   #'null
   (list
    #x10 ; use decals page for art FIXME? — probably will be cleared by #109
    (when-let (text (tile-property-value "Text" object))
      (error "Update TMX file to use Script property rather than Text~%~s" object))
    (when-let (script (tile-property-value "Script" object))
      (error "Can't place Script on a Decal but someone tried~%~s" object))
    (when-let (push (tile-property-value "Push" object))
      (cond
        ((or (string-equal "Very Heavy" push)
             (string-equal "VeryHeavy" push))
         (ash 3 28))
        ((string-equal "Heavy" push) (ash 2 28))
        ((or (string-equal "0" push) (emptyp push)) 0)
        (t (ash 1 28))))
    (when (tile-property-value "Enemy" object)
      (error "Enemy detected on decal, should be spawn point."))
    (when-let (crowns$ (tile-property-value "Crowns" object))
      (let ((crowns (parse-integer crowns$)))
        (assert (< -127 crowns 127) (crowns)
                "Cannot gain/lose more than 127 crowns at once: requested ~:d crowns"
                crowns)
        (logior (ash 1 30) crowns)))
    (when-let (speed$ (tile-property-value "Speed" object))
      (let ((speed (parse-integer speed$)))
        (check-type speed (integer 0 7))
        (ash speed 24))))))

(defun logior-numbers (&rest list)
  "Reduce the LIST of numbers (ignoring NILs) using `LOGIOR'"
  (reduce #'logior (remove-if #'null (flatten list))))

(defun collect-decal-object (object enemies base-tileset decal-tileset &key (tile-width 8))
  (declare (ignore enemies)) ; TODO
  (let ((x (floor (parse-number (assocdr "x" (second object))) tile-width))
        (y (1- (floor (parse-number (assocdr "y" (second object))) 16)))
        (name (or (assocdr "name" (second object) nil) "(Unnamed decal)")))
    (when-let (gid$ (assocdr "gid" (second object) nil))
      (let ((gid (let ((n (parse-integer gid$)))
                   (assert (<= 0 n 1023) (n)
                           "GID of decal object is insane, got ~d ($~x) from “~a”"
                           n n gid$)
                   n))
            (type (or (assocdr "type" (second object) nil) "rug"))
            (decal-props (logior-numbers (decal-properties->binary object))))
        (multiple-value-bind (id attrs tileset) (find-tile-by-number gid base-tileset
                                                                     :decal-tileset decal-tileset)
          (declare (ignore attrs))
          (let ((tile-default-palette (aref (tileset-palettes
                                             (elt (list base-tileset
                                                        decal-tileset)
                                                  tileset))
                                            id)))
            #+ () (format *trace-output* "~2&//% tile $~2,'0x base palette is ~d"
                          id tile-default-palette)
            (setf decal-props (logior (logand decal-props #xfffff8ff)
                                      (ash tile-default-palette 24))))
          (when (plusp tileset)         ; XXX ref #109 ?
            (setf decal-props (logior decal-props #x1000)))
          (when-let (pal$ (assocdr "Palette" (second object) nil))
            #+ () (format *trace-output* "~2&//% tile $~2,'0x override palette ~s" id pal$)
            (assert (typep (parse-integer pal$) '(integer 0 7)) (pal$)
                    "Expected a palette index from 0 to 7, not ~s" pal$)
            (setf decal-props (logior (logand decal-props (logxor #xffffffff (ash 7 5)))
                                      (ash (parse-integer pal$) 8))))
          (cond
            ((string-equal type "rug"))
            ((string-equal type "ceiling"))
            ((string-equal type "decal"))
            (t (error "Unrecognized “type” for object named “~a”: “~a” is not valid"
                      name type)))
          (format *trace-output* "~&Decal @(~3d, ~3d) ($~1x~2,'0x) pal. ~d “~a”"
                  x y
                  (ash (logand #x1000 decal-props) -12) id
                  (logand #x07 (ash decal-props -24))
                  name)
          (return-from collect-decal-object (list x y id decal-props)))))))

(defun collect-invisible-decals-for-tile (x y objects &key tile-width)
  (loop for object in objects
        when (and (object-covers-tile-p x y object :tile-width tile-width)
                  (or (tile-property-value "Text" object)
                      (tile-property-value "Speech" object)))
          collect (list x y #xff (logior-numbers (decal-properties->binary object)))))

(defun parse-tile-grid (layers objects base-tileset decal-tileset &key tile-width)
  (let* ((ground (parse-layer (first layers)))
         (detail (and (= 2 (length layers))
                      (parse-layer (second layers))))
         (output (make-array (list (array-dimension ground 0)
                                   (array-dimension ground 1)
                                   2)
                             :element-type 'integer))
         (attributes-table (list #(0 0 0 0 0 0)))
         (exits-table (cons nil nil))
         (decals-table (cons nil nil))
         (enemies (make-array 0 :element-type 'cons :adjustable t :fill-pointer t)))
    (assert (<= 10 (array-dimension ground 1) 64) ()
            "The tile map must have from 10-64 (not ~:d) rows"
            (array-dimension ground 1))
    (assert (<= 20 (array-dimension ground 0) 127) ()
            "The tile map must have from 20-127 (not ~:d) columns"
            (array-dimension ground 0))
    (assert (> 1025 (* (array-dimension ground 0) (array-dimension ground 1))) ()
            "The tile map must have no more than 1,024 tiles, but this one is ~
~:d×~:d = ~:d tiles"
            (array-dimension ground 0) (array-dimension ground 1)
            (* (array-dimension ground 0) (array-dimension ground 1)))
    (dotimes (y (array-dimension ground 1))
      (dotimes (x (array-dimension ground 0))
        (let* ((detailp (and detail (> (aref detail x y) 0)))
               (tile-number (if detailp
                                (aref detail x y)
                                (aref ground x y))))
          (when (zerop tile-number)
            (warn "Tile number zero (blank) replaced with $7f at (~d, ~d) on ~:[ground~;detail~] layer."
                  x y detailp)
            (setf tile-number #x7f))
          (unless (<= 1 tile-number 128)
            (cerror "Continue, using tile $7f"
                    "Tile number is ridiculous, got ~d ($~x) at (~d, ~d) on ~:[ground~;detail~] layer."
                    tile-number tile-number x y detailp)
            (setf tile-number #x7f))
          (multiple-value-bind (tile-id tile-attributes)
              (find-tile-by-number tile-number base-tileset
                                   :x x :y y :layer (if detailp "detail" "ground"))
            (when detailp
              (multiple-value-bind (alt-tile-id alt-tile-attributes)
                  (find-tile-by-number (aref ground x y) base-tileset
                                       :x x :y y :layer "detail")
                (setf (aref tile-attributes 5) alt-tile-id)
                (add-alt-tile-attributes tile-attributes alt-tile-attributes)))
            (setf (aref output x y 0) tile-id
                  (aref output x y 1) (assign-attributes
                                       (find-effective-attributes base-tileset x y objects
                                                                  tile-attributes exits-table enemies
                                                                  :tile-width tile-width)
                                       attributes-table))
            (when-let (decals (collect-invisible-decals-for-tile x y objects
                                                                 :tile-width tile-width))
              (appendf decals-table decals))))))
    (dolist (object objects)
      (when-let (decal (collect-decal-object object enemies
                                             base-tileset decal-tileset
                                             :tile-width tile-width))
        (appendf decals-table (list decal))))
    (mark-palette-transitions output attributes-table)
    (values output
            attributes-table
            (rest decals-table)
            (rest exits-table)
            enemies)))

(defun map-layer-depth (layer.xml)
  "Look for properties in LAYER.XML to indicate if it is the ground (0) or detail (1) layer."
  (when (and (<= 3 (length layer.xml))
             (equal "properties" (car (third layer.xml))))
    (loop for prop in (subseq (third layer.xml) 2)
          when (and (equal "property" (car prop))
                    (equalp "ground" (assocdr "name" (second prop)))
                    (or (equalp "true" (assocdr "value" (second prop)))
                        (equalp "t" (assocdr "value" (second prop)))))
            return 0
          when (and (equal "property" (car prop))
                    (equalp "detail" (assocdr "name" (second prop)))
                    (or (equalp "true" (assocdr "value" (second prop)))
                        (equalp "t" (assocdr "value" (second prop)))))
            return 1)))

(defclass tileset ()
  ((gid :initarg :gid :reader tileset-gid)
   (pathname :initarg :pathname :reader tileset-pathname)
   (image :initarg :image :accessor tileset-image)
   (palettes :accessor tileset-palettes)
   (tile-attributes :accessor tile-attributes
                    :initform (make-array (list 128 6)
                                          :element-type '(unsigned-byte 8)))))

(defmethod print-object ((tileset tileset) stream)
  (format stream "#<TILESET from ~a, GIDs start ~d, length ~d>"
          (enough-namestring (tileset-pathname tileset))
          (tileset-gid tileset)
          (array-dimension (tile-attributes tileset) 0)))

(defun load-tileset-image (pathname$)
  "Load the “sprite sheet” image for a tile set from PATHNAME$"
  (format *trace-output* "~&Loading tileset image from “~a”"
          (enough-namestring pathname$))
  (unless (find #\/ pathname$)
    (setf pathname$ (concatenate 'string "Source/Art/Tiles/" pathname$)))
  (let* ((png (png-read:read-png-file
               (let ((pathname (parse-namestring pathname$)))
                 (make-pathname
                  :name (pathname-name pathname)
                  :type (pathname-type pathname)
                  :defaults #p"./Source/Maps/Tiles/"))))
         (height (png-read:height png))
         (width (png-read:width png))
         (α (png-read:transparency png))
         (*machine* 7800))
    (png->palette height width
                  (png-read:image-data png)
                  α)))

(defun extract-8×16-tiles (image)
  (let ((output (list)))
    (dotimes (row (floor (array-dimension image 1) 16))
      (dotimes (column (floor (array-dimension image 0) 8))
        (let ((tile (extract-region image (* column 8) (* row 16)
                                    (+ (* column 8) 7) (+ (* row 16) 15))))
          (assert (= 8 (array-dimension tile 0)))
          (assert (= 16 (array-dimension tile 1)))
          (push tile output))))
    (format *trace-output* "… found ~d tile~:p in ~d×~d image"
            (length output) (array-dimension image 0) (array-dimension image 1))
    (reverse output)))

(defun extract-palettes (image &key (count 8))
  (let* ((last-row (1- (array-dimension image 1)))
         (palette-strip (extract-region image 0 last-row (1- (* 4 count)) last-row))
         (palettes (make-array (list count 4) :element-type '(unsigned-byte 8))))
    (dotimes (p count)
      (dotimes (c 4)
        (setf (aref palettes p c) (if (zerop c)
                                      (aref palette-strip 0 0)
                                      (aref palette-strip (+ c (* p 4)) 0)))))
    palettes))

(defun all-colors-in-tile (tile)
  (destructuring-bind (width height) (array-dimensions tile)
    (remove-duplicates (loop for y below height
                             append
                             (loop for x below width
                                   collect (aref tile x y)))
                       :test #'=)))

(defun tile-fits-palette-p (tile palette)
  (every (lambda (c) (member c palette))
         (all-colors-in-tile tile)))

(defun 2a-to-list (2a)
  "Convert the two-dimensional array 2A into a list of its elements sequentially"
  (loop for row from 0 below (array-dimension 2a 0)
        collecting (loop
                     with output = (make-array (list (array-dimension 2a 1)))
                     for column from 0 below (array-dimension 2a 1)
                     do (setf (aref output column) (aref 2a row column))
                     finally (return output))))

(defun 2a-to-lol (2a)
  "Convert the two-dimensional array 2A into a list-of-lists"
  (loop for row from 0 below (array-dimension 2a 0)
        collecting (loop
                     for column from 0 below (array-dimension 2a 1)
                     collect (aref 2a row column))))

(defun region->list-of-colors (tile)
  "Extract the list of all colors found in the two-dimensional array TILE"
  (destructuring-bind (width height) (array-dimensions tile)
    (loop for y below height
          append
          (loop for x below width
                collect (aref tile x y)))))

(defun color-distance-by-indices (index0 index1
                                  &key (palette (machine-palette)))
  "Find the color distance between the colors with PALETTE indicies INDEX0 and INDEX1"
  (destructuring-bind (c0-r c0-g c0-b)
      (elt palette index0)
    (destructuring-bind (c1-r c1-g c1-b)
        (elt palette index1)
      (color-distance c0-r c0-g c0-b
                      (list c1-r c1-b c1-g)))))

(defun palette-to-ansi-pairs (pal)
  (if (listp pal)
      (mapcar (lambda (index) (list index (apply #'ansi-color-pixel
                                                 (palette->rgb index))))
              pal)
      (list pal (apply #'ansi-color-pixel (palette->rgb pal)))))

(defun best-palette (tile palettes &key allow-imperfect-p x y)
  (let ((palettes (mapcar (lambda (p) (coerce p 'list)) (2a-to-list palettes))))
    (or (position-if (lambda (palette)
                       (tile-fits-palette-p tile palette))
                     palettes)
        (when allow-imperfect-p
          (loop for palette in palettes
                for index from 0
                with machine-palette = (machine-palette)
                with best = nil
                with best-distance = most-positive-fixnum
                do (let ((distance
                           (reduce #'min
                                   (mapcar
                                    (lambda (color)
                                      (loop for pal-color in palette
                                            minimize
                                            (color-distance-by-indices
                                             pal-color color
                                             :palette machine-palette)))
                                    (region->list-of-colors tile)))))
                     (when (< distance best-distance)
                       (setf best index
                             best-distance distance)))
                finally (return best)))
        (cond
          ;; TODO make a proper error with presentation methods to handle this
          ((clim:extended-output-stream-p *trace-output*)
           (error "Tile could not fit any palette:~% Tile: ~s~% Palettes: ~s
All colors: ~s~@[~% at (~3d,~3d)~]"
                  tile palettes (all-colors-in-tile tile) x y))
          ((tty-xterm-p)
           (error "Tile could not fit any palette:~% Tile: ~a
 Palettes: ~{~%~5t~{~{$~2,'0x ~a~}~^, ~}~^;~45t~{~{$~2,'0x ~a~}~^, ~}~^; ~}
All colors: ~{~{$~2,'0x ~a~}~^, ~}~@[~% at (~3d, ~3d)~]"
                  (pixels-to-ansi-string tile)
                  (mapcar #'palette-to-ansi-pairs palettes)
                  (palette-to-ansi-pairs (all-colors-in-tile tile))
                  x y))
          (t (error "Tile could not fit any palette:~% Tile: ~s~% Palettes: ~s
All colors: ~s~@[~% at (~3d,~3d)~]"
                    tile palettes (all-colors-in-tile tile) x y))))))

(defun split-images-to-palettes (image)
  (let ((tiles (extract-8×16-tiles image))
        (width (floor (array-dimension image 0) 8))
        (palettes (extract-palettes image))
        (output (make-array '(128) :element-type '(unsigned-byte 3))))
    (dotimes (i (length tiles))
      (setf (aref output i) (best-palette (elt tiles i) palettes
                                          :x (mod i width) :y (floor i width))))
    (values output palettes)))

(defun tile-property-value (key tile.xml &key tile-width)
  (dolist (info (cddr tile.xml))
    (when (and (equal "properties" (car info)))
      (dolist (prop (cddr info))
        (when (and (equal "property" (car prop))
                   (equalp key (assocdr "name" (second prop))))
          (when-let (value (assocdr "value" (second prop)))
            #+ () (format *trace-output* "~&Property value set from ~s ⇒ ~s" prop value)
            (return-from tile-property-value
              (let ((value (string-trim #(#\Space) value)))
                (cond ((or (equalp "true" value)
                           (equalp "t" value)
                           (equalp "on" value))
                       t)
                      ((or (equalp "false" value)
                           (equalp "f" value)
                           (equalp "off" value))
                       :off)
                      (t value)))))))))
  nil)

(defun tile-collision-p (tile.xml test-x test-y)
  (unless (and tile.xml (< 1 (length tile.xml)))
    (return-from tile-collision-p nil))
  (dolist (object-group (xml-matches "objectgroup" tile.xml))
    (let ((objects (when (and object-group (< 1 (length object-group)))
                     (remove-if (lambda (el)
                                  (or (not (equal "object" (car el)))
                                      (when-let (type-name (assocdr "type"
                                                                    (second el) nil))
                                        (not (equalp "Wall" type-name)))))
                                (subseq object-group 2)))))
      (dolist (object objects)
        (let ((height (parse-number (assocdr "height" (second object))))
              (width (parse-number (assocdr "width" (second object))))
              (object-x (parse-number (assocdr "x" (second object))))
              (object-y (parse-number (assocdr "y" (second object)))))
          (when (and (<= object-x test-x (+ object-x width))
                     (<= object-y test-y (+ object-y height)))
            (return-from tile-collision-p t))))))
  nil)

(defun locale-pathname (locale)
  (let* ((parts (split-sequence #\/ locale))
         (parts (if (equal "Maps" (elt parts 0))
                    (mapcar #'pascal-case (subseq parts 1))
                    (mapcar #'pascal-case parts))))
    (make-pathname :name (last-elt parts)
                   :type "tmx"
                   :directory (append (list :relative "Source" "Maps")
                                      (mapcar #'pascal-case
                                              (subseq parts 0 (1- (length parts))))))))

(defun load-other-map (locale)
  (xmls:parse-to-list (alexandria:read-file-into-string
                       (locale-pathname locale))))

(defvar *map-cache* (make-hash-table :test 'equalp))

(defun locale-xml (locale-name)
  (destructuring-bind (locale-pathname-dir locale-pathname-name)
      (split-sequence #\/ locale-name)
    (or (gethash locale-pathname-name *map-cache*)
        (setf (gethash locale-pathname-name *map-cache*)
              (xmls:parse-to-list
               (alexandria:read-file-into-string
                (locale-pathname (concatenate 'string
                                              locale-pathname-dir
                                              "/"
                                              locale-pathname-name))))))))

(defvar *maps-ids* (make-hash-table :test 'equal))
(defvar *maps-display-names* (make-hash-table :test 'equal))
(defvar *maps-dock-ids* (make-hash-table :test 'equal))
(defvar *dock-ids-maps* (make-hash-table :test 'equal))

(defun read-map-ids-table (&optional (table #p"Source/Tables/MapsIndex.ods"))
  (format *trace-output* "~&Reading maps table from “~a”… " (enough-namestring table))
  (unless (hash-table-p *maps-ids*)
    (setf *maps-ids* (make-hash-table :test 'equal)
          *maps-display-names* (make-hash-table :test 'equal)
          *maps-dock-ids* (make-hash-table :test 'equal)
          *dock-ids-maps* (make-hash-table :test 'equal)))
  (let* ((page (ss->lol (first (read-ods-into-lists table)))))
    (dolist (row page)
      (destructuring-bind (&key island full-name display-name id dock-id
                           &allow-other-keys)
          row
        (when full-name
          (let ((segment-name
                  (remove #\_ (concatenate 'string
                                           (pascal-case (string island)) "/"
                                           (pascal-case (string full-name))))))
            (setf (gethash segment-name *maps-ids*) (parse-integer id)
                  (gethash segment-name *maps-display-names*)
                  (lower-case display-name))
            (when (not (emptyp dock-id))
              (setf (gethash segment-name *maps-dock-ids*) (parse-integer dock-id)
                    (gethash (parse-integer dock-id) *dock-ids-maps*) segment-name)))))))
  (format *trace-output* " … now I know about ~:d map~:p" (hash-table-count *maps-ids*)))

(defun find-locale-id-from-xml (xml)
  (tagbody top
     (restart-case
         (let ((id-prop (find-if (lambda (el)
                                   (some (lambda (kv)
                                           (destructuring-bind (key value) kv
                                             (and (equal key "name") (equalp value "id"))))
                                         (second el)))
                                 (xml-matches "property" (xml-match "properties" xml nil)))))
           (when *current-scene*
             (unless id-prop
               (when (or (null *maps-ids*) (zerop (hash-table-count *maps-ids*)))
                 (read-map-ids-table))
               (setf id-prop (gethash *current-scene* *maps-ids*))))
           (assert id-prop (id-prop) "Cannot find a locale ID property from map data or the maps index spreadsheet.
~@[Searched the properties of “~a”~%~]~
There should be an ID property on the map itself
\(Map → Map Properties, name: ID, type: int)
with the locale's unique ID, or an entry in Source/Tables/MapsIndex.ods.
To CONTINUE, look up the unique ID and provide it now,
or correct the TMX file (add the ID) and DO-OVER."
                   *current-scene*)
           (return-from find-locale-id-from-xml
             (etypecase id-prop
               (number id-prop)
               (cons (parse-integer (second (find-if (lambda (kv)
                                                       (destructuring-bind (key value) kv
                                                         (declare (ignore value))
                                                         (equal key "value")))
                                                     (second id-prop))))))))
       (reload-map-ids-table ()
         :report "Reload Source/Tables/MapsIndex.ods"
         (setf *maps-ids* nil)
         (go top)))))

(defun find-entrance-by-name (xml name locale-name)
  "Find entrance NAME in the map XML for  LOCALE-NAME"
  (tagbody top
     (restart-case
         (let ((locale-id (get-asset-id :map
                                        (format nil "~{~a~^/~}"
                                                (mapcar #'pascal-case
                                                        (split-sequence #\/ locale-name))))))
           (dolist (group (xml-matches "objectgroup" xml))
             (dolist (object (xml-matches "object" group))
               (when-let (properties (xml-match "properties" object nil))
                 (dolist (prop (xml-matches "property" properties))
                   (when (and (find-if (lambda (kv)
                                         (destructuring-bind (key value) kv
                                           (and (equalp key "value")
                                                (equalp (pascal-case value)
                                                        name))))
                                       (second prop))
                              (find-if (lambda (kv) (destructuring-bind (key value) kv
                                                      (and (equalp key "name")
                                                           (equalp value "Entrance"))))
                                       (second prop)))
                     (let ((x (floor (parse-number
                                      (second (find-if (lambda (kv)
                                                         (destructuring-bind (key value) kv
                                                           (declare (ignore value))
                                                           (equalp key "x")))
                                                       (second object))))
                                     (parse-integer (assocdr "tilewidth" (second xml)))))
                           (y (floor (parse-number
                                      (second (find-if (lambda (kv)
                                                         (destructuring-bind (key value) kv
                                                           (declare (ignore value))
                                                           (equalp key "y")))
                                                       (second object))))
                                     16)))
                       (return-from find-entrance-by-name (list locale-id x y))))))))
           (cerror "Place at (10, 10) for now"
                   "Can't link to non-existing “~a” point in locale “~a”
Update map/s or script to agree with one another and DO-OVER."
                   name locale-name)
           (list locale-id 10 10))
       (reload-map ()
         :report (lambda (s) (format s "Reload “~a” map" locale-name))
         (setf xml (load-other-map locale-name))
         (go top)))))

(defun assign-exit (locale point exits)
  (format *trace-output* "~&Searching locale “~a” for an entrance point “~a”…" locale point)
  (let ((locale.xml (load-other-map locale)))
    (destructuring-bind (locale-id x y)
        (find-entrance-by-name locale.xml point locale)
      (format *trace-output* " Found at (~3d, ~3d)." x y)
      (or (position (list locale-id x y) exits :test #'equalp)
          (progn
            (setf (cdr (last exits)) (cons (list locale-id x y) nil))
            (1- (length exits)))))))

(defvar *enemies-by-name* (make-hash-table :test #'equalp))

(defun load-enemies-index (&optional
                             (index-pathname #p"Source/Generated/Enemies.index"))
  (with-input-from-file (index index-pathname)
    (loop for line = (read-line index nil nil)
          while line
          do (when (not (emptyp line))
               (let ((id (parse-integer (subseq line 0 2) :radix 16))
                     (name (subseq line 2)))
                 (setf (gethash name *enemies-by-name*) id))))
    (format *trace-output* "Loaded Enemies index (~r enem~@:p) from ~a"
            (hash-table-count *enemies-by-name*)
            (enough-namestring index-pathname))))

(defun find-enemy-id (name)
  (when (zerop (hash-table-count *enemies-by-name*))
    (load-enemies-index))
  (format *trace-output* "~%Enemy “~a” (index $~2,'0x)"
          name (gethash name *enemies-by-name*))
  (or (gethash name *enemies-by-name*)
      (error "No enemy named “~a” could be found" name)))

(defun ensure-number (n)
  (etypecase n
    (number n)
    (string (parse-number n))))

(defun add-attribute-values (tile-palettes xml bytes
                             &key
                               (exits nil exits-provided-p)
                               tile-id
                               tile-width
                             &allow-other-keys)
  (labels ((set-bit (byte bit)
             (setf (elt bytes byte) (logior (elt bytes byte) bit)))
           (clear-bit (byte bit)
             (setf (elt bytes byte) (logand (elt bytes byte) (logxor #xff bit))))
           (map-boolean (property byte bit)
             (when-let (value (tile-property-value property xml :tile-width tile-width))
               (cond ((eql t value) (set-bit byte bit))
                     ((eql :off value) (clear-bit byte bit))
                     (t (warn "Unrecognized value ~s for property ~s" value property))))))
    
    (when (tile-collision-p xml 4 0) (set-bit 0 #x01))
    (when (tile-collision-p xml 4 15) (set-bit 0 #x02))
    (when (tile-collision-p xml 0 7) (set-bit 0 #x04))
    (when (tile-collision-p xml 7 7) (set-bit 0 #x08))
    (map-boolean "Wall" 0 #x0f)
    (map-boolean "WallNorth" 0 #x01)
    (map-boolean "WallSouth" 0 #x02)
    (map-boolean "WallWest" 0 #x04)
    (map-boolean "WallEast" 0 #x08)
    ;; Ceiling → #x01 set by details layer
    (map-boolean "Wade" 1 #x02)
    (map-boolean "Swim" 1 #x04)
    (map-boolean "Ladder" 1 #x08)
    (map-boolean "Climb" 1 #x08)
    (map-boolean "Pit" 1 #x10)
    (map-boolean "Door" 1 #x20)
    (map-boolean "Flammable" 1 #x40)
    (map-boolean "StairsDown" 1 #x80)
    (map-boolean "Ice" 2 #x01)
    (map-boolean "Fire" 2 #x02)
    (when-let (switch (tile-property-value "Trigger" xml))
      (cond
        ((equalp switch "Step") (set-bit 2 #x04))
        ((equalp switch "Hit") (set-bit 2 #x08))
        ((equalp switch "Push") (set-bit 2 #x0c))
        (t (warn "Unknown value for switch Trigger property: ~s" switch))))
    (map-boolean "Iron" 2 #x10)
    #+ (or)
    (when-let (push (tile-property-value "Push" xml))
      (cond
        ((equalp push "Heavy") (set-bit 2 #x40))
        ((equalp push "VeryHeavy") (set-bit 2 #x60))
        (t (set-bit 2 #x20))))
    (when-let (destination (tile-property-value "Exit" xml))
      (set-bit 2 #x80)
      (let* ((dest (split-sequence #\/ destination))
             (locale (format nil "~{~a~^/~}" (butlast dest)))
             (point (last-elt dest)))
        (if exits-provided-p
            (set-bit 4 (logand #x1f (assign-exit locale point exits)))
            (warn "Exit in tileset data is not supported (to point “~a” in locale “~a”)" point locale))))
    (if-let (lock (tile-property-value "Lock" xml))
      (set-bit 3 (logand #x1f (parse-integer lock :radix 16)))
      (when (tile-property-value "Locked" xml)
        (warn "Locked tile without Lock code")))
    (if-let (switch (tile-property-value "Switch" xml))
      (set-bit 4 (ash (logand #x03 (parse-integer switch :radix 16)) 3)))
    (when-let (tile-id (or tile-id (assocdr "id" (second xml) nil)))
      (when (and tile-palettes tile-id (zerop (logand (ash 7 5) (elt bytes 4))))
        (clear-bit 4 (ash 7 5))
        (set-bit 4 (ash (mod (aref tile-palettes (ensure-number tile-id)) 8) 5))))
    (when-let (palette (tile-property-value "Palette" xml))
      #+ () (format *trace-output* "~&//* Hey, look, changing the palette to ~d because of~%~10t~s"
                    palette xml)
      (clear-bit 4 (ash 7 5))
      (set-bit 4 (ash (mod (parse-integer palette) 8) 5)))
    bytes))

(defun parse-tile-attributes (palettes xml i)
  (let ((bytes (make-array '(6) :element-type '(unsigned-byte 8)))
        (tile.xml (find-if (lambda (el)
                             (and (equal "tile" (car el))
                                  (= i (parse-integer (assocdr "id" (second el))))))
                           (subseq xml 2))))
    ;; returns bytes
    (add-attribute-values palettes tile.xml bytes :tile-id i)))

(defun tile-effective-palette (grid x y attributes-table)
  (the (unsigned-byte 4)
       (ash (logand (ash 7 5)
                    (aref (elt attributes-table (aref grid x y 1)) 4))
                    -5)))

(defun load-tileset (xml-reference &optional relative-path)
  (let* ((path (etypecase xml-reference
                 (cons (let ((source
                               (assocdr "source"
                                        (second xml-reference))))
                         (split-sequence #\/ source)))
                 (pathname (split-sequence #\/ (namestring xml-reference)))
                 (string (split-sequence #\/ xml-reference))))
         (pathname (merge-pathnames
                    (make-pathname
                     :directory (cons :relative
                                      (subseq path 0 (1- (length path))))
                     :name (subseq (last-elt path)
                                   0
                                   (position #\. (last-elt path) :from-end t))
                     :type (subseq (last-elt path)
                                   (1+ (position #\. (last-elt path) :from-end t))))
                    (or relative-path #p"./")))
         (gid (if (consp xml-reference)
                  (parse-integer (assocdr "firstgid" (second xml-reference)))
                  0))
         (xml (xmls:parse-to-list (alexandria:read-file-into-string pathname)))
         (tileset (make-instance 'tileset :gid gid :pathname pathname)))
    (format *trace-output* "~&Loading tileset data from ~a" (enough-namestring pathname))
    (assert (equal "tileset" (first xml)))
    (assert (member (assocdr "tilecount" (second xml)) '("64" "128") :test 'string-equal))
    (let* ((image (xml-match "image" xml))
           (image-data (load-tileset-image (assocdr "source" (second image))))
           (palette-data (split-images-to-palettes image-data)))
      (setf (tileset-image tileset) image-data
            (tileset-palettes tileset) palette-data)
      (dotimes (i (parse-integer (assocdr "tilecount" (second xml))))
        (let ((bytes (parse-tile-attributes palette-data xml i)))
          (dotimes (b 6)
            (setf (aref (tile-attributes tileset) i b) (elt bytes b))))))
    tileset))

(defun make-byte-array-with-fill-pointer (&optional original-array)
  (if original-array
      (if (array-has-fill-pointer-p original-array)
          original-array
          (make-array (array-dimensions original-array)
                      :fill-pointer t :adjustable t
                      :element-type '(unsigned-byte 8)
                      :initial-contents (coerce original-array 'list)))
      (make-array (list 0) :fill-pointer t :adjustable t
                           :element-type '(unsigned-byte 8))))

(defun rle-encode (non-repeated repeated repeated-times)
  (let ((repeated (make-byte-array-with-fill-pointer repeated))
        (non-repeated (make-byte-array-with-fill-pointer non-repeated)))
    (when (> (length non-repeated) 127)
      (return-from rle-encode
        (reduce (lambda (a b) (concatenate 'vector a b))
                (append (mapcar (lambda (segment)
                                  (rle-encode segment
                                              (make-byte-array-with-fill-pointer)
                                              0))
                                (loop for start from 0 by 127
                                      while (< start (length non-repeated))
                                      collecting (subseq non-repeated
                                                         start
                                                         (min (+ 127 start)
                                                              (length non-repeated)))))
                        (list (rle-encode (make-byte-array-with-fill-pointer)
                                          repeated repeated-times))))))
    (let ((output (make-byte-array-with-fill-pointer)))
      (when (plusp (length non-repeated))
        (vector-push-extend (1- (length non-repeated)) output)
        (dotimes (byte (length non-repeated))
          (vector-push-extend (aref non-repeated byte) output)))
      (when (plusp (length repeated))
        (vector-push-extend (logior #x80 (1- (length repeated))) output)
        (vector-push-extend (1- repeated-times) output)
        (dotimes (byte (length repeated))
          (vector-push-extend (aref repeated byte) output)))
      output)))

(defun rle-expanded-string (rle)
  (let ((output (make-byte-array-with-fill-pointer))
        (offset 0))
    (loop while (< offset (1- (length rle)))
          for string-length = (1+ (logand #x7f (aref rle offset)))
          do (if (zerop (logand #x80 (aref rle offset)))
                 (progn (loop for byte across (subseq rle (1+ offset)
                                                      (+ 1 offset string-length))
                              do (vector-push-extend byte output))
                        (incf offset (1+ string-length)))
                 (progn (dotimes (i (1+ (aref rle (1+ offset))))
                          (loop for byte across (subseq rle (+ 2 offset)
                                                        (+ 2 offset string-length))
                                do (vector-push-extend byte output)))
                        (incf offset (+ 2 string-length)))))
    output))

(defun rle-compress-segment (source)
  (when (< (length source) 4)
    (return-from rle-compress-segment
      (list (cons (rle-encode source (make-byte-array-with-fill-pointer) 0)
                  (length source)))))
  (let ((matches (list)))
    (lparallel:pdotimes (offset (min 127 (1- (length source))))
      (loop for length from (min 127 (- (length source) offset)) downto 1
            for first-part = (subseq source offset (+ offset length))
            do (loop for repeats
                     from (min 256 (floor (/ (- (length source) offset) length)))
                     downto (if (= 1 length) 3 2)
                     do (when (every (lambda (part) (equalp first-part part))
                                     (loop for i from 0 below repeats
                                           for n = (* i length)
                                           collect (subseq source (+ offset n)
                                                           (+ offset length n))))
                          (push (cons (rle-encode
                                       (subseq source 0 offset)
                                       (subseq source offset (+ offset length))
                                       repeats)
                                      (+ offset (* length repeats)))
                                matches)))))
    (incf *rle-options* (or (and matches (length matches))
                            1))
    (or matches
        (list (cons (rle-encode source (make-byte-array-with-fill-pointer) 0)
                    (length source))))))

(defun shorter (a b)
  (if (< (length a) (length b))
      a b))

(defun only-best-options (options)
  (let ((best-expanded-length (make-hash-table))
        (best-rle (make-hash-table)))
    (dolist (option options)
      (destructuring-bind (rle . expanded-length) option
        (let* ((length (length rle))
               (champion (gethash length best-expanded-length)))
          (if (or (null champion)
                  (> expanded-length length))
              (setf (gethash length best-expanded-length) expanded-length
                    (gethash length best-rle) rle)))))
    (let ((best-options
            (loop for length being each hash-key of best-expanded-length
                  collecting (cons (gethash length best-rle)
                                   (gethash length best-expanded-length)))))
      (if *rle-fast-mode*
          (if (> (length best-options) *rle-fast-mode*)
              (subseq (sort best-options
                            (lambda (a b)
                              (< (/ (length (car a)) (cdr a))
                                 (/ (length (car b)) (cdr b)))))
                      0 *rle-fast-mode*)
              best-options)
          best-options))))

(defun rle-compress-fully (source &optional recursive-p)
  (let ((total-length (length source))
        (options (only-best-options (rle-compress-segment source)))
        (fully (list)))
    (when (< 1 (length options))
      #+ (or)
      (format t "~& For source length ~:d, there are ~:d options with expanded-length from ~:d to ~:d bytes"
              (length source)
              (length options)
              (reduce #'min (mapcar #'cdr options))
              (reduce #'max (mapcar #'cdr options))))
    (dolist (option options)
      (destructuring-bind (rle . expanded-length) option
        (when (zerop (random 1000))
          (format *trace-output* "~&(RLE compressor: ~:d segment options considered)" *rle-options*))
        (cond
          ((and (not recursive-p) (> (length rle) *rle-best-full*))
           ;; no op, drop that option
           )
          ((= expanded-length total-length)
           (push rle fully))
          (t
           (let ((rest (rle-compress-fully (subseq source expanded-length) t)))
             (when rest
               (push (concatenate 'vector rle rest) fully)))))))
    (when fully
      (reduce #'shorter fully))))

(defparameter *rle-fast-mode* 1)

(defvar *rle-options* 0)

(defvar *rle-best-full* most-positive-fixnum)

(defun rle-compress (source)
  (let ((lparallel:*kernel* (lparallel:make-kernel
                             8 :name "Skyline-Tool RLE Compressor"))
        (*rle-options* 0)
        (*rle-best-full* (1+ (length source))))
    (format *trace-output* "~& Compressing ~:d byte~:p …" (length source))
    (finish-output *trace-output*)
    (unwind-protect
         (let ((rle (rle-compress-fully source nil)))
           (format *trace-output* " into ~:d byte~:p using RLE (-~d%), ~
after considering ~:d option~:p."
                   (length rle)
                   (round (- 100 (* 100 (/ (length rle) (length source)))))
                   *rle-options*)
           (if (> (length rle) (1+ (length source)))
               (prog1
                   (concatenate 'vector #(#xff) source)
                 (format *trace-output* "~&(Compression failed, saving uncompressed)"))
               rle))
      (lparallel:end-kernel))))

(defun hex-dump-comment (string)
  (format t "~{~&     ;; ~
~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~
~^  ~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~
~^  ~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~
~^  ~2,'0x~^ ~2,'0x~^ ~2,'0x~^ ~2,'0x~}"
          (coerce string 'list)))

(defun hex-dump-bytes (string &optional (stream t))
  (format stream "~{~&     .byte $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~
~^,   $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~}"
          (coerce string 'list)))

(defun xml-match (element xml &optional (error-code nil error-code-p))
  (or (find-if (lambda (el) (equal element (car el)))
               (subseq xml 2))
      (unless error-code-p
        (error "Not found: expected element “~a” but ~:[there are no child elements of “~a”~;~
only see elements: ~:*~{“~a”~^, ~} under “~a”.~]"
               element (mapcar #'car (subseq xml 2)) (car xml)))
      error-code))

(defun xml-matches (element xml)
  (when (and xml (< 1 (length xml)))
    (remove-if-not (lambda (el) (equal element (car el)))
                   (subseq xml 2))))

(defun write-word (number stream)
  (assert (<= 0 number #xffff) (number)
          "Cannot write 16-bit word-sized data with number #x~x (~:d); ~
range is 0 - #xffff (65,535)"
          number number)
  (write-byte (logand #x00ff number) stream)
  (write-byte (ash (logand #xff00 number) -8) stream))

(defun write-dword (number stream)
  (assert (<= 0 number #xffffffff) (number)
          "Cannot write 32-bit double-word-sized data with number #x~x (~:d); ~
range is 0 - #xffffffff (4,294,967,295)"
          number number)
  (write-byte (logand #x000000ff number) stream)
  (write-byte (ash (logand #x0000ff00 number) -8) stream)
  (write-byte (ash (logand #x00ff0000 number) -16) stream)
  (write-byte (ash (logand #xff000000 number) -24) stream))

(defun write-bytes (sequence stream)
  (loop for byte across (coerce sequence 'vector)
        do (write-byte byte stream)))

(define-constant +minifont-punctuation+
  " ,.?!/&+-×÷=“”’;:…@❓‘♪©•↑↓←→áâàäāãçčđéêèëēíîìïīłñóôòöōõŕšúûùüūþæœýÿøå¿¡«»ß()000°ªﬁ0ąężćńśź0"
  :test 'string=)

(defun char->minifont (char)
  (cond
    ((or (char<= #\0 char #\9)
         (char<= #\a char #\z)
         (char<= #\A char #\Z))
     (digit-char-p char 36))
    ((char= #\apostrophe char)
     (if-let (n #.(position #\’ +minifont-punctuation+ :test #'char=))
       (+ 36 n)
       (error "I hate apostrophes, really.")))
    (t (if-let (pos (position (char-downcase char) +minifont-punctuation+ :test #'char=))
         (+ 36 pos)
         (error "Cannot encode character “~:c” (~a) in minifont"
                char (sentence-case (char-name char)))))))

(defun minifont->char (byte &key replace)
  (unless replace
    (check-type byte (integer 0 127) "a minifont character value (0-127)"))
  (cond
    ((<= 0 byte 35) (format nil "~36r" byte))
    ((or (< byte 0) (> byte 127)) (string replace))
    ((= #x70 byte) "I’")
    ((= #x71 byte) "ll")
    ((= #x72 byte) "’r")
    ((= #x75 byte) "li")
    ((= #x76 byte) "fi")
    ((= #x77 byte) "’s")
    (t (string (elt +minifont-punctuation+ (- byte 36))))))

(defun unicode->minifont (string)
  (let ((mini-string (make-array (length string) :element-type '(unsigned-byte 8))))
    (loop for i below (length string)
          do (setf (aref mini-string i) (char->minifont (aref string i))))
    mini-string))

(defun minifont->unicode (string &key replace)
  (apply #'concatenate 'string
         (loop for i below (length string)
               collecting (minifont->char (aref string i) :replace replace))))

(defun decal-invisible-p (decal)
  (= #xff (elt decal 2)))

(defun assemble-binary (source-pathname)
  (let (#+ () (combined-source-pathname
                (make-pathname :directory (append (list "Source" "Generated")
                                                  (subseq (pathname-directory source-pathname) 1))
                               :defaults source-pathname)))
    (cerror "Run Commands are not implemented properly yet! Pushing just an RTS for ~a"
            (enough-namestring source-pathname))
    #(#x60) ; rts
    ))

(defun run-commands-content-for-map (pathname)
  (let ((run-commands-pathname (make-pathname :defaults pathname
                                              :type "s")))
    (when (probe-file run-commands-pathname)
      (assemble-binary run-commands-pathname))))

(defun tileset-rom-bank (xml)
  ;; FIXME #125
  (loop for (string id)
          on '("SandyIsland" 5
               "Indoor" 6
               "JungleIsland" 7
               "Ancient" 8
               "Mechanism" 9
               "Cityscape" #xa
               "Arturos" #xb
               "Shipboard" #xc
               "Undersea" #xd)
        by #'cddr
        when (some (lambda (match)
                     (search string (assocdr "source" (second match))))
                   (xml-matches "tileset" xml))
          do (return id)))

(defun write-binary-animations-list (animations-list s &key frame-rate)
  #+ () (format *trace-output* "~%WRITE-BINARY-ANIMATIONS-LIST: ~2%~s~2%" animations-list)
  (dolist (animation animations-list)
    (vector-push-extend 0 s)
    (loop for (frame duration) on animation by #'cddr
          do (vector-push-extend (* 2 frame) s)))
  (vector-push-extend 0 s)
  (dolist (animation animations-list)
    (vector-push-extend 0 s)
    (loop for (frame duration) on animation by #'cddr
          do (vector-push-extend (round (* duration frame-rate)) s)))
  (vector-push-extend 0 s)
  (vector-push-extend 0 s))

(defun map-data-vector (&key width height tile-grid attributes-table
                             exits-table animations-list decals-animations-list tv)
  (let ((s (make-byte-array-with-fill-pointer))
        (frame-rate (ecase tv (:ntsc 1/60) (:pal 1/50))))
    (adjust-array s #x1000)
    ;; map tile graphics
    (dotimes (y height)
      (dotimes (x width)
        (vector-push-extend (aref tile-grid x y 0) s)))
    ;; map tile attribute set indicator
    (setf (fill-pointer s) #x400)
    (dotimes (y height)
      (dotimes (x width)
        (vector-push-extend (aref tile-grid x y 1) s)))
    ;; attributes list
    (setf (fill-pointer s) #x800)
    (assert (every (lambda (attr) (= 6 (length attr)))
                   attributes-table)
            (attributes-table)
            "All attributes table entries must be precisely 6 bytes: ~%~s"
            attributes-table)
    (dolist (attr attributes-table)
      (dovector (byte attr)
        (vector-push-extend byte s))
      (assert (< (fill-pointer s) #xc00) ()
              "Overflow (to ~:d byte~:p) in attributes table when trying to add ~s (from among ~:d attribute~:p)"
              (fill-pointer s) attr (length attributes-table)))
    
    ;; exits list
    (setf (fill-pointer s) #xc00)
    (dolist (exit exits-table)
      (dolist (byte exit)               ; map/locale asset ID, x, y
        (vector-push-extend byte s))
      (assert (< (fill-pointer s) #xe00)))
    ;; animations list
    (setf (fill-pointer s) #xe00)
    (write-binary-animations-list animations-list s :frame-rate frame-rate)
    (write-binary-animations-list decals-animations-list s :frame-rate frame-rate)
    s))

(defun compile-map (pathname)
  (format *trace-output* "~&Loading tile map from ~a" (enough-namestring pathname))
  (read-map-ids-table)
  (let ((canon-name (format nil "~a.~a"
                            (lastcar (pathname-directory pathname))
                            (pathname-name pathname)))
        (xml (xmls:parse-to-list (alexandria:read-file-into-string pathname))))
    (assert (equal "map" (car xml)) ()
            "The XML header does not appear to be for a tiled map (TMX) file")
    (assert (equal "orthogonal" (assocdr "orientation" (second xml))) ()
            "The map file must be in orthogonal orientation")
    (assert (equal "right-down" (assocdr "renderorder" (second xml))) ()
            "The map file must be in right-down render order")
    (assert (member (assocdr "tilewidth" (second xml)) '("8" "16") :test #'string-equal) ()
            "The map file must have 8px or 16px wide tiles")
    (assert (equal "16" (assocdr "tileheight" (second xml))) ()
            "The map file must have 16px high tiles")
    (let* ((tilesets (mapcar (lambda (tileset) (load-tileset tileset pathname))
                             (xml-matches "tileset" xml)))
           (layers (xml-matches "layer" xml))
           (tile-width (parse-integer (assocdr "tilewidth" (second xml))))
           (object-groups (xml-matches "objectgroup" xml))
           (animations-list (parse-tile-animation-set (first tilesets)))
           (decals-animations-list (when (rest tilesets)
                                     (apply #'parse-tile-animation-set (rest tilesets)))))
      (assert (<= 1 (length layers) 2) ()
              "This tool requires 1-2 tile layers, found ~:d tile map layer~:p in ~a"
              (length layers) pathname)
      (when (= 2 (length layers))
        (when (or (and (null (map-layer-depth (first layers)))
                       (eql 0 (map-layer-depth (second layers))))
                  (and (null (map-layer-depth (second layers)))
                       (eql 1 (map-layer-depth (first layers))))
                  (and (map-layer-depth (first layers))
                       (map-layer-depth (second layers))
                       (> (map-layer-depth (first layers))
                          (map-layer-depth (second layers)))))
          (setf layers (reversef layers))))
      (assert (<= 0 (length object-groups) 1) ()
              "This tool requires only one object group (layer), found ~:d object groups"
              (length object-groups))
      (let ((base-tileset (first tilesets))
            (decal-tileset (when (<= 2 (length tilesets))
                             (second tilesets)))
            (objects (cddr (first object-groups))))
        (when (< 2 (length tilesets))
          (warn "Ignoring tilesets after the second: ~{~a~^, ~}" tilesets))
        (format *trace-output* "~&Parsing map layers…")
        (multiple-value-bind (tile-grid
                              attributes-table decals-table
                              exits-table enemies-list)
            (parse-tile-grid layers objects base-tileset decal-tileset :tile-width tile-width)
          (dolist (tv '(:ntsc :pal))
            (format *trace-output* "~&About to write map ~a for ~a… "
                    (title-case canon-name) tv)
            (let* ((width (array-dimension tile-grid 0))
                   (height (array-dimension tile-grid 1))
                   (display-name (or
                                  (gethash (substitute #\/ #\. canon-name) *maps-display-names*)
                                  (error "Can't figure out the display name for ~a" canon-name)))
                   (name (subseq display-name 0 (min 20 (length display-name))))
                   (compressed-map-data
                     (zx7-compress
                      (map-data-vector :width width :height height
                                       :tile-grid tile-grid
                                       :attributes-table attributes-table
                                       :exits-table exits-table
                                       :animations-list animations-list
                                       :decals-animations-list decals-animations-list
                                       :tv tv)
                      :base-name (concatenate 'string "Map."
                                              canon-name
                                              ".Data."
                                              (string-upcase tv))))
                   (run-commands-content (run-commands-content-for-map pathname)))
              (assert (<= (* width height) 1024))
              (format *trace-output* "~2&Found grid of ~d×~d tiles, with ~
~r unique attribute~:p, ~r decal~:p (~r invisible), ~r unique exit~:p, ~
~r distinct animation~:p, and ~r unique enem~@:p."
                      width height
                      (length attributes-table)
                      (length decals-table)
                      (count-if #'decal-invisible-p decals-table)
                      (length exits-table)
                      (length animations-list)
                      (length enemies-list))
              (format *trace-output* "~&Ready to write binary output for ~a … " tv)
              (force-output *trace-output*)
              (let ((outfile (make-pathname
                              :name (format nil "Map.~a.~a.~a"
                                            (last-elt (pathname-directory pathname))
                                            (pathname-name pathname) tv)
                              :directory '(:relative "Object" "Assets")
                              :type "o"))
                    (offset 0))
                (ensure-directories-exist outfile)
                (with-output-to-file (object outfile :element-type '(unsigned-byte 8)
                                                     :if-exists :supersede)
                  ;; offset 0, width
                  (write-byte width object)
                  ;; offset 1, height
                  (write-byte height object)
                  ;; offset 2-3, offset of compressed RAM map data
                  (write-word (setf offset (+ 20 1 (length name)))
                              object)
                  ;; offset 4-5, unused now
                  (write-word 0 object)
                  ;; offset 6-7, unused now
                  (write-word 0 object)
                  ;; offset 8-9, offset of decals list
                  (write-word (incf offset (length compressed-map-data))
                              object)
                  ;; offset 10-11, unused now
                  (write-word 0 object)
                  ;; offset 12-13, unused now
                  (write-word 0 object)
                  ;; offset 14-15, offset of enemies list — not yet implemented
                  (write-word 0 object)
                  ;; offset 16, tileset ROM bank
                  (write-byte (tileset-rom-bank xml) object)
                  ;; offset 17, future
                  (write-byte 0 object)
                  ;; offset 18-19, run-commands pointer
                  (if run-commands-content
                      (write-word (incf offset (+ 1 (array-total-size enemies-list))) object)
                      (write-word 0 object))
                  ;; offset 20, name (Pascal string)
                  (write-byte (length (unicode->minifont name)) object)
                  (write-bytes (unicode->minifont name) object)
                  ;; compressed art map
                  (write-bytes compressed-map-data object)
                  (format *trace-output* "Wrote compressed map data … ")
                  (force-output *trace-output*)
                  ;; decals list
                  (write-byte (length decals-table) object)
                  (assert (every (lambda (decal)
                                   (= 4 (length decal)))
                                 decals-table)
                          (decals-table)
                          "All decals table entries must be precisely 4 values: ~%~s"
                          decals-table)
                  (dolist (decal decals-table)
                    ;; x, y, gid of first art
                    (write-bytes (subseq decal 0 3) object)
                    ;; attributes
                    (write-dword (fourth decal) object)
                    #+ ()
                    (format *trace-output*
                            "~&~{ • Decal at ~d, ~d gid $~2,'0x attributes $~8,'0x~}"
                            (coerce decal 'list)))
                  ;; enemies list
                  (write-byte (length enemies-list) object)
                  (write-bytes run-commands-content object)
                  (loop for enemy across enemies-list
                        do (write-bytes enemy object))
                  (format *trace-output* " end of file at $~4,'0x … "
                          (file-position object))
                  (force-output *trace-output*)))
              (format *trace-output* "done."))))))))

(defun rip-tiles-from-tileset (tileset images &optional (start-i 0))
  (let ((i start-i))
    (dotimes (y (floor (array-dimension (tileset-image tileset) 1) 16))
      (dotimes (x (floor (array-dimension (tileset-image tileset) 0) 8))
        (setf (aref images i)
              (extract-region (tileset-image tileset)
                              (* x 8) (* y 16)
                              (1- (* (1+ x) 8)) (1- (* (1+ y) 16))))
        (incf i)))))

(defun palette-index (pixel palette)
  (position pixel (coerce palette 'list)))

(defun rip-bytes-from-image (image palettes bytes index &key x y)
  (let ((palette (elt (2a-to-list palettes)
                      (best-palette image palettes :x x :y y))))
    (dotimes (y 16)
      (dotimes (half 2)
        (let ((byte-index (+ (+ half (* 2 index)) (* y #x100))))
          (check-type byte-index (integer 0 (4096)))
          (dotimes (x 4)
            (setf (ldb (byte 2 (* 2 x)) (aref bytes byte-index))
                  (palette-index (aref image
                                       (+ (- 3 x) (* 4 half))
                                       (- 15 y))
                                 palette))))))))

(defun compile-tileset (pathname &optional common-pathname)
  (let ((*machine* 7800)
        (outfile (make-pathname :directory '(:relative "Object" "Assets")
                                :name (format nil "Tileset.~a" (pathname-name pathname))
                                :type "o")))
    (ensure-directories-exist outfile)
    (let* ((tileset (load-tileset pathname))
           (width (floor (array-dimension (tileset-image tileset) 0) 8))
           (palettes (extract-palettes (tileset-image tileset)))
           (images (make-array (list 128)))
           (bytes (make-array (list (* 256 16)) :element-type '(unsigned-byte 8))))
      (rip-tiles-from-tileset tileset images)
      (when common-pathname
        (rip-tiles-from-tileset (load-tileset common-pathname) images 64))
      (dotimes (i 128)
        (rip-bytes-from-image (aref images i) palettes bytes i
                              :x (mod i width) :y (floor i width)))
      (with-output-to-file (object outfile
                                   :element-type '(unsigned-byte 8)
                                   :if-exists :supersede)
        (write-bytes bytes object)))))

(defun ensure-byte (number)
  (coerce (round number) '(unsigned-byte 8)))

(defun darken-color-in-palette (color)
  (destructuring-bind (r g b) (palette->rgb color)
    (destructuring-bind (h s v) (multiple-value-list (dufy:rgb-to-hsv r g b))
      (let ((s* (* s 3/4))
            (v* (* v 1/2) ))
        (apply #'rgb->palette (mapcar #'ensure-byte
                                      (multiple-value-list (dufy:hsv-to-rgb h s* v*))))))))

(defun lighten-color-in-palette (color)
  (destructuring-bind (r g b) (palette->rgb color)
    (destructuring-bind (h s v) (multiple-value-list (dufy:rgb-to-hsv r g b))
      (let ((s* (* s 3/4))
            (v* (+ v (* 1/2 (- 255 v)))))
        (apply #'rgb->palette (mapcar #'ensure-byte
                                      (multiple-value-list (dufy:hsv-to-rgb h s* v*))))))))

(defun redden-color-in-palette (color)
  (destructuring-bind (r g b) (palette->rgb color)
    (let ((r* (min #xff (* r 4/3)))
          (g* (min #xff (* g 2/3)))
          (b* (min #xff (* b 2/3))))
      (apply #'rgb->palette (mapcar #'ensure-byte (list r* g* b*))))))

(defun cyanate-color-in-palette (color)
  (destructuring-bind (r g b) (palette->rgb color)
    (let ((r* (min #xff (* r 2/3)))
          (g* (min #xff (* g 4/3)))
          (b* (min #xff (* b 4/3))))
      (apply #'rgb->palette (mapcar #'ensure-byte (list r* g* b*))))))

(defun adjust-palettes (adjust-color-function palettes)
  (let ((adjusted-palettes (make-array (list 8 4) :element-type '(unsigned-byte 8))))
    (dotimes (palette-index 8)
      (dotimes (color-index 4)
        (let ((target-color (funcall adjust-color-function (aref palettes palette-index color-index))))
          (dotimes (pal* palette-index)
            (dotimes (col* 4)
              (when (and (= target-color (aref adjusted-palettes pal* col*))
                         (/= (aref palettes palette-index color-index)
                             (aref palettes pal* col*)))
                (setf target-color (mod (+ target-color #x10) #x100)))))
          (setf (aref adjusted-palettes palette-index color-index)
                target-color))))
    adjusted-palettes))

(defun extract-tileset-palette (pathname outfile)
  (ensure-directories-exist outfile)
  (with-output-to-file (output outfile :if-exists :supersede)
    (flet ((dump-palettes (series label)
             (format *trace-output* "~%~10a:" label)
             (dotimes (p 8)
               (dotimes (c 4)
                 (print-wide-pixel (aref series p c) *trace-output*)))
             (format output "~%~10t;; ~a:~%~12t.byte ~a ; Background"
                     label
                     (atari-colu-string (aref series 0 0)))
             (dotimes (palette-index 8)
               (format output "~%~12t.byte ~a, ~a, ~a"
                       (atari-colu-string (aref series palette-index 1))
                       (atari-colu-string (aref series palette-index 2))
                       (atari-colu-string (aref series palette-index 3))))))
      (let* ((tileset (load-tileset pathname)))
        (format output ";;; Palette ~a~%;;; extracted from ~a"
                (enough-namestring outfile) (enough-namestring pathname))
        (dolist (*region* '(:ntsc :pal))
          (let ((palettes (extract-palettes (tileset-image tileset))))
            (format *trace-output* "~% ~a:~%" (enough-namestring outfile))
            (format output "~2%~10t.if TV == ~a" *region*)
            (dump-palettes palettes "Base")
            (dump-palettes (adjust-palettes #'darken-color-in-palette palettes) "Dark")
            (dump-palettes (adjust-palettes #'lighten-color-in-palette palettes) "Light")
            (dump-palettes (adjust-palettes #'redden-color-in-palette palettes) "Red")
            (dump-palettes (adjust-palettes #'cyanate-color-in-palette palettes) "Cyan")
            (format output "~%~10t.fi~%")))))))

(defun find-named-object-in-scene (name-object &optional (scene-name *current-scene*))
  (dolist (match (xml-matches "object" (xml-match "objectgroup" (locale-xml scene-name))))
    (when-let (this-name (ignore-errors (assocdr "name" (cadr match))))
      (when (string-equal this-name name-object)
        (return-from find-named-object-in-scene
          (list :x (parse-number (assocdr "x" (cadr match)))
                :y (parse-number (assocdr "y" (cadr match)))
                :gid (parse-integer (assocdr "gid" (cadr match))))))))
  (error "Can't find “~a” in scene “~a”" name-object scene-name))
