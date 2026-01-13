(in-package :skyline-tool)

(defvar *tileset*)


(defvar *tileset*)

                          :element-type '(unsigned-byte 8))))
    (loop for x from left to right
          do (loop for y from top to bottom
                   do (setf (aref copy (- x left) (- y top)) (aref original x y))))
    copy))

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

(defun tile->bits (tile)
  (do-collect (y to 7)
    (reduce #'logior
            (loop for x from 0 to 7
                  collecting (if (zerop (aref tile x y))
                                 0
                                 (expt 2 (- 7 x)))))))

(defun tile->colour (tile)
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

(defun image-colours (palette-image
                      &optional
                        (height (array-dimension palette-image 1))
                        (width (array-dimension palette-image 0)))
  "Return the set of distinct colors in use in the paletted image"
  (remove-duplicates
   (remove-if #'null
              (loop for y from 0 to (1- height)
                    appending (loop for x from 0 to (1- width)
                                    collecting (aref palette-image x y))))))

(defun mob-colours (mob)
  (image-colours mob 21 24))

(defun ensure-monochrome (mob)
  (let ((all-colours (mob-colours mob)))
    (unless (= 1 (length all-colours))
      (warn "MOB data is hi-res and not monochrome (using ~D; saw ~{~D~^, ~})"
            (car all-colours) all-colours))
    (code-char (car all-colours))))

(defun ensure-1+chrome (mob)
  (let ((all-colours (remove-if (rcurry #'member '(9 10))
                                (mob-colours mob))))
    (unless (or (null all-colours)
                (= 1 (length all-colours)))
      (warn "MOB data has more than 1 distinct colour after brown & orange ~
\(using ~D; saw ~{~D~^, ~})"
            (car all-colours) all-colours))
    (code-char (logior #x80 (or (car all-colours) 0)))))

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

(defun tia-player-interpret/strip (pixels)
  (let ((shape nil)
        (colors nil))
    (loop for row from 0 below (second (array-dimensions pixels))
          do (push (reduce #'logior
                           (loop for bit from 0 to 7
                                 collect (if (plusp (aref pixels bit row))
                                             (expt 2 (- 7 bit))
                                             0)))
                   shape)
          do (push (or
                    (first
                     (remove-if #'null (loop for bit from 0 to 7
                                             for color = (aref pixels bit row)
                                             collect (when (plusp color)
                                                       color))))
                    0)
                   colors))
    (values (reverse shape) (reverse colors))))

(defun tia-player-interpret (pixels)
  (loop
    with shapes
    with colors
    for x from 0 below (/ (array-dimension pixels 0) 8)
    do (multiple-value-bind (shape color)
           (tia-player-interpret/strip
            (copy-rect pixels
                       (* 8 x) 0
                       8 (array-dimension pixels 1)))
         (appendf shapes shape)
         (appendf colors color))
    finally (return (values shapes colors))))

(defun try-to-maintain-palette (new old &optional (overall old))
  (if (or (null old) (emptyp old))
      (if overall
          (return-from try-to-maintain-palette
            (try-to-maintain-palette new overall))
          (return-from try-to-maintain-palette new)))
  (assert (= (length new) (length old)))
  (let ((offer
          (loop with palette = (copy-list old)
                with introductions = (loop for color in (remove-duplicates new)
                                           when (not (member color old))
                                             collect color)
                for i from 0 below (length new)
                do (unless (or (and (member (elt old i) new)
                                    (if (plusp i)
                                        (not (member (elt old i) (subseq palette 0 i)))
                                        t)))
                     (if (and (member (elt overall i) new)
                              (if (plusp i)
                                  (not (member (elt overall i) (subseq palette 0 i)))
                                  t))
                         (setf (elt palette i) (elt overall i))
                         (setf (elt palette i) (pop introductions))))
                finally (return palette))))
    (assert (every (lambda (color) (member color offer)) new)
            (offer) "The offered palette of (~{$~2,'0x~^ ~}) did not contain colors in (~{$~2,'0x~^ ~})
(tried to preserve palette indices from (~{$~2,'0x~^ ~})~@[ or (~{$~2,'0x~^ ~})~]" offer new old)
    offer))

(assert (equalp '(0 1 2 3) (try-to-maintain-palette '(3 2 1 0) '(0 1 2 3))))
(assert (equalp '(0 1 4 3) (try-to-maintain-palette '(3 4 1 0) '(0 1 2 3))))
(assert (equalp '(0 6 7 8) (try-to-maintain-palette '(6 0 7 8) '(0 1 2 3))))
(assert (equalp '(5 6 7 8) (try-to-maintain-palette '(5 6 7 8) '(0 1 2 3))))

(assert (equalp '(5 6 7 8) (try-to-maintain-palette '(6 7 8 5) '(0 1 2 3) '(5 6 7 8))))

(assert (equalp '(0 1 2 3) (try-to-maintain-palette '(3 2 1 0) nil '(0 1 2 3))))
(assert (equalp '(0 1 4 3) (try-to-maintain-palette '(3 4 1 0) nil '(0 1 2 3))))
(assert (equalp '(0 6 7 8) (try-to-maintain-palette '(6 0 7 8) nil '(0 1 2 3))))
(assert (equalp '(5 6 7 8) (try-to-maintain-palette '(5 6 7 8) nil '(0 1 2 3))))

(defun mode-e-row-bytes (pixels &key last-row-palette y overall-palette
                                     enforce-overall-palette-p)
  (check-type pixels array)
  (assert (= 1 (array-dimension pixels 1)))
  (let ((shape nil)
        (palette (if enforce-overall-palette-p
                     overall-palette
                     (try-to-maintain-palette
                      (most-popular-colors pixels (array-dimension pixels 0) 1
                                           :count 4)
                      last-row-palette
                      overall-palette))))
    (assert (= (ceiling (array-dimension pixels 0) 4)
               (length (group-into-4 (coerce (pixels-into-palette pixels palette
                                                                  :y0 y :best-fit-p t)
                                             'list)))))
    (dolist (pixels (group-into-4 (coerce (pixels-into-palette pixels palette
                                                               :y0 y :best-fit-p t)
                                          'list)))
      (push (logior (ash (elt pixels 0) 6)
                    (ash (elt pixels 1) 4)
                    (ash (elt pixels 2) 2)
                    (elt pixels 3))
            shape))
    (assert (= (ceiling (array-dimension pixels 0) 4) (length shape)))
    (values (reverse shape) palette)))

(defun mode-e-interpret (pixels &key base-palette (color-per-line-p t))
  (loop with shapes
        with colors
        with last-palette = nil
        with overall-palette = (or (when color-per-line-p base-palette)
                                   (try-to-maintain-palette
                                    (most-popular-colors pixels
                                                         (array-dimension pixels 0)
                                                         (array-dimension pixels 1)
                                                         :count 4)
                                    base-palette))
        for y from 0 below (array-dimension pixels 1)
        do (multiple-value-bind (shape palette)
               (mode-e-row-bytes (copy-rect pixels 0 y (array-dimension pixels 0) 1)
                                 :last-row-palette last-palette :y y
                                 :overall-palette overall-palette
                                 :enforce-overall-palette-p (not color-per-line-p))
             (assert (= (length palette) 4))
             (assert (= (length shape) (ceiling (array-dimension pixels 0) 4)))
             (setf last-palette (if color-per-line-p
                                    palette
                                    overall-palette))
             (appendf shapes shape)
             (push palette colors)
             (assert (= (length shapes) (* 1/4 (array-dimension pixels 0) (1+ y)))))
        finally (return (values shapes
                                (mapcar (lambda (line)
                                          (mapcar (lambda (i) (when i (* 2 i))) line))
                                        (if color-per-line-p
                                            (reverse colors)
                                            (list overall-palette)))))))

(defun 48px-array-to-bytes (pixels)
  (do-collect (column below 6)
    (do-collect (row downfrom (1- (array-dimension pixels 1)) to 0)
      (reduce #'logior
              (do-collect (bit below 8)
                (if (plusp (aref pixels (+ bit (* column 8)) row))
                    (expt 2 (- 7 bit))
                    0))))))

(defun tia-48px-interpret (pixels)
  (let ((shape (48px-array-to-bytes pixels))
        (colors nil))
    (loop for row from 0 below (second (array-dimensions pixels))
          do (push (or (first
                        (remove-if #'null
                                   (loop for bit from 0 to 7
                                         for color = (aref pixels bit row)
                                         collect (when (plusp color)
                                                   color))))
                       0)
                   colors))
    (values shape (reverse colors))))

(defun bits-to-art (byte)
  (check-type byte string)
  (assert (= 8 (length byte)))
  (assert (every (lambda (char) (member char '(#\0 #\1))) byte))
  (substitute #\⬜ #\0
              (substitute #\⬛ #\1
                          (make-array 8
                                      :element-type 'character
                                      :initial-contents byte))))

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

(defun tia-48px-preview (image-pixels)
  (let ((shape (48px-array-to-bytes image-pixels))
        (height (second (array-dimensions image-pixels))))
    (loop for row from (1- height) downto 0
          for row-bytes = (do-collect (column below 6)
                            (elt (elt shape column) row))
          collecting (reduce (curry #'concatenate 'string)
                             (mapcar #'bits-to-art (mapcar
                                                    (curry #'format nil "~8,'0b")
                                                    row-bytes))))))

(defun pathname-base-name (pathname)
  (subseq (pathname-name pathname)
          0 (position #\. (pathname-name pathname))))

(define-constant +atari-ntsc-color-names+
    '(COLGRAY COLYELLOW COLBROWN COLORANGE COLRED COLMAGENTA
      COLPURPLE COLINDIGO COLBLUE COLTURQUOISE COLCYAN COLTEAL
      COLSEAFOAM COLGREEN COLSPRINGGREEN COLGOLD)
  :test 'equalp)

(define-constant +atari-pal-color-names+ ; FIXME: #1241 these are the NTSC ones
    '(COLGREY COLSPINACH COLGOLD COLORANGE
      COLRED COLMAGENTA COLVIOLET COLPURPLE
      COLINDIGO COLBLUE COLSTONEWASH COLTURQUOISE
      COLGREEN COLSEAFOAM COLSPRINGGREEN COLALGAE)
  :test 'equalp)

(defun atari-color-name (index &optional (tv *region*))
  (elt (ecase tv
         (:ntsc +atari-ntsc-color-names+)
         (:pal +atari-pal-color-names+)
         (:secam +vcs-secam-color-names+))
       index))

(defun atari-colu (byte &optional (tv *region*))
  (if (null byte)
      (list (atari-color-name 0 tv) 15)
      (let ((co (ash (logand byte #xf0) -4))
            (lu (logand byte #x0f)))
        (list (atari-color-name co tv) lu))))

(defun atari-colu-string (byte)
  (destructuring-bind (co lu) (atari-colu byte)
    (unless co
      (warn "Atari Color code ~s is not valid in region ~a, using fallback" (ash (logand byte #xf0) -4) *region*)
      (setf co "Hue0"))  ; Fallback color name
    (unless (and (integerp lu) (<= 0 lu 15))
      (warn "Atari Luminance value ~s is not valid, using fallback" lu)
      (setf lu 8))  ; Fallback luminance
    (format nil "CoLu(~a, $~x)" co lu)))

(defun atari-colu-run (&rest _)
  (error "unimplemented: ~s" _))

(defun compile-tia-48px (png-file out-dir height image-pixels)
  (let ((out-file-name (merge-pathnames
                        (make-pathname :name
                                       (pathname-name png-file)
                                       :type "s")
                        out-dir)))
    (format *trace-output* "~% Ripping TIA 48px graphics from 48×~D image"
            height)
    (ensure-directories-exist out-file-name)
    (with-output-to-file (source-file out-file-name
                                      :if-exists :supersede)
      (multiple-value-bind (shape colors) (tia-48px-interpret image-pixels)
        (format source-file ";;; -*- fundamental -*-
;;; Compiled sprite data from ~a
;;; Edit the original (probably Source/Art/~:*~a.png), editing this file is futile.

;;; Bitmap preview:
~{~%;;;   ~a~}
~a:	.block
 Height = ~d
 Width = 48
Shape:~{~{~a~}~2%~}
;CoLu:~{~%	.byte ~{~a~^ ~}~}
 .bend
"
                (pathname-name png-file)
                (tia-48px-preview image-pixels)
                (assembler-label-name (pathname-base-name png-file))
                height
                (mapcar (curry #'mapcar #'byte-and-art) shape)
                (mapcar (lambda (palette)
                          (mapcar #'atari-colu-string palette))
                        colors)))
      (format *trace-output* "~% Done writing to ~A" out-file-name))))

(defun reverse-7-or-8 (shape)
  (let* ((height (length shape))
         (group-height (if (zerop (mod height 7)) 7 8)))
    (loop for group from 0 below height by group-height
          append (loop for line from (1- group-height) downto 0
                       collecting (elt shape (+ group line))))))

(defun reverse-16 (shape)
  (let* ((height (length shape))
         (group-height 16))
    (loop for group from 0 below height by group-height
          append (loop for line from (1- group-height) downto 0
                       collecting (elt shape (+ group line))))))

(defun rows-of-width (bytes pixels-wide &key (pixels-per-byte 4))
  (loop
    with row-bytes = (let ((bytes-wide (/ pixels-wide pixels-per-byte)))
                       (check-type bytes-wide (integer 0 319))
                       bytes-wide)
    with output = (make-array (list row-bytes
                                    (ceiling (length bytes) row-bytes))
                              :element-type '(unsigned-byte 8))
    for i from 0 below (length bytes)
    for column = (mod i row-bytes)
    for row = (floor i row-bytes)
    for byte = (elt bytes i)
    do (setf (aref output column row) byte)
    finally (return (loop for y from 0 below (array-dimension output 1)
                          collecting (loop for x from 0 below (array-dimension output 0)
                                           collecting (aref output x y))))))

(defun make-fillable-vector (list)
  (let ((vector (make-array (length list) :fill-pointer t :adjustable t
                                          :element-type '(unsigned-byte 8))))
    (loop for i from 0
          for element in list
          do (setf (aref vector i) element)
          finally (return vector))))

(defun zx7-compress (bytes
                     &key (base-name (string (gensym "ZX7CompressTemp-"))))
  (format *trace-output* "~&Calling external compressor: ")
  (finish-output *trace-output*)
  (let ((output (let ((*standard-output* *trace-output*)
                      (*error-output* *trace-output*)
                      (bin-pathname (make-pathname :name base-name
                                                   :type "bin"
                                                   :directory '(:relative "Object" "Assets")))
                      (zx7-pathname (make-pathname :name base-name
                                                   :type "zx7"
                                                   :directory '(:relative "Object" "Assets"))))
                  (ensure-directories-exist bin-pathname)
                  (ensure-directories-exist zx7-pathname)
                  (write-byte-vector-into-file bytes bin-pathname :if-exists :overwrite
                                                                  :if-does-not-exist :create)
                  (uiop:run-program (format nil "./bin/zx7mini ~a ~a"
                                            (namestring bin-pathname)
                                            (namestring zx7-pathname))
                                    :output t :error-output t)
                  (with-input-from-file (zx7 zx7-pathname :element-type '(unsigned-byte 8))
                    (read-stream-content-into-byte-vector zx7)))))
    (format *trace-output* "… compression complete, new length ~:d bytes (-~5f%)"
            (length output) (- 100.0 (* 100.0 (/ (length output) (length bytes)))))
    output))

(defun compile-5200-mode-e-bitmap (image-pixels &key png-file
                                                     target-dir
                                                     (height (array-dimension image-pixels 1))
                                                     (width (array-dimension image-pixels 0))
                                                     (compressp (< 512 (* height width)))
                                                     (color-per-line-p t)
                                                     base-palette)
  (let ((out-file-name (merge-pathnames
                        (make-pathname :name
                                       (pathname-name png-file)
                                       :type "s")
                        target-dir)))
    (format *trace-output* "~% Ripping Mode ~a pixmap graphics from ~D×~D image…"
            (if (< height 97) "D/E" "E")
            width height)
    (finish-output *trace-output*)
    (with-output-to-file (source-file out-file-name :if-exists :supersede)
      (assert (= height (array-dimension image-pixels 1)))
      (assert (= width (array-dimension image-pixels 0)))
      (multiple-value-bind (shape colors) (mode-e-interpret image-pixels
                                                            :base-palette base-palette
                                                            :color-per-line-p color-per-line-p)
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
~{~%~10t.byte $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~^,  $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~}
CoLu:
~{~%~10t;; ~{~15a ~15a ~15a ~15a~}~}
~{~%~10t.byte $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~^,  $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~}
 .bend
"
                (enough-namestring png-file)
                (enough-namestring (make-pathname :defaults png-file :type "xcf"))
                (assembler-label-name (pathname-name png-file))
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
      (format *trace-output* "~% Done writing to ~A" out-file-name))))

(defun compile-gtia-player (png-file out-dir
                            height width image-pixels)
  (let ((out-file-name (merge-pathnames
                        (make-pathname :name
                                       (pathname-name png-file)
                                       :type "s")
                        out-dir)))
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
      (format *trace-output* "~% Done writing to ~A" out-file-name))))

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
  (declare (ignore))
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   out-dir)))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; -*- asm -*-
;;; Generated file; editing is useless. Source is ~a (derived from the matching XCF)
;;;

VIC2Font:
"
              png-file)
      (let ((colour (loop for char from 0 below (* (/ height 8) (/ width 8))
                          for x-cell = (mod (* char 8) width)
                          for y-cell = (* 8 (floor (* char 8) width))
                          for char-data = (extract-region image-nybbles
                                                          x-cell y-cell
                                                          (+ 7 x-cell) (+ 7 y-cell))
                          do (format src-file
                                     "~%	;; 		 character ~d ($~:*~x)~{~a~}"
                                     char
                                     (map 'list #'byte-and-art
                                          (tile->bits char-data)))
                          collect (tile->colour char-data))))
        (format *error-output* "~% Wrote binary font (monochrome) data to ~A." out-file))
      (finish-output src-file))))

(defun tile-cell-vic2-x (cell width)
  "Each tile's data is arranged into four cells, like so:

 0 1
 2 3

This gives the X position of the top-left corner of a 16×16 pixel tile
cell (where the cell's number is (+ (* tile 4) cell)) within an image
of the given width."
  (mod (+ (* (floor cell 4) 16)
          (* (mod cell 2) 8))
       width))

(defun tile-cell-vic2-y (cell width)
  (+ (* (floor (floor cell 4) (floor width 16)) 16)
     ;; even cells are on alternate rows
     (* (mod cell 2) 8)))

;;; Unit tests. This actually took me a while to get right!

(dotimes (i 62)
  (assert (= (tile-cell-vic2-y (* i 4) 16) (* 16 i)) nil
          "Tile ~D in 16px image should start at ~D, but TILE-CELL-VIC2-Y reports ~D"
          i (* 16 i) (tile-cell-vic2-y (* 4 i) 16)))

(loop for width in '(16 32 64 128)
      do (dotimes (i #xff)
           (assert (> (/ 16384 width) (tile-cell-vic2-y i width))
                   nil "The TILE-CELL-VIC2-Y function must return a valid value;
value ~D for tile-cell ~D is too far down for an image with width ~D" (tile-cell-vic2-y i width) i width)))

(defun compile-atari-8×8 (png-file target-dir height width)
  (let ((out-file (merge-pathnames
                   (make-pathname :name
                                  (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; This is a generated file. Editing is futile.~2%")
      (loop for x1 from 0 below width by 8
            for y1 from 0 below height by 8
            for i from 0
            do (loop for y0 from 7 downto 0
                     do (format src-file "~t.byte %~0,8b" 0))))))

(defun compile-tileset-64 (png-file out-dir height width image-nybbles)
  (declare (ignore height))
  (let ((out-file (merge-pathnames
                   (make-pathname :name
                                  (concatenate 'string "tiles."
                                               (pathname-name png-file))
                                  :type "s")
                   out-dir)))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (let ((colour (loop for cell from 0 to #xff
                          for x-cell = (tile-cell-vic2-x cell width)
                          for y-cell = (tile-cell-vic2-y cell width)
                          for tile-data = (extract-region image-nybbles x-cell y-cell (+ 7 x-cell) (+ 7 y-cell))
                          do (format src-file "~{~a~}"
                                     (map 'list #'bytes-and-art (tile->bits tile-data)))
                          collect (tile->colour tile-data))))

        (format *error-output* "~% Tileset with multiple colours found")
        (loop for cell in colour
              for i from 0 upto #xff
              do (cond
                   ((null cell) (princ #\NUL src-file))
                   ((null (cdr cell)) (princ (code-char (car cell)) src-file))
                   (t (princ (code-char (car cell)) src-file)
                      (warn "Tile ~D (~:*$~2,'0X) cell at (~:D×~:D) uses colours: ~{~D, ~D~}; using ~D"
                            (floor i 4) (floor i 4)
                            (tile-cell-vic2-x i width) (tile-cell-vic2-y i width)
                            cell (car cell)))))
        (format *error-output* "~% Wrote binary tileset data to ~A." out-file)))))

#+ (or)
(defun compile-tileset (png-file out-dir height width image-nybbles)
  (case *machine*
    ((64 128) (compile-tileset-64 png-file out-dir height width image-nybbles))
    (otherwise (error "Tile set compiler not set up yet for ~a" (machine-long-name)))))

(defun monochrome-lines-p (palette-pixels height width)
  (every
   #'identity
   (loop for row from 0 below height
         for colors = (remove-duplicates
                       (remove-if
                        #'zerop
                        (loop for column from 0 below width
                              collect (aref palette-pixels column row)))
                       :test #'=)
         collect (or (null colors)
                     (= 1 (length colors))))))

(defgeneric dispatch-png% (machine png-file target-dir
                           png height width α palette-pixels))

#+mcclim
(defmethod dispatch-png% :before (machine png-file target-dir
                                  png height width α palette-pixels)
  (when (clim:extended-output-stream-p *trace-output*)
    (clim:formatting-table (*trace-output*)
      (clim:formatting-row (*trace-output*)
        (clim:formatting-cell (*trace-output*)
          (clim:with-text-face (*trace-output* :bold)
            (princ "PNG file: " *trace-output*))
          (clim:present png-file 'pathname :stream *trace-output*)))
      (clim:formatting-row (*trace-output*)
        (clim:formatting-cell (*trace-output*)
          (clim:draw-pattern*
           *trace-output*
           (clim:make-pattern-from-bitmap-file png-file
                                               :format :png)
           0 0))))))

(defun monochrome-image-p (palette-pixels)
  (> 3 (length (image-colours palette-pixels))))

(defmethod dispatch-png% ((machine (eql 2600)) png-file target-dir
                          png height width α palette-pixels)
  (let ((monochrome-lines-p (monochrome-lines-p palette-pixels height width)))
    (cond
      ((and (zerop (mod height 5))
            (zerop (mod width 4))
            (= 48 (* (/ height 5) (/ width 4)))
            (monochrome-image-p palette-pixels))
       (format *trace-output* "~% Image ~A seems to be a font" png-file)
       (compile-font-8×8 png-file target-dir height width palette-pixels))

      ((and (= width 48))
       (format *trace-output* "~% Image ~a seems to be a 48px ~
 “high-resolution” bitmap"
               png-file)
       (compile-tia-48px png-file target-dir height palette-pixels))

      ((and (zerop (mod height 7))
            (zerop (mod width 4))
            (< 10 (* (/ height 7) (/ width 4)))
            monochrome-lines-p)
       (format *trace-output* "~% Image ~A seems to be a tileset" png-file)
       (compile-tileset png-file))

      ((and (zerop (mod width 8))
            (or (zerop (mod height 7))
                (zerop (mod height 8))))
       (format *trace-output* "~% Image ~A seems to be sprite (player) data"
               png-file)
       #+ () (compile-tia-player png-file target-dir height width palette-pixels))

      ((and (zerop (mod width 8))
            (zerop (mod height 8)))
       (format *trace-output* "~% Image ~A seems to be Atari 8×8 tiles" png-file)
       (compile-atari-8×8 png-file target-dir height width))

      (t (error "Don't know how to deal with image with dimensions ~
~:D×~:D pixels ~:[with~;without~] monochrome lines"
                width height monochrome-lines-p)))))

(defmethod dispatch-png% ((machine (eql 5200)) png-file target-dir
                          png height width α palette-pixels)
  (let ((monochrome-lines-p (monochrome-lines-p palette-pixels height width)))
    (cond
      ((and (= 256 width) (= 16 height))
       (format *trace-output* "~% Image ~a seems to be a ~d×~dpx Mode D skybox art"
               png-file width height)
       (compile-5200-mode-e-bitmap palette-pixels
                                   :png-file png-file
                                   :target-dir target-dir
                                   :height height
                                   :width width
                                   :compressp nil
                                   :color-per-line-p nil))
      ((= width 160)
       (format *trace-output* "~% Image ~a seems to be a full-screen (playfield) pixmap, assuming Mode D/E"
               png-file)
       (compile-5200-mode-e-bitmap palette-pixels
                                   :png-file png-file
                                   :target-dir target-dir
                                   :height height
                                   :width width
                                   :compressp t
                                   :base-palette '(0 7 27 83)))
      ((and (= width 12) (zerop (mod height 12)))
       (format *trace-output* "~% Image ~a seems to be 12×12 icons, assuming Mode D/E"
               png-file)
       (compile-5200-mode-e-bitmap palette-pixels
                                   :png-file png-file
                                   :target-dir target-dir
                                   :height height
                                   :width width
                                   :compressp nil
                                   :color-per-line-p nil
                                   :base-palette '(0 27 83 7)))
      ((and (= width 256) (zerop (mod height 64)))
       (format *trace-output* "~% Image ~a seems to be 64×64 icons, assuming Mode D/E"
               png-file)
       (compile-5200-mode-e-bitmap palette-pixels
                                   :png-file png-file
                                   :target-dir target-dir
                                   :height height
                                   :width width
                                   :compressp t
                                   :color-per-line-p nil))
      ((zerop (mod width 8))
       (format *trace-output* "~% Image ~A seems to be sprite (player) data"
               png-file)
       (compile-gtia-player png-file target-dir height width palette-pixels))

      (t (error "Don't know how to deal with image with dimensions ~
~:D×~:D pixels ~:[with~;without~] monochrome lines"
                width height monochrome-lines-p)))))

(defmethod dispatch-png% ((machine (eql 20)) png-file target-dir
                          png height width α palette-pixels)
  (assert (and (zerop (mod height 8))
               (zerop (mod width 8))
               (member (* (/ height 8) (/ width 8)) '(64 128 256))
               (monochrome-image-p palette-pixels)))
  (format *trace-output* "~% Image ~A seems to be a VIC-20 8×8 font" png-file)
  (compile-font-8×8 png-file target-dir height width palette-pixels))

(defmethod dispatch-png% ((machine (eql 64)) png-file target-dir
                          png height width α palette-pixels)
  (cond
    ((and (zerop (mod height 8))
          (zerop (mod width 8))
          (= 256 (* (/ height 8) (/ width 8)))
          (monochrome-image-p palette-pixels))
     (format *trace-output* "~% Image ~A seems to be a font" png-file)
     (compile-font-8×8 png-file target-dir height width palette-pixels))

    ((and (zerop (mod height 16))
          (zerop (mod width 16))
          (>= 64 (* (/ height 16) (/ width 16))))
     (format *trace-output* "~% Image ~A seems to be a tileset" png-file)
     (compile-tileset png-file))

    ((and (zerop (mod height 21))
          (zerop (mod width 24)))
     (format *trace-output* "~% Image ~A seems to be sprite MOB data" png-file)
     (compile-mob png-file target-dir height width palette-pixels))

    (t (error "Don't know how to deal with image with dimensions ~:D×~:D pixels"
              width height))))

(defun dispatch-png (png-file target-dir)
  (with-simple-restart (retry-png "Retry processing PNG file ~a" png-file)
    (format *trace-output* "~%Reading PNG image ~a…" png-file)
    (force-output *trace-output*)
    (let* ((png (png-read:read-png-file png-file))
           (height (png-read:height png))
           (width (png-read:width png))
           (α (png-read:transparency png))
           (palette-pixels (png->palette height width
                                         (png-read:image-data png)
                                         α)))
      (dispatch-png% *machine* png-file target-dir
                     png height width α palette-pixels))))

(defun write-7800-binary (index-out bytes-lists)
  (with-output-to-file (binary index-out
                               :element-type '(unsigned-byte 8)
                               :if-exists :supersede)
    (let ((page-length (length (first bytes-lists))))
      (unless (<= 0 page-length #x100)
        (error "Page length is nonsense, must be 0-256 ($0-$100) but got ~:d ($~:*~x)" page-length))
      (format *trace-output* "~&~A: Writing ~:D pages, each of which is ~:D bytes (out of 256 possible)~
~@[, last section has ~:D bytes free though~]; total file size should be ~:d ($~:*~x) byte~:p…"
              index-out (* (floor page-length #x100)
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
  "Interleave and reverse bytes"
  (loop for j below (apply #'max (mapcar #'length bytes-lists))
        collect (loop for i from (1- (length bytes-lists)) downto 0
                      collect (if (< j (length (elt bytes-lists i)))
                                  (elt (elt bytes-lists i) j)
                                  0))))

(defgeneric parse-7800-object (mode png &key width height palette))

(defun extract-regions (pixels width height)
  "Split PIXELS into regions of WIDTH×HEIGHT"
  (let ((images (list)))
    (dotimes (y (floor (/ (array-dimension pixels 1) height)))
      (dotimes (x (/ (array-dimension pixels 0) width))
        (push (extract-region pixels
                              (* x width) (* y height)
                              (1- (* (1+ x) width))
                              (1- (* (1+ y) height)))
              images)))
    (reverse images)))

(define-condition color-not-in-palette-error (error)
  ((x :initarg :x :reader color-not-in-palette-x)
   (y :initarg :y :reader color-not-in-palette-y)
   (i :initarg :i :reader color-not-in-palette-i)
   (image :initarg :image :reader color-not-in-palette-image)
   (pixel :initarg :pixel :reader color-not-in-palette-pixel)
   (palette :initarg :palette :reader color-not-in-palette-palette)
   (image-pixels :initarg :image-pixels :reader color-not-in-palette-image-pixels)))

(defmethod print-object ((c color-not-in-palette-error) s)
  (format s "The color found in the image data was not found in the palette.
At ~d, ~d at index ~d ~@[in image ~s~]
Pixel color: $~2,'0x
Palette contains these colors: ~{$~2,'0x~^, ~}"
          (color-not-in-palette-x c)
          (color-not-in-palette-y c)
          (color-not-in-palette-i c)
          (when (not (emptyp (color-not-in-palette-image c)))
            (color-not-in-palette-image c))
          (color-not-in-palette-pixel c)
          (coerce (color-not-in-palette-palette c) 'list))
  #+ () ;; TODO: #1243: #1242
  (print-image (color-not-in-palette-image-pixels c)
               (color-not-in-palette-palette c)
               ))

(defun pixel-into-palette (pixel palette &key x0 y0 x i image best-fit-p)
  (check-type pixel (integer 0 #xff))
  (let ((index (or (position pixel palette)
                   (when best-fit-p
                     (destructuring-bind (r g b) (elt (machine-palette) pixel)
                       (position (find-nearest-in-palette (mapcar (lambda (i) (elt (machine-palette) i))
                                                                  (coerce palette 'list))
                                                          r g b)
                                 (mapcar (lambda (c) (elt (machine-palette) c))
                                         (coerce palette 'list))
                                 :test 'equalp))))))
    (or index
        (error 'color-not-in-palette-error
               :pixel pixel
               :x (if x (if x0 (+ x0 x) x) "?")
               :y (if y0 y0 "?")
               :i (if i (format nil "in image ~d " i) "")
               :image image
               :palette palette))))

(defun ansi-color-rgb (r g b &optional (foregroundp t))
  (format nil "~c[~d;2;~d;~d;~dm"
          #\Escape (if foregroundp 38 48) r g b))

(defun ansi-color-pixel (r g b)
  (format nil "~a~a██~c[0m" (ansi-color-rgb r g b)
          (ansi-color-rgb r g b nil)
          #\Escape))

(defun pixels-to-ansi (pixels &key x y)
  (flet ((tb ()
             (terpri)
             (princ (ansi-color-pixel 0 0 0))
             (dotimes (x0 (array-dimension pixels 0))
               (princ (ansi-color-pixel 0 0 (if (eql x0 x) #xff 0))))
             (princ (ansi-color-pixel 0 0 0))
             (format t "~c[0m" #\Escape)))
    (format t "~& Image (~:d×~:d pixels):"
            (array-dimension pixels 0)
            (array-dimension pixels 1))
    (tb)
    (dotimes (y0 (array-dimension pixels 1))
      (terpri)
      (princ (ansi-color-pixel 0 0 (if (eql y y0) #xff 0)))
      (dotimes (x0 (array-dimension pixels 0))
        (destructuring-bind (r g b) (palette->rgb (aref pixels x0 y0))
          (princ (ansi-color-pixel r g b))))
      (princ (ansi-color-pixel 0 0 (if (eql y y0) #xff 0)))
      (format t "~c[0m" #\Escape))
    (tb)
    (terpri)
    (finish-output)))

(defun pixels-to-clim (pixels &key x y (stream t))
  (let ((s (or stream t)))
    (flet ((tb ()
               (terpri)
               (print-clim-pixel (list 0 0 0) s)
               (dotimes (x0 (array-dimension pixels 0))
                 (print-clim-pixel (let ((val (if (= x x0) #xff 0))) (list val val val)) s))
               (print-clim-pixel (list 0 0 0) s)))
      (format t "~& Image (~:d×~:d pixels):"
              (array-dimension pixels 0)
              (array-dimension pixels 1))
      (tb)
      (dotimes (y0 (array-dimension pixels 1))
        (terpri)
        (print-clim-pixel (list 0 0 (if (eql y y0) #xff 0)) s)
        (dotimes (x0 (array-dimension pixels 0))
          (destructuring-bind (r g b) (palette->rgb (aref pixels x0 y0))
            (print-clim-pixel (list r g b) s)))
        (print-clim-pixel (list 0 0 (if (eql y y0) #xff 0)) s))
      (tb)
      (terpri)
      (finish-output))))

(defun pixels-to-ansi-string (pixels &key x y)
  (with-output-to-string (*standard-output*)
    (pixels-to-ansi pixels :x x :y y)))

(defun pixels-into-palette (pixels palette &key x0 y0 i best-fit-p image)
  "Assign every one of PIXELS to fit within PALETTE.

Optional X0, Y0, I are used for messaging, indicating that X0, Y0 is the
position within a larger image I."
  (assert (= 1 (array-dimension pixels 1)))
  (let* ((width (array-dimension pixels 0))
         (output (make-array (list width) :element-type '(unsigned-byte 8))))
    (dotimes (x width)
      (let ((pixel (aref pixels x 0)))
        (setf (aref output x)
              (pixel-into-palette pixel palette
                                  :x0 x0 :y0 y0 :x x :i i
                                  :image image
                                  :best-fit-p best-fit-p))))
    output))

(defun 7800-image-to-160a (image &key byte-width height palette best-fit-p)
  "Convert IMAGE to 160A bytes.

BYTE-WIDTH  is the  width of  IMAGE in  bytes; HEIGHT  is the  height in
pixels; PALETTE is the palette to which to hold the image. If BEST-FIT-P
is generally  true, then allow #'PIXELS-INTO-PALETTE  to use the
best-fit color from  the palette; otherwise, allow  it to signal
an error."
  (let ((bytes-across (list)))
    (dotimes (b byte-width)
      (let ((bytes (list)))
        (dotimes (y height)
          (let* ((byte-pixels (extract-region image
                                              (* b 4) y
                                              (1- (* (1+ b) 4)) y))
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
@item Monochrome display (1 background/foreground color + transparent)
@item 320 pixels horizontal resolution
@item 1 bit per pixel = 1/8 byte per pixel
@item Pixel value 0 = transparent, value 1 = background/foreground color
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

@strong{Maria Graphics Modes Reference:}
For detailed information about the six Maria drawing modes (320A/B/C/D and 160A/B), see @ref{Asset Formats Graphics Organization, , Graphics Memory Organization} in the Asset Formats chapter of the Developer's Guide, and the DMA control information in @ref{DMA Initialization Requirements, , DMA Initialization Requirements}.

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
                                               :best-fit-p best-fit-p)))
            ;; 320C packs 4 pixels into 1 byte, 2 bits per pixel
            (push (logior
                   (ash (aref indices 0) 6)
                   (ash (aref indices 1) 4)
                   (ash (aref indices 2) 2)
                   (aref indices 3))
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

#+mcclim
(defun print-clim-pixel (color stream &key shortp (unit #x10))
  (setf unit (or unit #x10))
  (clim:with-output-as-presentation (stream color 'palette-color)
    (clim:with-room-for-graphics (stream ;; :width (* (if shortp 1 3/2) unit 2)
                                  ;; :height (* (if shortp 1 3/2) unit)
			    )
      (setf (clim:medium-ink stream) (apply #'clim:make-rgb-color
                                            (mapcar (lambda (c) (/ c 255.0))
                                                    (elt (machine-palette 7800) color))))
      (clim:draw-rectangle* stream 0 0
                            (* (if shortp 1 3/2) unit 2)
                            (* (if shortp 1 3/2) unit) :filled t)
      (setf (clim:medium-ink stream) clim:+foreground-ink+))))

(defun print-ansi-pixel (color stream)
  (format stream (apply #'ansi-color-pixel
                        (etypecase color
                          (integer (elt (machine-palette *machine*) color))
                          (cons color)))))

(defun print-wide-pixel (color stream &key shortp unit)
  (cond
    #+mcclim
    ((member (package-name (symbol-package (class-name (class-of stream))))
             '(clim clim-listener) :test #'string-equal)
     (print-clim-pixel color stream :shortp shortp :unit unit))
    ((tty-xterm-p)
     (print-ansi-pixel color stream))
    (t (if (consp color)
           (format stream " #~{~2,'0x~2,'0x~2,'0x~} " color)
           (format stream " ~2,'0x " color)))))

(defun print-machine-palette (stream)
  (format stream "~2&Machine palette:")
  (dotimes (i #x100)
    (when (zerop (mod i #x10))
      (terpri stream))
    (print-wide-pixel i stream :unit 8))
  (force-output stream))

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
                                                  (1+ (* 2 byte-i)) y))
                     (indices (pixels-into-palette byte-pixels palette
                                                   :x0 (* 2 byte-i) :y0 y :i i)))
                ;; pixel:bit order = A: 3276, B: 1054
;;;
                ;; which translates to bit:pixel order =
;;;
                ;; A1 A0 B1 B0 A3 A2 B3 B2
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
                                               b y
                                               (+ b 7) y)))
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
                                                (1- (* (1+ b) 4)) y))
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

(defmethod parse-7800-object ((mode (eql :320c)) pixels &key width height palette)
  (assert (>= 8 (length palette)))
  (let ((total-width (array-dimension pixels 0))
        (total-height (1- (array-dimension pixels 1))))
    (assert (zerop (mod total-height height)) (total-height)
            "Image height must be modulo ~:Dpx plus 1px for palette strip, but got ~:Dpx"
            height (1+ total-height))
    (assert (zerop (mod total-width width)) (total-width)
            "Image width must be module ~:Dpx, but get ~:Dpx" width total-width)
    (assert (zerop (mod width 4)) (width)
            "Width for mode 320C must be modulo 4px, not ~:Dpx" width))
  (let* ((byte-width (/ width 4))
         (images (extract-regions pixels width height))
         (bytes-lists (list)))
    (dolist (image images)
      (dotimes (b byte-width)
        (let ((bytes (list)))
          (dotimes (y height)
            (let* ((byte-pixels (extract-region image
                                                (* b 4) y
                                                (1- (* (1+ b) 4)) y))
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
          (push bytes bytes-lists))))
    (reverse bytes-lists)))

(defmethod parse-7800-object ((mode (eql :320d)) png &key width height palette)
  (declare (ignore png width height palette))
  (error "unimplemented mode ~A" mode))

(defun grab-7800-palette (mode png)
  "Extract the palette values for mode MODE from graphic PNG"
  (when (member mode '(:320a :320d))
    (return-from grab-7800-palette nil))
  (let* ((palette-size (ecase mode
                         (:160a 32)
                         (:160b 16)
                         (:320b 4)
                         (:320c 8)))
         (last-row (1- (array-dimension png 1)))
         (palette-strip (extract-region png
                                        0 last-row
                                        (1- palette-size) last-row)))
    (let ((palette (loop for i below palette-size
                         collect (aref palette-strip i 0))))
      (if (tty-xterm-p)
          (format *trace-output* "~&Palette detected: ~{
~5t~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}~^;~
~45t~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}, ~{$~2,'0x ~a~}~^;~}"
                  (mapcar #'palette-to-ansi-pairs palette))
          (format *trace-output* "~&Palette detected: ~{$~2,'0x~^, ~}" palette))
      palette)))

(defun parse-into-7800-bytes (art-index)
  (let ((bytes (list)))
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&~A: parsing in mode ~A (start at $~2,'0x)… "
                png-name mode (length bytes))
        (let* ((png (png-read:read-png-file png-name))
               (height (png-read:height png))
               (width (png-read:width png))
               (palette-pixels (png->palette height width
                                             (png-read:image-data png)
                                             (png-read:transparency png)))
               (palette (grab-7800-palette mode palette-pixels)))
          (appendf bytes
                   (parse-7800-object mode palette-pixels :width width-px :height height-px
                                                          :palette palette)))
        (format *trace-output* " … Done. (ends at $~2,'0x)" (1- (length bytes)))))
    (nreverse bytes)))

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

(defun compile-art-7800 (index-out index-in)
  (let ((*machine* 7800))
    (write-7800-binary index-out
                       (interleave-7800-bytes
                        (parse-into-7800-bytes
                         (read-7800-art-index index-in))))))

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
                       (machine-from-filename index-out)
                       5200)))
    (dolist (file png-files)
      (dispatch-png file index-out))))

(defun def->tile-id (tile-definition x y)
  (destructuring-bind (tag x₀ y₀ x₁ y₁) tile-definition
    (declare (ignore tag x₁ y₁))
    (let ((set-width (reduce #'max (mapcar #'fourth *tileset*))))
      (+ x₀ x (* set-width (+ y₀ y))))))

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

(defun tile-control-value (tile)
  (logand (if (getf tile :wall) #x80 0)
          (if (getf tile :swim) #x40 0)))

(defvar *tia-tiles*)
(defvar *tia-pf-colors*)

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
  (format t "~{~%	.byte $~2,'0x~^, ~2,'0x~^, ~2,'0x~^, ~2,'0x~^,~
 ~2,'0x~^, ~2,'0x~^, ~2,'0x~^, ~2,'0x~}"
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

(defun screen-to-grid/tia/tles (screen)
  (check-type screen (array integer (8 8)))
  (let ((tiles (make-array (list 4 8) :element-type 'fixnum)))
    (dotimes (y 8)
      (dotimes (2x 4)
        (let ((big-endian-p (evenp 2x)))
          (let* ((left (aref screen (* 2x 2) y))
                 (right (aref screen (1+ (* 2x 2)) y))
                 #+ ()  (tile-hash (tile-hash left right big-endian-p))
                 (merged-tile (or (gethash tile-hash *merged-tiles*)
                                  (setf (gethash tile-hash *merged-tiles*)
                                        (incf *tile-counter*)))))
            (assert (<= merged-tile *tile-counter*))
            (setf (aref tiles 2x y) merged-tile)))))
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
                            #'< :key (lambda (n) (gethash n popularity))))))

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

Determine if a 4×16 pixel stamp uses only monochrome values (0,1), making it suitable for 320A mode.

@strong{Detection Logic:}
@itemize
@item Counts unique palette indices in the stamp
@item Returns true if only values 0,1 are used (monochrome)
@item Suitable for 320A mode (1 color + transparent)
@item False indicates 320C mode needed (4 colors + transparent)
@end itemize

Used by 320A/C mode ripping to automatically select appropriate graphics mode per stamp."
  (let ((colors (make-hash-table)))
    (destructuring-bind (width height) (array-dimensions stamp)
      (dotimes (x width)
        (dotimes (y height)
          (setf (gethash (aref stamp x y) colors) t)))
      (<= (hash-table-count colors) 2))))

(defun limit-region-to-palette (region palette &key (allow-imperfect-p t))
  (let ((output (make-array (array-dimensions region)))
        (rgb (mapcar #'palette->rgb (coerce palette 'list))))
    (destructuring-bind (width height) (array-dimensions region)
      (dotimes (x width)
        (dotimes (y height)
          (setf (aref output x y)
                (if allow-imperfect-p
                    (apply #'rgb->palette
                           (apply #'find-nearest-in-palette rgb
                                  (palette->rgb (aref region x y))))
                    (or (position (aref region x y) palette)
                        (error "Color ~s  at (~d, ~d) is not in palette ~s"
                               (aref region x y) x y palette)))))))
    output))

(defun png-to-blob-pathname (png-file)
  (make-pathname :directory '(:relative "Source" "Generated" "Assets")
                 :name (concatenate 'string "Blob."
                                    (pathname-name png-file))
                 :type "s"))

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

(defun write-blob-palettes (png output)
  (princ "Palette:" output)
  (dolist (*region* '(:ntsc :pal))
    (let ((palettes (extract-palettes
                     (png->palette (png-read:height png)
                                   (png-read:width png)
                                   (png-read:image-data png)))))
      (format output "~%~10t.if TV == ~a
~12t.byte ~a~{~%~12t.byte ~a, ~a, ~a~}
~10t.fi~%"
              *region*
              (atari-colu-string (aref palettes 0 0))
              (mapcan (lambda (pal) (mapcar #'atari-colu-string (coerce (subseq pal 1 4) 'list)))
                      (2a-to-list palettes))))))

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
              (let ((bytes-across (7800-image-to-160a (elt span stamp)
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
                        ;; 320A mode: 8 pixels per byte, monochrome
                        (let ((bytes-across (7800-image-to-320a stamp-data
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

(defun blob-rip-7800 (png-file &optional (imperfectp$ nil))
  "@cindex BLOB ripping
@cindex graphics mode auto-detection
@cindex 160A mode
@cindex 320A/C mode

@table @code
@item Package: skyline-tool
@item Arguments: png-file (pathname or string), &optional imperfectp$ (boolean)
@item Returns: nil
@item Side Effects: Creates .s file with BLOB data, outputs progress to *trace-output*
@end table

Rip a Bitmap Large Object Block from PNG-FILE, automatically selecting the appropriate Atari 7800 graphics mode based on image width.

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
         (height (png-read:height png))
         (width (png-read:width png))
         (palette-pixels (png->palette height width
                                       (png-read:image-data png)))
         (output-pathname (png-to-blob-pathname png-file))
         (imperfectp (or (eql :imperfect imperfectp$)
                         (equal imperfectp$ "--imperfect"))))
    (format *trace-output* "accepting ~:[only perfect palette matches~;imperfect palette matches~]… " imperfectp)
    ;; Route to appropriate ripping method based on width
    (if (= width 320)
        (blob-rip-7800-320ac png-file imperfectp$)
        (blob-rip-7800-160a png-file imperfectp$))))

  (defun blob-rip-7800-160a (png-file &optional (imperfectp$ nil))
    "@cindex BLOB ripping
@cindex 160A graphics mode
@cindex sprite graphics

@table @code
@item Package: skyline-tool
@item Arguments: png-file (pathname or string), &optional imperfectp$ (boolean)
@item Returns: nil
@item Side Effects: Creates .s file with BLOB data, outputs progress to *trace-output*
@end table

Rip a Bitmap Large Object Block in 160A mode from PNG-FILE for standard sprite graphics.

@strong{Graphics Mode:}
@itemize
@item 4 pixels per byte (2 bits per pixel)
@item Up to 25 palettes (background + 8 palettes × 3 colors each)
@item Variable width (multiple of 4 pixels)
@item Height multiple of 16 + 1 pixels (palette strip)
@end itemize

@strong{Use Cases:}
@itemize
@item Character sprites and animations
@item Game objects and items
@item General purpose graphics (non-320px wide)
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
        (ensure-directories-exist output-pathname)
        (with-output-to-file (output output-pathname :if-exists :supersede)
          (format output ";;; Bitmap Large Object Block for Atari 7800
;;; Derived from source file ~a. This is a generated file.~3%

Blob_~a:~10t.block~2%"
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
    (format *trace-output* " … done!~%"))

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
@item Uses 320C mode for color stamps (2 bits per pixel, 4 colors + transparent)
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
    (let* ((palettes (extract-palettes palette-pixels))
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
      (ensure-directories-exist output-pathname)
      (with-output-to-file (output output-pathname :if-exists :supersede)
        (format output ";;; Bitmap Large Object Block for Atari 7800 (320A/C mode)
;;; Derived from source file ~a. This is a generated file.~3%

Blob_~a:~10t.block~2%"
                (enough-namestring png-file)
                (assembler-label-name (pathname-name png-file)))
        (write-blob-palettes png output)
        (format output "~%Zones:~%~10t.byte ~d~10t; zone count" zones)
        (dotimes (zone zones)
          (format output "~2&Zone~d:" zone)
          (flet ((emit-span (x span last-palette last-mode)
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
                               id last-palette (length span)
                               (- x (length span)))))))))
        (loop with span = nil
              with last-palette = nil
              with last-mode = nil
              for x from 0 by 1
              for column from 0 below columns
              for stamp = (aref stamps column zone)
              for mode = (if (stamp-is-monochrome-p stamp) :320a :320c) ; Auto-detect mode
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
              do (when (= (mod column 20) 0)
                   (format *trace-output* " col ~d/~d…" column columns)
                   (force-output *trace-output*))
                 (cond
                   ((zerop column)
                    (setf span (list paletted-stamp)
                          last-palette palette
                          last-mode mode))
                   ((blank-stamp-p stamp (aref palettes 0 0))
                    (emit-span x span last-palette last-mode)
                    (setf span nil
                          last-palette nil
                          last-mode nil))
                   ((and (or (null last-palette)
                             (= palette last-palette))
                         (eq mode last-mode)
                         (< (length span) 31))
                    (appendf span (list paletted-stamp))
                    (setf last-palette palette
                          last-mode mode))
                   (t
                    (emit-span x span last-palette last-mode)
                    (setf span (list paletted-stamp)
                          last-palette palette
                          last-mode mode)))
              finally
                 (emit-span x span last-palette last-mode))
        (format output "~%~10t.word $0000")
        (blob/write-spans-320ac spans output :imperfectp imperfectp)
        (format output "~2%~10t.bend~%"))))
  (format *trace-output* " … done!~%"))

(defun vcs-ntsc-color-names ()
  (loop for hue below #x10
        appending (loop for value below #x10 by 2
                        collecting (format nil "~a $~x"
                                           (subseq (string (elt +atari-ntsc-color-names+ hue)) 3)
                                           value))))

(defun vcs-pal-color-names ()
  (loop for hue below #x10
        appending (loop for value below #x10 by 2
                        collecting (format nil "~a $~x"
                                           (subseq (string (elt +atari-pal-color-names+ hue)) 3)
                                           value))))

(defun prosystem-ntsc-color-names ()
  (loop for hue below #x10
        appending (loop for value below #x10
                        collecting (format nil "~a $~x"
                                           (subseq (string (elt +atari-ntsc-color-names+ hue)) 3)
                                           value))))

(defun prosystem-pal-color-names ()
  (loop for hue below #x10
        appending (loop for value below #x10
                        collecting (format nil "~a $~x"
                                           ;; XXX these are NTSC color names
                                           (subseq (string (elt +atari-pal-color-names+ hue)) 3)
                                           value))))


(defun write-gimp-palette (name colors &optional color-names)
  (with-output-to-file (pal (make-pathname :name name
                                           :type "gpl"
                                           :directory '(:relative "Tools"))
                            :if-exists :supersede)
    (format pal "GIMP Palette
Name: ~a
Columns: ~d
#~{~%~3d ~3d ~3d # ~a~}~%"
            (substitute #\Space #\- name)
            (ecase (length colors)
              ((1 2 4 8) (length colors))
              ((16 32 64) 16)
              (128 8)
              (256 16)
              (512 32)
              (4096 16))
            (if color-names
                (mapcan (lambda (rgb n) (append rgb (list n)))
                        colors color-names)
                (mapcan (lambda (rgb) (append rgb (list (format nil "#~{~2,'0x~2,'0x~2,'0x~}" rgb))))
                        colors))))
  (format *trace-output* "~&Wrote ~:d color~:p palette “~a”~%"
          (length colors)
          (substitute #\Space #\- name))
  (when (<= (length colors) 256)
    (let ((i 0))
      (dolist (color colors)
        (print-wide-pixel color *trace-output*)
        (cond
          ((< (length colors) 20)
           (format *trace-output* " ~a~%"(elt color-names i)))
          (t
           (when (= 15 (mod i 16))
             (terpri *trace-output*))))
        (incf i))))
  (format *trace-output* "~C[0m" #\Escape))

(defun write-gimp-palettes ()
  "Write out Gimp palettes for those I know"
  (write-gimp-palette "Atari-2600-NTSC" +vcs-ntsc-palette+ (vcs-ntsc-color-names))
  ;; (write-gimp-palette "Atari-2600-PAL" +vcs-pal-palette+ (vcs-pal-color-names))
  (write-gimp-palette "Atari-2600-SECAM" +vcs-secam-palette+
                      (mapcar (lambda (s) (cl-change-case:title-case (subseq (string s) 3)))
                              +vcs-secam-color-names+))
  (write-gimp-palette "Atari-7800-NTSC" +prosystem-ntsc-palette+ (prosystem-ntsc-color-names))
  (write-gimp-palette "Atari-7800-PAL" +prosystem-pal-palette+ (prosystem-pal-color-names))
  (write-gimp-palette "Commodore-64" +c64-palette+
                      (mapcar (compose #'cl-change-case:title-case
                                       #'string)
                              +c64-names+))
  (write-gimp-palette "NES-NTSC" +nes-palette-ntsc+)
  (write-gimp-palette "NES-PAL" +nes-palette-pal+)
  (write-gimp-palette "TurboGrafx-16" +tg16-palette+)
  (write-gimp-palette "Lynx" +lynx-palette+)
  (write-gimp-palette "Intellivision" +intv-palette+
                      (mapcar (compose #'cl-change-case:title-case
                                       #'string)
                              +intv-color-names+)))

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
(defun print-clim-color (color stream)
  (clim:with-output-as-presentation (stream color 'palette-color)
    (clim:with-room-for-graphics (stream :height 24)
      (print-wide-pixel color stream :shortp t)
      (format stream " Color $~2,'0x = ~a #~2,'0x~2,'0x~2,'0x"
              color (atari-colu-string color)
              (elt (elt (machine-palette 7800) color) 0)
              (elt (elt (machine-palette 7800) color) 1)
              (elt (elt (machine-palette 7800) color) 2)))))

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
)