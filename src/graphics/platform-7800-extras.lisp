(in-package :skyline-tool)

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

(defun 320c-choose-limit-palette (stamp c2-entries)
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

(defun blank-stamp-p (region background-color)
  (destructuring-bind (width height) (array-dimensions region)
    (dotimes (x width)
      (dotimes (y height)
        (unless (= background-color (aref region x y))
          (return-from blank-stamp-p nil)))))
  t)

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

(defun compile-art-7800 (index-out index-in)
  "Compile 7800 art assets from INDEX-IN to binary at INDEX-OUT.

Parses a 7800 art index file, converts the referenced PNG assets into
interleaved 7800-format bytes (bitplanes for Maria), and writes the
resulting binary file.

@table @asis
@item INDEX-OUT
Output path for the compiled binary
@item INDEX-IN
Input path for the 7800 art index file
@end table

@xref{fun:read-7800-art-index}, @xref{fun:interleave-7800-bytes}."
  (let ((*machine* 7800)
        (*region* (if (boundp '*region*) *region* :ntsc)))
    (write-7800-binary index-out
                       (interleave-7800-bytes
                        (parse-into-7800-bytes
                         (read-7800-art-index index-in))))))

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

(defun gather-stamp-bytes (normalized-pixels stamp-bytes &key stamp-index)
  (dotimes (b #x10)
    (dotimes (y #x10)
      (let ((a (aref normalized-pixels (* 2 b) y))
            (b (aref normalized-pixels (1+ (* 2 b)) y)))
        (setf (aref stamp-bytes (+ (* #x10 stamp-index) b) y)
              (160b-wiggle-nybbles a b))))))

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

(defun most-popular-13-colors (pixels width height)
  (most-popular-colors pixels width height :count 13))

(defun most-popular-colors (pixels width height &key count background)
  (let ((popularity (make-hash-table)))
    (dotimes (x width)
      (dotimes (y height)
        (unless (and background (= background (aref pixels x y)))
          (incf (gethash (aref pixels x y) popularity 0)))))
    (list-chomp count (sort (hash-table-keys popularity)
                            #'> :key (lambda (n) (gethash n popularity))))))

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
  (let* ((root (uiop:ensure-directory-pathname (or (project-root) (uiop:getcwd))))
         (abs-output (merge-pathnames output-pathname root)))
    (ensure-directories-exist abs-output)
    (let* ((dir (uiop:pathname-directory-pathname abs-output))
           (wip-name (format nil "~A.wip.~36,6,'0R"
                             (pathname-name abs-output)
                             (logxor (ash (get-internal-real-time) 16)
                                     (random #xfffffff))))
           (part (merge-pathnames
                  (make-pathname :name wip-name
                                 :type (pathname-type abs-output))
                  dir))
           (dest (merge-pathnames
                  (make-pathname :name (pathname-name abs-output)
                                 :type (pathname-type abs-output))
                  dir))
           (ok nil))
      (unwind-protect
           (progn
             (with-output-to-file (out part :if-exists :supersede
                                            :external-format :utf-8)
               (funcall writer out))
             (when (probe-file dest)
               (ignore-errors (delete-file dest)))
             (rename-file part dest)
             (setf ok t))
        (unless ok
          (when (probe-file part)
            (ignore-errors (delete-file part))))))))

(defun write-blob-palettes (png output &key (extractor 'extract-palettes) (start-offset 0))
  (fresh-line output)
  (princ "Palette:" output)
  (dolist (*region* '(:ntsc :pal))
    (let ((palettes (funcall extractor
                             (png->palette (png-read:height png)
                                           (png-read:width png)
                                           (png-read:image-data png)))))
      (format output "~%~10t.if TV == ~a
~12t.byte ~a~{~%~12t.byte ~a, ~a, ~a~}
~10t.fi~%"
              *region*
              (atari-colu-string (aref palettes 0 0))
              (append (make-list (* 3 start-offset) :initial-element 0)
                      (mapcan (lambda (pal) (mapcar #'atari-colu-string (coerce (subseq pal 1 4) 'list)))
                              (2a-to-list palettes)))))))


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
      (dolist (bytes-list (7800-image-to-160ab image
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
