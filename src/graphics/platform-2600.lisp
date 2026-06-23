(in-package :skyline-tool)

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

(defun try-to-maintain-palette (new old &key (overall old) lenient-p)
  (if (or (null old) (emptyp old))
      (if overall
          (return-from try-to-maintain-palette
            (try-to-maintain-palette new overall :overall overall :lenient-p lenient-p))
          (return-from try-to-maintain-palette new)))
  ;; Pad shorter palette to match new's length (Mode E rows can have varying color counts)
  (when (< (length old) (length new))
    (setf old (append old (loop repeat (- (length new) (length old)) collect 0))))
  (when (and overall (< (length overall) (length new)))
    (setf overall (append overall (loop repeat (- (length new) (length overall)) collect 0))))
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
    (if (every (lambda (color) (member color offer)) new)
        offer
        (if lenient-p
            new
            (progn
              (assert (every (lambda (color) (member color offer)) new)
                      (offer) "The offered palette of (~{$~2,'0x~^ ~}) did not contain colors in (~{$~2,'0x~^ ~})
(tried to preserve palette indices from (~{$~2,'0x~^ ~})~@[ or (~{$~2,'0x~^ ~})~]" offer new old)
              offer)))))

(assert (equalp '(0 1 2 3) (try-to-maintain-palette '(3 2 1 0) '(0 1 2 3))))
(assert (equalp '(0 1 4 3) (try-to-maintain-palette '(3 4 1 0) '(0 1 2 3))))
(assert (equalp '(0 6 7 8) (try-to-maintain-palette '(6 0 7 8) '(0 1 2 3))))
(assert (equalp '(5 6 7 8) (try-to-maintain-palette '(5 6 7 8) '(0 1 2 3))))

(assert (equalp '(5 6 7 8) (try-to-maintain-palette '(6 7 8 5) '(0 1 2 3) :overall '(5 6 7 8))))

(assert (equalp '(0 1 2 3) (try-to-maintain-palette '(3 2 1 0) nil :overall '(0 1 2 3))))
(assert (equalp '(0 1 4 3) (try-to-maintain-palette '(3 4 1 0) nil :overall '(0 1 2 3))))
(assert (equalp '(0 6 7 8) (try-to-maintain-palette '(6 0 7 8) nil :overall '(0 1 2 3))))
(assert (equalp '(5 6 7 8) (try-to-maintain-palette '(5 6 7 8) nil :overall '(0 1 2 3))))

(defun mode-e-row-bytes (pixels &key last-row-palette y overall-palette
                                     enforce-overall-palette-p lenient-palette-p)
  (check-type pixels array)
  (assert (= 1 (array-dimension pixels 1)))
  (let ((shape nil)
        (palette (if enforce-overall-palette-p
                     overall-palette
                     (try-to-maintain-palette
                      (most-popular-colors pixels (array-dimension pixels 0) 1
                                           :count 4)
                      last-row-palette
                      :overall overall-palette
                      :lenient-p lenient-palette-p))))
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

(defun mode-e-interpret (pixels &key base-palette (color-per-line-p t) lenient-palette-p)
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
                                 :enforce-overall-palette-p (not color-per-line-p)
                                 :lenient-palette-p lenient-palette-p)
             (assert (= (length palette) 4))
             (assert (= (length shape) (ceiling (array-dimension pixels 0) 4)))
             (setf last-palette (if color-per-line-p
                                    palette
                                    overall-palette))
             (appendf shapes shape)
             (push palette colors)
             (assert (= (length shapes) (* 1/4 (array-dimension pixels 0) (1+ y)))))
        finally (return (values shapes
                                (if color-per-line-p
                                    (reverse colors)
                                    (list overall-palette))))))

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
    (assert co (co) "Atari Color code ~s is not valid in region ~a" co *region*)
    (check-type lu (integer 0 15) "Atari Luminance value 0-15")
    (format nil "CoLu(~a, $~x)" co lu)))

(defun atari-colu-run (&optional byte)
  "Run atari-colu on BYTE (or nil) and return the result.
   If BYTE is omitted or NIL, returns the default background color.
   This stub replaces the previous unimplemented placeholder.
   It simply forwards to `atari-colu` for actual logic."
  (if byte
      (atari-colu byte)
      (atari-colu nil)))

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

(defun compile-batari-48px-command (png-file output-bas &rest args)
  "Command-line wrapper for compile-batari-48px.
   Arguments: PNG-FILE OUTPUT-BAS [titlescreen-kernel-p] [tv-standard]
   If third arg is 't' or 'T', enables titlescreen kernel mode.
   If fourth arg exists, uses it as TV standard (:ntsc, :pal, :secam)."
  (let* ((titlescreen-kernel-p (and args (string-equal (first args) "t")))
         (tv-standard (cond ((and args (>= (length args) 2))
                             (let ((std (string-upcase (second args))))
                               (cond ((string= std "NTSC") :ntsc)
                                     ((string= std "PAL") :pal)
                                     ((string= std "SECAM") :secam)
                                     (t :ntsc))))
                            (t :ntsc))))
    (compile-batari-48px png-file output-bas
                         :titlescreen-kernel-p titlescreen-kernel-p
                         :tv-standard tv-standard)))

(defun compile-batari-48px (png-file output-bas &key (titlescreen-kernel-p nil) (tv-standard :ntsc))
  "Compile a 48×42 pixel PNG bitmap to batariBASIC data format.
   Output format: 6 columns × 42 bytes, inverted-y (bottom-to-top),
   one byte per row, then double-newline before next column.
   Binary format: %00000000 with NO remarks inside data block.

   When TITLESCREEN-KERNEL-P is T:
   - Extracts color-per-line data from source PNG
   - Uses ×2 drawing style (double-height mode, 42 rows → 84 scanlines)
   - Outputs color data for each row (each row becomes 2 scanlines)
   - Output format suitable for titlescreen kernel minikernel
   - Determines minikernel slot (1, 2, or 3) from output filename:
     * Art.AtariAge.s → 48x2_1 (AtariAge logo)
     * Art.AtariAgeText.s → 48x2_2 (AtariAge text, replaces Interworldly on Publisher)
     * Art.Interworldly.s → 48x2_2 (Interworldly, conflicts with AtariAgeText - not currently used)
     * Art.ChaosFight.s → 48x2_3 (ChaosFight logo)

   Input PNG can be color (for titlescreen kernel) or 1bpp (for basic bitmap)."
  (let* ((input-path (uiop:ensure-pathname png-file))
         (png (png-read:read-png-file input-path))
         (width (png-read:width png))
         (height (png-read:height png))
         (rgb (png-read:image-data png))
         (alpha (png-read:transparency png))
         (output-path (uiop:ensure-pathname output-bas))
         (output-name (pathname-name output-path))
         ;; Determine minikernel slot from output filename
         (kernel-slot (cond ((search "AtariAgeText" output-name :test #'string-equal)
                             2)  ; AtariAgeText uses slot 2
                            ((or (search "AtariAge" output-name :test #'string-equal)
                                 (search "Publisher" output-name :test #'string-equal))
                             1)  ; AtariAge logo uses slot 1
                            ((or (search "Interworldly" output-name :test #'string-equal)
                                 (search "Author" output-name :test #'string-equal))
                             4)  ; Interworldly uses slot 4
                            ((or (search "ChaosFight" output-name :test #'string-equal)
                                 (search "Title" output-name :test #'string-equal))
                             3)  ; ChaosFight uses slot 3
                            (t 1)))  ; Default to slot 1
         (kernel-prefix (format nil "bmp_48x2_~d" kernel-slot))
         ;; Determine page address for bitmap data (pack four bitmaps on adjacent pages)
         (page-address (cond ((= kernel-slot 1)
                              "$f100")  ; AtariAge at $f100
                             ((= kernel-slot 2)
                              "$f200")  ; AtariAgeText at $f200
                             ((= kernel-slot 3)
                              "$f300")  ; ChaosFight at $f300
                             ((= kernel-slot 4)
                              "$f400")  ; Author at $f400
                             (t
                              "$f100"))))  ; Default to $f100
    (unless (= width 48)
      (error "Bitmap must be 48 pixels wide; got ~a" width))
    (unless (= height 42)
      (error "Bitmap must be 42 pixels tall; got ~a" height))
    (let* ((*machine* 2600)
           (*region* tv-standard)
           (palette (png->palette rgb alpha))
           (pixels (make-array (list width height)
                               :element-type '(unsigned-byte 8)))
           (label-name (cl-change-case:pascal-case (pathname-base-name input-path))))
      ;; Convert RGB to 1bpp bitmap (black/white) for shape data
      (loop for y from 0 below height
            do (loop for x from 0 below width
                     do (let ((r (aref rgb x y 0))
                              (g (aref rgb x y 1))
                              (b (aref rgb x y 2))
                              (a (if alpha (aref alpha x y) 255)))
                          (setf (aref pixels x y)
                                (if (and (>= a 128)
                                         (> (+ r g b) (* 3 128)))
                                    1 0)))))
      ;; Extract color-per-line data if titlescreen kernel mode
      (let* ((shape (48px-array-to-bytes pixels))
             (colors-per-line (when titlescreen-kernel-p
                                (loop for y from 0 below height
                                      collect (dominant-playfield-color palette width y))))
             ;; Create color output path: Art.Name.s -> Art.Name.colors.s
             (color-output-path (when titlescreen-kernel-p
                                  (make-pathname :directory (pathname-directory output-path)
                                                 :name (format nil "~a.colors" (pathname-name output-path))
                                                 :type (pathname-type output-path)))))
        (ensure-directories-exist output-path)
        (when color-output-path
          (ensure-directories-exist color-output-path))
        (if titlescreen-kernel-p
            ;; Titlescreen kernel assembly format (×2 drawing style)
            ;; Colors are written to a separate file to allow bitmap data to be page-aligned
            (progn
              ;; Write bitmap data file (without colors)
              (with-open-file (stream output-path
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                ;; Header banner
                (format stream ";;; Chaos Fight - ~a~%" (namestring output-path))
                (format stream "~%")
                (format stream ";;;; This is a generated file, do not edit.~%")
                (format stream ";;;; Color tables are in separate .colors.s files~%")
                (format stream ";;;; Bitmap data is packed at page-aligned address ~a (CPU/RORG space)~%" page-address)
                (format stream "~%")
                ;; Set relocatable CPU address for bitmap data (titlescreen kernel always runs in bank 9)
                ;; NOTE: We must *not* change the assembler's file offset here, or we blow past Bank 9's file space.
                ;;       Using RORG keeps the CPU address correct ($F100/$F200/…) without seeking the output file.
                (format stream "   rorg ~a~%" page-address)
                (format stream "~%")
                ;; Essential data without verbose comments
                (format stream "~a_window = ~d~%" kernel-prefix height)
                (format stream "~%")
                (format stream "~a_height = ~d~%" kernel-prefix height)
                (format stream "~%")
                (format stream " BYTE 0 ; leave this here!~%")
                (format stream "~%~%")
                ;; Note: Color table, PF1, PF2, and background are in separate .colors.s file at $f500
                ;; Output bitmap columns (6 columns: 00-05)
                ;; Alignment only at beginning (line above), not between strips
                (loop for column from 0 below 6
                      do (format stream "~%~%")
                      do (format stream "~a_~2,'0D~%" kernel-prefix column)
                         ;; Output rows in reverse order (bottom to top, inverted-y) - tab-indented
                         (loop for row from (1- height) downto 0
                               for byte = (elt (elt shape column) row)
                               for binary = (format nil "~8,'0b" byte)
                               do (format stream "~tBYTE %~a~%" binary))
                         (format stream "~%~%")))
              ;; Write color table, PF1, PF2, and background to separate file
              ;; These will be combined into titlescreen_colors.s at $f500
              (with-open-file (color-stream color-output-path
                                            :direction :output
                                            :if-exists :supersede
                                            :if-does-not-exist :create)
                ;; Header banner
                (format color-stream ";;; Chaos Fight - ~a~%" (namestring color-output-path))
                (format color-stream "~%")
                (format color-stream ";;;; This is a generated file, do not edit.~%")
                (format color-stream ";;;; Color table, PF1, PF2, and background for ~a bitmap~%" kernel-prefix)
                (format color-stream ";;;; This file will be included in titlescreen_colors.s at $f500~%")
                (format color-stream "~%")
                (format color-stream "~a_colors ~%" kernel-prefix)
                ;; Output colors in reverse order (bottom to top) - one per row, tab-indented
                (loop for y from (1- height) downto 0
                      for color-idx = (elt colors-per-line y)
                      for color-byte = (playfield-color-byte color-idx tv-standard)
                      do (format color-stream "~tBYTE $~2,'0X~%" color-byte))
                (format color-stream "~%")
                ;; PF1, PF2, and background (will be at $f500 with colors)
                ;; PF1 and PF2 must be defined unconditionally for ifconst checks in kernel
                (format color-stream "~a_PF1~%" kernel-prefix)
                (format color-stream "~tBYTE %00000000~%")
                (format color-stream "~a_PF2~%" kernel-prefix)
                (format color-stream "~tBYTE %00000000~%")
                (format color-stream " ifnconst ~a_background~%" kernel-prefix)
                (format color-stream "~a_background~%" kernel-prefix)
                (format color-stream " endif~%")
                (format color-stream "~tBYTE $00~%")
                (format color-stream "~%"))
              (format *trace-output* "~% Done writing titlescreen kernel bitmap to ~A~%" output-path)
              (format *trace-output* "~% Done writing color table to ~A~%" color-output-path)
              (values output-path color-output-path))
            ;; Basic batariBASIC data format (backward compatibility)
            (progn
              (with-open-file (stream output-path
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
                (format stream "rem Generated bitmap data from ~a~%" (pathname-name png-file))
                (format stream "rem Do not edit - regenerate from source artwork~%~%")
                (format stream "data Bitmap~a~%" label-name)
                (loop for column below 6
                      do (loop for row from 41 downto 0
                               for byte = (elt (elt shape column) row)
                               for binary = (format nil "~8,'0b" byte)
                               do (format stream "~%        %~a" binary))
                      do (format stream "~%~%"))
                (format stream "end~%"))
              (format *trace-output* "~% Done writing batariBASIC bitmap to ~A~%" output-path)
              output-path))))))

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
  (format *trace-output* "~&Calling external compressor for ~a: " base-name)
  (finish-output *trace-output*)
  (let ((output (let ((*standard-output* *trace-output*)
                      (*error-output* *trace-output*)
                      (bin-pathname (make-pathname :name base-name
                                                   :type "bin"
                                                   :directory `(:relative "Object" "Assets"
							    ,(machine-directory-name))))
                      (zx7-pathname (make-pathname :name base-name
                                                   :type "zx7"
                                                   :directory `(:relative "Object" "Assets"
							    ,(machine-directory-name)))))
                  (ensure-directories-exist bin-pathname)
                  (ensure-directories-exist zx7-pathname)
                  (write-byte-vector-into-file bytes bin-pathname :if-exists :overwrite
                                                                  :if-does-not-exist :create)
                  (uiop:run-program (list (namestring (merge-pathnames "bin/zx7mini" (uiop:getcwd)))
                                          (namestring bin-pathname)
                                          (namestring zx7-pathname))
                                    :output t :error-output t)
                  (with-input-from-file (zx7 zx7-pathname :element-type '(unsigned-byte 8))
                    (read-stream-content-into-byte-vector zx7)))))
    (format *trace-output* "… compression complete, new length ~:d bytes (-~5f%)"
            (length output) (- 100.0 (* 100.0 (/ (length output) (length bytes)))))
    output))

