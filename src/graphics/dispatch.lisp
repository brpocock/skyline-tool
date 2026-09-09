(in-package :skyline-tool)

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
  (> 3 (length (image-colors palette-pixels))))

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
       (compile-gtia-player png-file target-dir height width palette-pixels))

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

(defmethod dispatch-png% ((machine (eql 2416)) png-file target-dir
                          png height width α palette-pixels)
  "Dispatch PNG processing for Commander X-16 (VERA graphics chip)"
  (let ((monochrome-lines-p (monochrome-lines-p palette-pixels height width)))
    (cond
      ;; Text mode fonts (8x8 characters, monochrome)
      ((and (= width 128) (= height 64) monochrome-lines-p) ; 16x8 characters
       (format *trace-output* "~% Image ~A seems to be Commander X-16 text font (128x64)"
               png-file)
       (compile-cdr-text-font png-file target-dir palette-pixels))

      ;; Tile sets (multiples of 8x8, 16x16, 32x32)
      ((and (zerop (mod width 8)) (zerop (mod height 8))
            (>= (* (/ width 8) (/ height 8)) 16)) ; At least 16 tiles
       (format *trace-output* "~% Image ~A seems to be Commander X-16 tileset (~Dx~D)"
               png-file (/ width 8) (/ height 8))
       (compile-cdr-tileset png-file target-dir height width palette-pixels))

      ;; Bitmap modes (320x240, 640x480, etc.)
      ((and (= width 320) (= height 240))
       (format *trace-output* "~% Image ~A seems to be Commander X-16 bitmap (320x240)"
               png-file)
       (compile-cdr-bitmap png-file target-dir height width palette-pixels))

      ((and (= width 640) (= height 480))
       (format *trace-output* "~% Image ~A seems to be Commander X-16 bitmap (640x480)"
               png-file)
       (compile-cdr-bitmap png-file target-dir height width palette-pixels))

      ;; Sprite data (8-pixel aligned)
      ((zerop (mod width 8))
       (format *trace-output* "~% Image ~A seems to be Commander X-16 sprite data"
               png-file)
       (compile-cdr-sprite png-file target-dir height width palette-pixels))

      (t (error "Don't know how to deal with Commander X-16 image with dimensions ~
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
     (compile-tileset-64 png-file target-dir height width palette-pixels))

    ((and (zerop (mod height 21))
          (zerop (mod width 24)))
     (format *trace-output* "~% Image ~A seems to be sprite MOB data" png-file)
     (compile-mob png-file target-dir height width palette-pixels))

    ;; Full-screen character-cell blob (multiples of 40×25 chars = 320×200)
    ((and (zerop (mod width 8))
          (zerop (mod height 8))
          (>= (/ width 8) 40)
          (>= (/ height 8) 25))
     (format *trace-output* "~% Image ~A seems to be a C64 bitmap blob" png-file)
     (compile-c64-blob png-file target-dir height width palette-pixels))

    (t (error "Don't know how to deal with image with dimensions ~:D×~:D pixels"
              width height))))

(defmethod dispatch-png% ((machine (eql 128)) png-file target-dir
                          png height width α palette-pixels)
  "C128: VDC BLOB for 640×200/320×200 images; otherwise VIC-II (same as C64)."
  (cond
    ;; VDC bitmap blobs: 640 or 320 wide, 200 or 400 tall
    ((and (member width '(640 320))
          (member height '(200 400)))
     (format *trace-output* "~% Image ~A (~D×~D) seems to be a VDC bitmap" png-file width height)
     (compile-vdc-blob png-file target-dir height width palette-pixels))
    ;; Fall through to VIC-II (same logic as C64)
    (t
     (format *trace-output* "~% C128: deferring ~A to VIC-II path" png-file)
     (dispatch-png% 64 png-file target-dir png height width α palette-pixels))))

(defmethod dispatch-png% ((machine (eql 9918)) png-file target-dir ; ColecoVision
                          png height width α palette-pixels)
  "ColecoVision / TMS9918: pattern data (8 bytes/tile), name table, color table."
  (declare (ignore α))
  (let ((blob-out (merge-pathnames (png-to-blob-pathname png-file) target-dir)))
    (setf width (- width (mod width 8))
          height (- height (mod height 8)))
    (if (or (< width 8) (< height 8))
        (error "TMS9918 image ~A too small (~D×~D)" png-file width height)
        (let* ((cols (/ width 8))
               (rows (/ height 8))
               (cells (* cols rows))
               (uniq (make-array 256 :adjustable t :fill-pointer 0))
               (ht (make-hash-table :test 'equal))
               (nametable (make-array cells :element-type '(unsigned-byte 8)))
               (stem (pathname-name (merge-pathnames png-file))))
          (loop for cell from 0 below cells
                for row = (floor cell cols) for col = (mod cell cols)
                for sx = (* col 8) for sy = (* row 8)
                for key = (cell-binary-key-from-image palette-pixels sx sy 8 8)
                for idx = (gethash key ht)
                do (unless idx
                     (setf idx (length uniq))
                     (setf (gethash key ht) idx)
                     (vector-push-extend key uniq))
                   (setf (aref nametable cell) idx))
          ;; Reduce if pattern table overflows 256
          (when (> (length uniq) 256)
            (warn "TMS9918 image ~A: ~D patterns, reducing to 256..." png-file (length uniq))
            (reduce-tile-set uniq nametable :max-slots 256))
          (ensure-directories-exist blob-out)
          (with-output-to-file (src blob-out :if-exists :supersede)
            (format src ";;; TMS9918 tile data from ~A~%" png-file)
            (format src ";;; ~D×~D chars; ~D unique~2%" cols rows (length uniq))
            (format src "~APatterns:~%" stem)
            (loop for u from 0 below (length uniq)
                  for row-bytes = (aref uniq u)
                  do (loop for byte in row-bytes
                           do (format src "  .byte $~2,'0X~%" byte)))
            (format src "~ANameTable:~%" stem)
            (loop for i from 0 below cells
                  do (format src "  .byte ~D~%" (aref nametable i))))
          (format *trace-output* "~&wrote TMS9918 blob to ~A~%" blob-out)
          blob-out))))

(defmethod dispatch-png% ((machine (eql 2609)) png-file target-dir
                          png height width α palette-pixels)
  "Intellivision: GROM-first tile map + deduplicated GRAM cards for non-GROM 8×8 cells."
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file) :type "s")
                   target-dir)))
    (compile-blob-intv-screen png-file out-file palette-pixels width height)))

(defun compile-ted-bitmap (png-file target-dir height width palette-pixels)
  "Compile TED bitmap graphics (320x200, 2 colors per 8x8 cell).

Divides the image into 40×25 8×8 character cells.  Each cell gets a
monochrome bitmap (8 bytes) and a color RAM byte (upper nybble = fg,
lower nybble = bg).  Outputs @file{.s} assembly with @code{bitmap},
@code{colorram}, and @code{screen} sections."
  (setf width (- width (mod width 8))
        height (- height (mod height 8)))
  (let* ((out-file (merge-pathnames
                    (make-pathname :name (pathname-name png-file) :type "s")
                    target-dir))
         (lab (pathname-name png-file))
         (cols (/ width 8))
         (rows (/ height 8))
         (total (* cols rows)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src out-file :if-exists :supersede)
      (format src ";;; TED bitmap compiled from ~A~%" png-file)
      (format src ";;; ~D×~D px → ~D×~D chars~2%" width height cols rows)
      (format src "~A_cols EQU ~D~%" lab cols)
      (format src "~A_rows EQU ~D~2%" lab rows)
      (format src "~A_bitmap:  ;; ~D cells × 8 bytes~%" lab total)
      (format src "~A_colorram:  ;; ~D bytes~%" lab total)
      (loop for cell from 0 below total
            for row = (floor cell cols) for col = (mod cell cols)
            for sx = (* col 8) for sy = (* row 8)
            for fg = 1 for bg = 0
            for freq = (make-array 16 :initial-element 0)
            do (loop for y from sy below (+ sy 8)
                     do (loop for x from sx below (+ sx 8)
                              for c = (aref palette-pixels x y)
                              do (incf (aref freq c))
                                 (when (> c bg)
                                   (if (zerop (aref freq bg))
                                       (setf bg c)
                                       (unless (= c bg)
                                         (setf fg c))))))
               ;; Emit bitmap (8 bytes per cell, 1 bit per pixel, fg = 1)
               (loop for y from sy below (+ sy 8)
                     for byte = (loop for x from sx below (+ sx 8)
                                      for c = (aref palette-pixels x y)
                                      sum (if (and (plusp c) (/= c bg)) (ash 1 (- 7 (- x sx))) 0))
                     ;; interleave bitmap and colorram for TED layout
                     do (format src "    .byte $~2,'0X~%" byte))
               (format src "    .byte $~2,'0X  ; color (@~D,~D)~%"
                       (logior (ash (logand fg #x0F) 4) (logand bg #x0F))
                       col row)))
    (format *trace-output* "~&Compiled TED bitmap: ~A (~D cells)~%" out-file total)))

(defvar *ted-screen-colors* nil
  "Global variable to store TED screen color data during bitmap compilation")

(defun compile-ted-charmap (png-file target-dir height width palette-pixels)
  "Compile TED character map graphics (8x8 cells, 2 colors each)"
  (let* ((out-file (merge-pathnames
                    (make-pathname :name (pathname-name png-file)
                                   :type "s")
                    target-dir))
         (chars-wide (/ width 8))
         (chars-high (/ height 8)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; TED Character map compiled from ~A
;;; Commodore 16/Plus4 character graphics (~Dx~D)
;;; Generated automatically
~2%" png-file width height)

      (let* ((total-chars (* chars-wide chars-high)))

        (format src-file ";;; Character data: ~D characters × 8 bytes each = ~D bytes
;;; Color RAM: ~D bytes
~2%" total-chars (* total-chars 8) total-chars)

        ;; Generate character data
        (format src-file "~A_chars:~%" (pathname-name png-file))

        (dotimes (char-y chars-high)
          (dotimes (char-x chars-wide)
            (format src-file "~%    ;; Character (~D,~D) - index ~D~%"
                    char-x char-y (+ (* char-y chars-wide) char-x))

            ;; Extract colors for this character
            (let ((colors-used (make-hash-table)))
              (dotimes (y 8)
                (dotimes (x 8)
                  (let* ((global-x (+ (* char-x 8) x))
                         (global-y (+ (* char-y 8) y))
                         (color-index (if (and (< global-x width) (< global-y height))
                                          (aref palette-pixels global-x global-y)
                                          0)))
                    (setf (gethash color-index colors-used) t))))

              (let* ((color-list (loop for color being the hash-keys of colors-used collect color))
                     (bg-color (if color-list (car color-list) 0))
                     (fg-color (if (> (length color-list) 1) (cadr color-list) bg-color)))

                ;; Generate character bitmap (8 bytes)
                (dotimes (byte 8)
                  (let ((char-byte 0))
                    (dotimes (bit 8)
                      (let* ((pixel-x bit)
                             (pixel-y byte)
                             (global-x (+ (* char-x 8) pixel-x))
                             (global-y (+ (* char-y 8) pixel-y))
                             (pixel-color (if (and (< global-x width) (< global-y height))
                                              (aref palette-pixels global-x global-y)
                                              bg-color)))
                        (when (= pixel-color fg-color)
                          (setf char-byte (logior char-byte (ash 1 (- 7 bit)))))))
                    (format src-file "    .byte $~2,'0X~%" char-byte)))

                ;; Store color info
                (push (cons (+ (* char-y chars-wide) char-x)
                            (logior (ash (logand fg-color #x0F) 4)
                                    (logand bg-color #x0F)))
                      *ted-char-colors*)))))

        ;; Generate color RAM data
        (format src-file "~2%~A_colors:~%" (pathname-name png-file))
        (let ((color-data (make-array total-chars :element-type '(unsigned-byte 8) :initial-element 0)))
          (dolist (color-info *ted-char-colors*)
            (destructuring-bind (offset . color) color-info
              (setf (aref color-data offset) color)))

          (dotimes (i total-chars)
            (when (zerop (mod i 16))
              (format src-file "~%    .byte "))
            (format src-file "$~2,'0X" (aref color-data i))
            (if (= (mod (1+ i) 16) 0)
                (format src-file "~%")
                (format src-file ", "))))

        ;; Clear global data
        (setf *ted-char-colors* nil)

        (format src-file "~2%;;; Character map descriptor
~A_descriptor:
    .word ~A_chars      ; Character data pointer
    .word ~A_colors     ; Color RAM pointer
    .byte ~D            ; Characters wide
    .byte ~D            ; Characters high
    .word ~D            ; Total characters
~2%" (pathname-name png-file) (pathname-name png-file) (pathname-name png-file)
chars-wide chars-high total-chars))))

  (format *trace-output* "~&Compiled TED character map: ~A (~Dx~D chars)" out-file chars-wide chars-high))

(defvar *ted-char-colors* nil
  "Global variable to store TED character color data during compilation")

