(in-package :skyline-tool)

(defun compile-ted-sprite (png-file target-dir height width palette-pixels)
  "Compile TED sprite graphics (24x21 pixels, 2 colors)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; TED Sprite compiled from ~A
;;; Commodore 16/Plus4 sprite (24x21, 2 colors)
;;; Generated automatically
~2%" png-file)

      (format src-file ";;; Sprite data: 24x21 pixels = 63 bytes bitmap + 2 bytes color
~2%")

      ;; Generate sprite bitmap data (63 bytes)
      (format src-file "~A_sprite:~%" (pathname-name png-file))

      ;; Extract colors used in sprite
      (let ((colors-used (make-hash-table)))
        (dotimes (y 21)
          (dotimes (x 24)
            (let ((color-index (aref palette-pixels x y)))
              (setf (gethash color-index colors-used) t))))

        (let* ((color-list (loop for color being the hash-keys of colors-used collect color))
               (bg-color (if color-list (car color-list) 0))
               (fg-color (if (> (length color-list) 1) (cadr color-list) bg-color)))

          ;; Generate sprite bitmap (63 bytes: 21 rows × 3 bytes each)
          (dotimes (row 21)
            (format src-file "~%    ;; Row ~D~%" row)
            (dotimes (byte 3)  ; 3 bytes per row (24 bits)
              (let ((sprite-byte 0))
                (dotimes (bit 8)
                  (let* ((pixel-x (+ (* byte 8) bit))
                         (pixel-color (if (< pixel-x 24)
                                          (aref palette-pixels pixel-x row)
                                          bg-color)))
                    (when (= pixel-color fg-color)
                      (setf sprite-byte (logior sprite-byte (ash 1 (- 7 bit)))))))
                (format src-file "    .byte $~2,'0X~%" sprite-byte))))

          ;; Sprite color data
          (format src-file "~2%~A_colors:~%" (pathname-name png-file))
          (format src-file "    .byte $~2,'0X  ; Background color~%"
                  (logand bg-color #x0F))
          (format src-file "    .byte $~2,'0X  ; Foreground color~2%"
                  (logand fg-color #x0F))

          (format src-file ";;; Sprite descriptor
~A_descriptor:
    .word ~A_sprite     ; Sprite data pointer
    .word ~A_colors     ; Color data pointer
    .byte 24            ; Width in pixels
    .byte 21            ; Height in pixels
    .byte ~D            ; Multicolor flag (0 = normal)
~2%" (pathname-name png-file) (pathname-name png-file) (pathname-name png-file) 0))))

    (format *trace-output* "~&Compiled TED sprite: ~A (24x21 pixels)" out-file)))

(defun compile-ted-multicolor-sprite (png-file target-dir height width palette-pixels)
  "Compile TED multicolor sprite graphics (12x21 pixels, 4 colors)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; TED Multicolor Sprite compiled from ~A
;;; Commodore 16/Plus4 multicolor sprite (12x21, 4 colors)
;;; Generated automatically
~2%" png-file)

      (format src-file ";;; Multicolor sprite data: 12x21 pixels = 63 bytes bitmap + 4 bytes color
~2%")

      ;; Generate multicolor sprite bitmap data
      (format src-file "~A_sprite:~%" (pathname-name png-file))

      ;; For multicolor sprites, each pixel represents 2 bits (4 colors)
      ;; The sprite is 12 pixels wide, so each row is 24 bits = 3 bytes
      (dotimes (row 21)
        (format src-file "~%    ;; Row ~D (multicolor)~%" row)
        (dotimes (byte 3)  ; 3 bytes per row (24 bits for 12 pixels × 2 bits each)
          (let ((sprite-byte 0))
            (dotimes (pixel 4)  ; 4 pixels per byte (8 bits / 2 bits per pixel)
              (let* ((pixel-x (+ (* byte 4) pixel))
                     (color-index (if (< pixel-x 12)
                                      (aref palette-pixels pixel-x row)
                                      0)))
                ;; Pack 2-bit color index into byte
                (setf sprite-byte (logior sprite-byte
                                          (ash (logand color-index 3) (* pixel 2))))))
            (format src-file "    .byte $~2,'0X~%" sprite-byte))))

      ;; Multicolor sprite color data (4 colors)
      (format src-file "~2%~A_colors:~%" (pathname-name png-file))
      (format src-file "    .byte $00  ; Background color~%")
      (format src-file "    .byte $01  ; Color 1~%")
      (format src-file "    .byte $02  ; Color 2~%")
      (format src-file "    .byte $03  ; Color 3~2%")

      (format src-file ";;; Multicolor sprite descriptor
~A_descriptor:
    .word ~A_sprite     ; Sprite data pointer
    .word ~A_colors     ; Color data pointer
    .byte 12            ; Width in pixels
    .byte 21            ; Height in pixels
    .byte 1             ; Multicolor flag (1 = multicolor)
~2%" (pathname-name png-file) (pathname-name png-file) (pathname-name png-file)))

    (format *trace-output* "~&Compiled TED multicolor sprite: ~A (12x21 pixels)" out-file)))

(defmethod dispatch-png% ((machine (eql 222)) png-file target-dir
                          png height width α palette-pixels)
  "Dispatch PNG processing for Apple //gs graphics - temporarily disabled"
  (error "Apple //gs graphics compilation not yet implemented"))

;; vcs800 (7850) and Android reference PNGs directly from Hicolor folder;
;; no skyline-tool compilation needed.

;; Atari 400/800 support - delegate to 5200 routines
(defmethod dispatch-png% ((machine (eql 400)) png-file target-dir ; Atari 400
                          png height width α palette-pixels)
  "Atari 400 graphics dispatch - delegates to 5200 Mode E bitmap compilation"
  (format *trace-output* "~% Atari 400: delegating graphics compilation to 5200 Mode E")
  (dispatch-png% 5200 png-file target-dir png height width α palette-pixels))

(defmethod dispatch-png% ((machine (eql 800)) png-file target-dir ; Atari 800
                          png height width α palette-pixels)
  "Atari 800 graphics dispatch - delegates to 5200 Mode E bitmap compilation"
  (format *trace-output* "~% Atari 800: delegating graphics compilation to 5200 Mode E")
  (dispatch-png% 5200 png-file target-dir png height width α palette-pixels))

(defmethod dispatch-png% ((machine (eql 200)) png-file target-dir
                          png height width α palette-pixels)
  "Dispatch PNG processing for Atari Lynx"
  (cond
    ;; Sprite data (8-pixel aligned, various heights)
    ((and (zerop (mod width 8)) (<= width 64) (<= height 64))
     (format *trace-output* "~% Image ~A seems to be Lynx sprite data (~dx~d)" png-file width height)
     (compile-lynx-sprite png-file target-dir height width palette-pixels))

    ;; Tile data (8x8 tiles)
    ((and (zerop (mod width 8)) (zerop (mod height 8))
          (>= (* (/ width 8) (/ height 8)) 16))
     (format *trace-output* "~% Image ~A seems to be Lynx tile data (~dx~d tiles)" png-file (/ width 8) (/ height 8))
     (compile-lynx-tiles png-file target-dir height width palette-pixels))

    ;; Font data (8-pixel wide characters)
    ((and (= width 128) (= height 64)) ; 16x8 characters (8x8 each)
     (format *trace-output* "~% Image ~A seems to be Lynx font data (128x64)" png-file)
     (compile-lynx-font png-file target-dir height width palette-pixels))

    ;; Default case - treat as sprite
    (t
     (format *trace-output* "~% Image ~A treated as Lynx sprite data" png-file)
     (compile-lynx-sprite png-file target-dir height width palette-pixels))))

(defmethod dispatch-png% ((machine (eql 264)) png-file target-dir
                          png height width α palette-pixels)
  "Dispatch PNG processing for Commodore 16/Plus4 (TED chip)"
  (let ((monochrome-lines-p (monochrome-lines-p palette-pixels height width)))
    (cond
      ;; TED bitmap mode: 320x200 pixels (2 colors per 8x8 cell)
      ((and (= width 320) (= height 200))
       (format *trace-output* "~% Image ~A seems to be C16/Plus4 bitmap (320x200)" png-file)
       (compile-ted-bitmap png-file target-dir height width palette-pixels))

      ;; Character mode: multiples of 8x8 for character cells
      ((and (zerop (mod width 8)) (zerop (mod height 8))
            (<= width 320) (<= height 200))
       (format *trace-output* "~% Image ~A seems to be C16/Plus4 character graphics (~dx~d)" png-file width height)
       (compile-ted-charmap png-file target-dir height width palette-pixels))

      ;; Sprite data: 24x21 pixels (standard C16 sprite)
      ((and (= width 24) (= height 21))
       (format *trace-output* "~% Image ~A seems to be C16/Plus4 sprite (24x21)" png-file)
       (compile-ted-sprite png-file target-dir height width palette-pixels))

      ;; Multicolor sprite: 12x21 pixels (half width for multicolor)
      ((and (= width 12) (= height 21))
       (format *trace-output* "~% Image ~A seems to be C16/Plus4 multicolor sprite (12x21)" png-file)
       (compile-ted-multicolor-sprite png-file target-dir height width palette-pixels))

      (t (error "Don't know how to deal with C16/Plus4 image with dimensions ~
~:D×~:D pixels~:[ with monochrome lines~; without monochrome lines~]"
                width height monochrome-lines-p)))))

(defmethod dispatch-png% ((machine (eql 16)) png-file target-dir
                          png height width α palette-pixels)
  "TurboGrafx-16 / PC Engine: sprite tiles and background tiles."
  (cond
    ((and (zerop (mod width 8))
          (zerop (mod height 8)))
     (format *trace-output* "~% Image ~A seems to be TG16 tile/sprite data" png-file)
     (compile-tg16-sprite png-file target-dir height width palette-pixels))
    (t (error "Don't know how to deal with TG16 image ~D×~D" width height))))

(defmethod dispatch-png% ((machine (eql 88)) png-file target-dir
                          png height width α palette-pixels)
  "Dispatch PNG processing for Super Nintendo Entertainment System"
  (cond
    ;; Mode 7 data (256x256 affine transformation background)
    ((and (= width 256) (= height 256))
     (format *trace-output* "~% Image ~A seems to be SNES Mode 7 background" png-file)
     (compile-snes-mode7 png-file target-dir height width palette-pixels))

    ;; 8x8 tiles for sprites/background (supports 2BPP, 4BPP, 8BPP)
    ((and (zerop (mod height 8))
          (zerop (mod width 8))
          (>= (* (/ height 8) (/ width 8)) 1))
     (format *trace-output* "~% Image ~A seems to be SNES tiles (~dx~d)" png-file width height)
     (compile-snes-tiles png-file target-dir height width palette-pixels))

    ;; Sprites (16x16 or 32x32 typical for SNES)
    ((and (member width '(16 32 64)) (member height '(16 32 64)))
     (format *trace-output* "~% Image ~A seems to be SNES sprite data (~dx~d)" png-file width height)
     (compile-snes-sprite png-file target-dir height width palette-pixels))

    ;; Default case - treat as tiles
    (t
     (format *trace-output* "~% Image ~A treated as SNES tiles" png-file)
     (compile-snes-tiles png-file target-dir height width palette-pixels))))

;;; Lynx Graphics Compilation Functions

(defun compile-lynx-sprite (png-file target-dir height width palette-pixels)
  "Compile Atari Lynx sprite data (8-bit indexed colors)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "spr")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; Atari Lynx Sprite compiled from ~A
;;; Dimensions: ~Dx~D pixels
~2%" png-file width height)

      ;; Lynx sprite format: pixels stored sequentially
      (format src-file ";;; Lynx Sprite Data (1 byte per pixel, 8-bit indexed)
~A_data:~%" (pathname-name png-file))

      (dotimes (y height)
        (format src-file "    ;; Row ~D~%" y)
        (dotimes (x width)
          (let ((color-index (aref palette-pixels x y)))
            (format src-file "    .byte $~2,'0X~@[~]" color-index
                    (if (= (mod x 16) 15) "~%" "")))))
      (format src-file "~%.export ~A_data~%" (pathname-name png-file)))))

(defun compile-lynx-tiles (png-file target-dir height width palette-pixels)
  "Compile Atari Lynx tile data (8x8 tiles)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "til")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; Atari Lynx Tiles compiled from ~A
;;; Dimensions: ~Dx~D pixels (~Dx~D tiles)
~2%" png-file width height (/ width 8) (/ height 8))

      ;; Lynx tile format: 64 bytes per 8x8 tile
      (format src-file ";;; Lynx Tile Data (64 bytes per 8x8 tile)
~A_tiles:~%" (pathname-name png-file))

      (let ((tiles-across (/ width 8))
            (tiles-down (/ height 8)))
        (dotimes (tile-y tiles-down)
          (dotimes (tile-x tiles-across)
            (format src-file "~%    ;; Tile ~D,~D~%" tile-y tile-x)
            (dotimes (pixel-y 8)
              (dotimes (pixel-x 8)
                (let* ((src-x (+ (* tile-x 8) pixel-x))
                       (src-y (+ (* tile-y 8) pixel-y))
                       (color-index (if (and (< src-x width) (< src-y height))
                                        (aref palette-pixels src-x src-y)
                                        0)))
                  (format src-file "    .byte $~2,'0X~@[~]" color-index
                          (if (= pixel-x 7) "~%" ""))))))))))

  (format src-file "~%.export ~A_tiles~%" (pathname-name png-file)))

(defun compile-lynx-font (png-file target-dir height width palette-pixels)
  "Compile Atari Lynx font data (8x8 characters)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "fnt")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; Atari Lynx Font compiled from ~A
;;; Font: 8x8 characters, 16 characters wide
~2%" png-file)

      ;; Lynx font format: 8 bytes per character (8x8 bits)
      (format src-file ";;; Lynx Font Data (8 bytes per character)
~A_font:~%" (pathname-name png-file))

      ;; Process 8x8 character cells
      (dotimes (char-y 8) ; 8 characters high
        (dotimes (char-x 16) ; 16 characters wide
          (format src-file "~%    ;; Character ~D,~D~%" char-y char-x)
          (dotimes (pixel-y 8) ; 8 pixels per character
            (let ((byte 0))
              (dotimes (pixel-x 8) ; 8 pixels per character row
                (let ((x (+ (* char-x 8) pixel-x))
                      (y (+ (* char-y 8) pixel-y)))
                  (when (and (< x 128) (< y 64)
                             (> (aref palette-pixels x y) 0))
                    (setf byte (logior byte (ash 1 (- 7 pixel-x)))))))
              (format src-file "    .byte $~2,'0X~%" byte)))))
      (format src-file "~%.export ~A_font~%" (pathname-name png-file)))))

;;; SNES Graphics Compilation Functions

(defun compile-snes-mode7 (png-file target-dir height width palette-pixels)
  "Compile SNES Mode 7 background data from PNG image"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "m7")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (format *trace-output* "~&Compiling SNES Mode 7 data to ~A…" out-file)
    (let* ((png (png-read:read-png-file png-file))
           (image-height (png-read:height png))
           (image-width (png-read:width png))
           (image-data (png-read:image-data png))
           (transparency (png-read:transparency png)))
      (with-output-to-file (out out-file :element-type '(unsigned-byte 8)
                                         :if-exists :supersede)
        ;; Mode 7 data is 256x256 bytes of palette indices (8-bit per pixel)
        ;; Process the image data and convert to palette indices
        (dotimes (y 256)
          (dotimes (x 256)
            (let* ((src-x (min (1- image-width) (floor (* x (/ image-width 256.0)))))
                   (src-y (min (1- image-height) (floor (* y (/ image-height 256.0)))))
                   (color-index (if (and (< src-x image-width) (< src-y image-height))
                                    ;; Get palette index from palette-pixels (pre-converted from image)
                                    (aref palette-pixels src-x src-y)
                                    0)))
              ;; Write 8-bit palette index
              (write-byte (logand color-index #xFF) out))))))
    (format *trace-output* " done - processed ~Dx~D image into 256x256 Mode 7 data." image-width image-height)))

(defun compile-snes-tiles (png-file target-dir height width palette-pixels)
  "Compile SNES tiles from PNG image into 2BPP CHR format (16 bytes/tile).

Each 8×8 tile is encoded as two bitplanes (8 bytes low, 8 bytes high),
producing 16 bytes per tile.  Output is binary @file{.chr}."
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file) :type "chr")
                   target-dir))
        (tiles-across (/ width 8))
        (tiles-down (/ height 8)))
    (ensure-directories-exist (directory-namestring out-file))
    (format *trace-output* "~&Compiling SNES ~D×~D tiles to ~A…" tiles-across tiles-down out-file)
    (with-output-to-file (out out-file :element-type '(unsigned-byte 8)
                                       :if-exists :supersede)
      (dotimes (ty tiles-down)
        (dotimes (tx tiles-across)
          (let ((low (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0))
                (high (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
            (dotimes (y 8)
              (dotimes (x 8)
                (let* ((px (+ (* tx 8) x))
                       (py (+ (* ty 8) y))
                       (c (mod (if (and (< px width) (< py height))
                                   (aref palette-pixels px py)
                                   0)
                               4)))
                  (when (logbitp 0 c)
                    (setf (aref low y) (logior (aref low y) (ash 1 (- 7 x)))))
                  (when (logbitp 1 c)
                    (setf (aref high y) (logior (aref high y) (ash 1 (- 7 x))))))))
            ;; Write 16 bytes: 8 low bitplane, then 8 high bitplane
            (dotimes (y 8) (write-byte (aref low y) out))
            (dotimes (y 8) (write-byte (aref high y) out))))))
    (format *trace-output* " done (~D bytes).~%" (* tiles-across tiles-down 16))))

(defun compile-snes-sprite (png-file target-dir height width palette-pixels)
  "Compile SNES sprite data as 2BPP 8×8 tiles in binary @file{.spr}."
  (compile-snes-tiles png-file target-dir height width palette-pixels))

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
