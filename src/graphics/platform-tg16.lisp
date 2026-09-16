(in-package :skyline-tool)

(defun compile-tg16-tile-data (palette-pixels tile-x tile-y)
  "Convert an 8x8 tile from palette-pixels to TG16 planar format (32 bytes)"
  (let ((tile-bytes (make-array 32 :element-type '(unsigned-byte 8) :initial-element 0)))
    (declare (type (simple-array (unsigned-byte 8) (*)) tile-bytes))
    ;; TG16 uses planar format: 4 bitplanes of 8 bytes each
    (dotimes (y 8)
      (dotimes (x 8)
        (let* ((global-x (+ (* tile-x 8) x))
               (global-y (+ (* tile-y 8) y))
               (color-index (if (and (< global-x (array-dimension palette-pixels 0))
                                     (< global-y (array-dimension palette-pixels 1)))
                                (aref palette-pixels global-x global-y)
                                0))) ; Default to color 0 for out-of-bounds
          ;; Ensure color index is valid (0-15 for 4-bit)
          (when (> color-index 15)
            (warn "TG16 tile color index ~D exceeds 4-bit limit, truncating to ~D" color-index (logand color-index 15))
            (setf color-index (logand color-index 15)))

          ;; Set bits in the 4 bitplanes
          (dotimes (bit 4)
            (when (logbitp bit color-index)
              (let ((byte-index (+ (* bit 8) y)))
                (setf (aref tile-bytes byte-index)
                      (logior (aref tile-bytes byte-index) (ash 1 (- 7 x))))))))))
    tile-bytes))

(defun compile-tg16-sprite (png-file target-dir height width palette-pixels)
  "Compile TurboGrafx-16/PC Engine sprite data from PNG image"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; TG16 Sprite data compiled from ~A
;;; TurboGrafx-16/PC Engine sprite format
;;; Dimensions: ~Dx~D pixels (4-bit color)
~2%" png-file width height)
      
      ;; TG16 sprite data format: 4-bit pixels, planar arrangement
      (let* ((sprite-width-tiles (/ width 8))   ; in 8-pixel units
             (sprite-height-tiles (/ height 8))  ; in 8-pixel units
             (total-tiles (* sprite-width-tiles sprite-height-tiles)))
        
        (format src-file ";;; Sprite dimensions: ~Dx~D tiles (~D total tiles)
;;; Each tile: 32 bytes (4 bitplanes x 8 bytes)
;;; Total sprite data: ~D bytes
~2%" sprite-width-tiles sprite-height-tiles total-tiles (* total-tiles 32))
        
        ;; Generate sprite data structure
        (format src-file "~A_sprite_data:~%" (pathname-name png-file))
        
        ;; For each 8x8 tile in the sprite (row-major order)
        (dotimes (tile-y sprite-height-tiles)
          (dotimes (tile-x sprite-width-tiles)
            (format src-file "~%    ;; Tile (~D,~D) - bytes ~D-~D~%"
                    tile-x tile-y (* (+ tile-x (* tile-y sprite-width-tiles)) 32)
                    (+ (* (+ tile-x (* tile-y sprite-width-tiles)) 32) 31))
            
            ;; Convert tile to TG16 planar format
            (let ((tile-bytes (compile-tg16-tile-data palette-pixels tile-x tile-y)))
              ;; Output 4 bitplanes of 8 bytes each
              (dotimes (bitplane 4)
                (format src-file "~&          .text x\"")
                (dotimes (byte 8)
                  (let ((byte-index (+ (* bitplane 8) byte)))
                    (format src-file "~2,'0x" (aref tile-bytes byte-index))))
                (format src-file "\""))))
          
          (format src-file "~2%;;; Sprite descriptor for HuC6270 VDC
~A_sprite_descriptor:
    .byte ~D  ; Width in tiles
    .byte ~D  ; Height in tiles
    .word ~A_sprite_data  ; Data pointer
    .word ~D  ; Total tiles
~2%" (pathname-name png-file) sprite-width-tiles sprite-height-tiles (pathname-name png-file) total-tiles)))
      (format *trace-output* "~&Compiled TG16 sprite: ~A (~Dx~D tiles, ~D bytes)"
              out-file sprite-width-tiles sprite-height-tiles (* sprite-width-tiles sprite-height-tiles 32)))))

(defun compile-tg16-background (png-file target-dir height width palette-pixels)
  "Compile TurboGrafx-16/PC Engine background data - TEMPORARILY DISABLED DUE TO COMPILATION ERROR"
  (error "TG16 background compilation temporarily disabled due to compilation error"))
