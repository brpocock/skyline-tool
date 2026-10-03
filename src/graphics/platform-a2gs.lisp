(in-package :skyline-tool)

(defun compile-a2gs-super-hires (png-file target-dir height width palette-pixels)
  "Compile Apple //gs Super Hi-Res graphics (320x200, 16 colors)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; Apple //gs Super Hi-Res graphics compiled from ~A
;;; 320x200 pixels, 16 colors (4-bit)
;;; Generated automatically
~2%" png-file)

      (format src-file ";;; Super Hi-Res memory layout: 4 bitplanes x 800 bytes each
;;; Total: 3200 bytes for 320x200 pixels
~2%")

      ;; Generate bitplane data
      (format src-file "~A_data:~%" (pathname-name png-file))

      ;; Apple //gs Super Hi-Res uses 4 bitplanes
      (dotimes (bitplane 4)
        (format src-file "~%    ;; Bitplane ~D (bit ~D of color index)~%" bitplane bitplane)
        (format src-file "    .byte ")

        ;; Each bitplane has 800 bytes (320x200 pixels / 8 bits per byte)
        (dotimes (byte 800)
          (let ((byte-value 0))
            ;; Calculate which pixels this byte represents
            (dotimes (bit 8)
              (let* ((pixel-index (+ (* byte 8) bit))
                     (x (mod pixel-index 320))
                     (y (/ pixel-index 320)))
                (when (< y 200)  ; Ensure we don't go beyond image height
                  (let ((color-index (if (and (< x (array-dimension palette-pixels 0))
                                              (< y (array-dimension palette-pixels 1)))
                                         (aref palette-pixels x y)
                                         0)))
                    ;; Extract the specific bit from the color index
                    (when (logbitp bitplane color-index)
                      (setf byte-value (logior byte-value (ash 1 (- 7 bit)))))))))

            (if (= byte 799)
                (format src-file "$~2,'0X~%" byte-value)
                (format src-file "$~2,'0X, " byte-value))))

        ;; Start new line every 16 bytes for readability
        (when (= (mod (1+ byte) 16) 0)
          (format src-file "~%    .byte ")))

      ;; Add palette information
      (format src-file "~2%;;; Palette data (16 colors)
~A_palette:
    ;; Apple //gs 16-color palette entries
    ;; Each entry is a 16-bit RGB value: 00000RRRRRGGGGGBBBBB
    .word $0000, $0000, $0000, $0000  ; Colors 0-3 (placeholder)
    .word $0000, $0000, $0000, $0000  ; Colors 4-7 (placeholder)
    .word $0000, $0000, $0000, $0000  ; Colors 8-11 (placeholder)
    .word $0000, $0000, $0000, $0000  ; Colors 12-15 (placeholder)
~2%" (pathname-name png-file))

      ;; Add graphics descriptor
      (format src-file ";;; Graphics descriptor
~A_descriptor:
    .word ~A_data      ; Graphics data pointer
    .word ~A_palette   ; Palette pointer
    .word 320          ; Width in pixels
    .word 200          ; Height in pixels
    .byte 16           ; Number of colors
    .byte 4            ; Bits per pixel
~2%" (pathname-name png-file) (pathname-name png-file) (pathname-name png-file))))

  (format *trace-output* "~&Compiled Apple //gs Super Hi-Res: ~A (320x200, 16 colors, 3200 bytes)"
          out-file))

(defun compile-a2gs-double-hires (png-file target-dir height width palette-pixels)
  "Compile Apple //gs Double Hi-Res graphics (560x192, 16 colors)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; Apple //gs Double Hi-Res graphics compiled from ~A
;;; 560x192 pixels, 16 colors (4-bit)
;;; Generated automatically
~2%" png-file)

      (format src-file ";;; Double Hi-Res memory layout: 4 bitplanes x 1344 bytes each
;;; Total: 5376 bytes for 560x192 pixels
;;; Note: Apple //gs Double Hi-Res uses 560 pixels horizontally
~2%")

      ;; Generate bitplane data
      (format src-file "~A_data:~%" (pathname-name png-file))

      ;; Apple //gs Double Hi-Res uses 4 bitplanes
      (dotimes (bitplane 4)
        (format src-file "~%    ;; Bitplane ~D (bit ~D of color index)~%" bitplane bitplane)
        (format src-file "    .byte ")

        ;; Each bitplane has 1344 bytes (560x192 pixels / 8 bits per byte)
        (dotimes (byte 1344)
          (let ((byte-value 0))
            ;; Calculate which pixels this byte represents
            (dotimes (bit 8)
              (let* ((pixel-index (+ (* byte 8) bit))
                     (x (mod pixel-index 560))
                     (y (/ pixel-index 560)))
                (when (< y 192)  ; Ensure we don't go beyond image height
                  (let ((color-index (if (and (< x (array-dimension palette-pixels 0))
                                              (< y (array-dimension palette-pixels 1)))
                                         (aref palette-pixels x y)
                                         0)))
                    ;; Extract the specific bit from the color index
                    (when (logbitp bitplane color-index)
                      (setf byte-value (logior byte-value (ash 1 (- 7 bit)))))))))

            (if (= byte 1343)
                (format src-file "$~2,'0X~%" byte-value)
                (format src-file "$~2,'0X, " byte-value))))

        ;; Start new line every 16 bytes for readability
        (when (= (mod (1+ byte) 16) 0)
          (format src-file "~%    .byte ")))

      ;; Add graphics descriptor
      (format src-file "~2%;;; Graphics descriptor
~A_descriptor:
    .word ~A_data      ; Graphics data pointer
    .word 560          ; Width in pixels
    .word 192          ; Height in pixels
    .byte 16           ; Number of colors
    .byte 4            ; Bits per pixel
~2%" (pathname-name png-file) (pathname-name png-file))))

  (format *trace-output* "~&Compiled Apple //gs Double Hi-Res: ~A (560x192, 16 colors, 5376 bytes)"
          out-file))

(defun compile-a2gs-hires (png-file target-dir height width palette-pixels)
  "Compile Apple //gs Hi-Res graphics (280x192, 6 colors)"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; Apple //gs Hi-Res graphics compiled from ~A
;;; 280x192 pixels, 6 colors (NTSC artifact colors)
;;; Generated automatically
~2%" png-file)

      (format src-file ";;; Hi-Res memory layout: 3 bitplanes x 768 bytes each
;;; Total: 2304 bytes for 280x192 pixels
;;; Uses NTSC artifact colors for 6-color palette
~2%")

      ;; Generate bitplane data
      (format src-file "~A_data:~%" (pathname-name png-file))

      ;; Apple //gs Hi-Res uses 3 bitplanes (for 6 colors, but actually uses 2-bit encoding + NTSC artifacts)
      (dotimes (bitplane 3)
        (format src-file "~%    ;; Bitplane ~D~%" bitplane)
        (format src-file "    .byte ")

        ;; Each bitplane has 768 bytes (280x192 pixels / 8 bits per byte)
        (dotimes (byte 768)
          (let ((byte-value 0))
            ;; Calculate which pixels this byte represents
            (dotimes (bit 8)
              (let* ((pixel-index (+ (* byte 8) bit))
                     (x (mod pixel-index 280))
                     (y (/ pixel-index 280)))
                (when (< y 192)  ; Ensure we don't go beyond image height
                  (let ((color-index (if (and (< x (array-dimension palette-pixels 0))
                                              (< y (array-dimension palette-pixels 1)))
                                         (aref palette-pixels x y)
                                         0)))
                    ;; For Hi-Res, we use 3 bits but only 6 colors are actually distinct
                    ;; due to NTSC artifacting
                    (when (and (< color-index 8) (logbitp bitplane (logand color-index 7)))
                      (setf byte-value (logior byte-value (ash 1 (- 7 bit)))))))))

            (if (= byte 767)
                (format src-file "$~2,'0X~%" byte-value)
                (format src-file "$~2,'0X, " byte-value))))

        ;; Start new line every 16 bytes for readability
        (when (= (mod (1+ byte) 16) 0)
          (format src-file "~%    .byte ")))

      ;; Add graphics descriptor
      (format src-file "~2%;;; Graphics descriptor
~A_descriptor:
    .word ~A_data      ; Graphics data pointer
    .word 280          ; Width in pixels
    .word 192          ; Height in pixels
    .byte 6            ; Number of colors (NTSC artifact)
    .byte 3            ; Effective bits per pixel
~2%" (pathname-name png-file) (pathname-name png-file)))

    (format *trace-output* "~&Compiled Apple //gs Hi-Res: ~A (280x192, 6 colors, 2304 bytes)"
            out-file)))

(defun compile-a2gs-sprite (png-file target-dir height width palette-pixels)
  "Compile Apple //gs sprite graphics"
  (let ((out-file (merge-pathnames
                   (make-pathname :name (pathname-name png-file)
                                  :type "s")
                   target-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; Apple //gs sprite compiled from ~A
;;; ~Dx~D pixels
;;; Generated automatically
~2%" png-file width height)

      ;; Determine sprite format based on size
      (cond
        ((and (= width 16) (= height 16))
         (format src-file ";;; 16x16 sprite (small sprite)~%"))
        ((and (= width 32) (= height 32))
         (format src-file ";;; 32x32 sprite (medium sprite)~%"))
        ((and (= width 64) (= height 64))
         (format src-file ";;; 64x64 sprite (large sprite)~%"))
        (t
         (format src-file ";;; Custom sprite size~%")))

      ;; For sprites, we can use Super Hi-Res format but smaller
      (let* ((bytes-per-bitplane (* (/ width 8) height)) ; width/8 bytes per line × height lines
             (total-bytes (* bytes-per-bitplane 4))) ; 4 bitplanes

        (format src-file ";;; Sprite data: ~D bytes per bitplane, ~D bitplanes, ~D total bytes
~2%" bytes-per-bitplane 4 total-bytes)

        ;; Generate sprite data
        (format src-file "~A_data:~%" (pathname-name png-file))

        (dotimes (bitplane 4)
          (format src-file "~%    ;; Bitplane ~D~%" bitplane)
          (format src-file "    .byte ")

          (dotimes (byte bytes-per-bitplane)
            (let ((byte-value 0))
              ;; Calculate pixel data for this byte
              (dotimes (bit 8)
                (let* ((pixel-in-byte (+ (* byte 8) bit))
                       (x-in-sprite (mod pixel-in-byte width))
                       (y-in-sprite (/ pixel-in-byte width)))
                  (when (< y-in-sprite height)
                    (let ((color-index (if (and (< x-in-sprite (array-dimension palette-pixels 0))
                                                (< y-in-sprite (array-dimension palette-pixels 1)))
                                           (aref palette-pixels x-in-sprite y-in-sprite)
                                           0)))
                      (when (logbitp bitplane color-index)
                        (setf byte-value (logior byte-value (ash 1 (- 7 bit)))))))))

              (if (= byte (1- bytes-per-bitplane))
                  (format src-file "$~2,'0X~%" byte-value)
                  (format src-file "$~2,'0X, " byte-value))))

          (when (= (mod (1+ bitplane) 4) 0)
            (format src-file "~%")))

        ;; Add sprite descriptor
        (format src-file "~2%;;; Sprite descriptor
~A_descriptor:
    .word ~A_data      ; Sprite data pointer
    .word ~D           ; Width in pixels
    .word ~D           ; Height in pixels
    .byte 16           ; Colors per sprite
    .byte 4            ; Bits per pixel
~2%" (pathname-name png-file) (pathname-name png-file) width height))))

  (format *trace-output* "~&Compiled Apple //gs sprite: ~A (~Dx~D pixels)"
          out-file width height))


(defun compile-art-a2 (index-out index-in)
  "Compile art assets for Apple II HIRES graphics mode"
  (let ((*machine* 2))
    (write-a2-art-index index-out
                        (read-a2-art-index index-in))))

;; (defun compile-art-a2e (index-out index-in)
;;   "Compile art assets for Apple //e Double HIRES graphics mode"
;;   (let ((*machine* 23))
;;     (write-a2e-art-index index-out
;;                         (read-a2e-art-index index-in))))

(defun read-a2-art-index (index-in)
  "Read Apple II HIRES art index file and return list of (png-name width height)"
  (let ((png-list (list)))
    (format *trace-output* "~&Apple II HIRES: reading art index ~A…" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while line
            do (let ((trimmed (string-trim " " line)))
                 (when (and (> (length trimmed) 0)
                            (not (char= (char trimmed 0) #\;)))
                   (let* ((parts (split-sequence #\space trimmed :remove-empty-subseqs t))
                          (png-name (first parts))
                          (width (parse-integer (second parts)))
                          (height (parse-integer (third parts))))
                     (push (list png-name width height) png-list))))))
    (nreverse png-list)))


(defun write-a2-art-index (index-out png-list)
  "Write Apple II HIRES art assembly code"
  (format *trace-output* "~&Apple II HIRES: writing art data …")
  (with-output-to-file (out index-out :if-exists :supersede :if-does-not-exist :create)
    (format out ";;; Apple II HIRES Art Assets compiled from index
;;; Generated for Apple II HIRES graphics (280x192, 6 colors)
~2%")
    (dolist (png-entry png-list)
      (destructuring-bind (png-name width height) png-entry
        (format out ";;; ~A: ~Dx~D pixels~%" png-name width height)
        (format out "~A_data:~%" (pathname-name png-name))

        ;; FIXME
        ;; Apple II HIRES stores 7 pixels per byte (140 bytes per line)
        (dotimes (y (ceiling height 192)) ; Handle multiple screens if needed
          (format out "~%    ;; Screen ~D~%" y)
          (dotimes (line 192)           ; 192 scan lines
            (format out "~%    ;; Line ~D~%" line)
            ;; 40 bytes per line (280 pixels / 7 pixels per byte = 40 bytes)
            (format out "~{~%~10t.byte $~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~^,   ~
$~2,'0x~^, $~2,'0x~^, $~2,'0x~^, $~2,'0x~}" bytes)))
        (format out "~%    ;; End of ~A data~2%" png-name)))))

(defun write-a2e-art-index (index-out png-list)
  "Write Apple //e Double HIRES art assembly code"
  (format *trace-output* "~&Apple //e Double HIRES: writing art data …")
  (with-output-to-file (out index-out :if-exists :supersede :if-does-not-exist :create)
    (format out ";;; Apple //e Double HIRES Art Assets compiled from index
;;; Generated for Apple //e Double HIRES graphics (560x192, 16 colors)
~2%")

    (dolist (png-entry png-list)
      (destructuring-bind (png-name width height) png-entry
        (format out ";;; ~A: ~Dx~D pixels~%" png-name width height)
        (format out "~A_data:~%" (pathname-name png-name))

        ;; Generate placeholder Double HIRES bitmap data
        ;; Double HIRES stores 7 pixels per byte in both main and aux memory
        ;; 80 bytes per line (560 pixels / 7 pixels per byte = 80 bytes)
        (dotimes (y (ceiling height 192)) ; Handle multiple screens if needed
          (format out "~%    ;; Screen ~D~%" y)
          (dotimes (line 192) ; 192 scan lines
            (format out "~%    ;; Line ~D - Main memory~%" line)
            ;; Main memory: 40 bytes per line
            (dotimes (byte 40)
              (if (= byte 39)
                  (format out "    .byte $00~%")
                  (format out "    .byte $00, ")))
            (format out "~%    ;; Line ~D - Aux memory~%" line)
            ;; Aux memory: 40 bytes per line
            (dotimes (byte 40)
              (if (= byte 39)
                  (format out "    .byte $00~%")
                  (format out "    .byte $00, ")))
            
            (format out "~%    ;; End of ~A data~2%" png-name)))
        (format out "~%    ;; Double HIRES color palette constants (16 colors)~%")
        (let ((color-list '("BLACK" "DARK_GRAY" "MEDIUM_GRAY" "LIGHT_GRAY"
                            "LIGHT_GRAY2" "VERY_LIGHT_GRAY" "VERY_LIGHT_GRAY2" "WHITE"
                            "RED" "LIGHT_RED" "MAGENTA" "LIGHT_MAGENTA"
                            "BLUE" "LIGHT_BLUE" "GREEN" "LIGHT_GREEN")))
          (dotimes (i (length color-list))
            (format out "DHGR_~A = $~2,'0X~%" (nth i color-list) i)))))))

(defun compile-art-a2gs (index-out index-in)
  "Compile art assets for Apple //gs platform"
  (let ((*machine* 222))
    (write-a2gs-art-index index-out
                          (read-a2gs-art-index index-in))))

(defun read-a2gs-art-index (index-in)
  "Read Apple //gs art index file"
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading Apple //gs art index …" (enough-namestring index-in))
    (with-input-from-file (index index-in)
      (loop for line = (read-line index nil)
            while (and line (plusp (length line)) (not (char= #\; (char line 0))))
            do (let ((line (string-trim #(#\Space #\Tab #\Newline #\Return #\Page) line)))
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

(defun write-a2gs-art-index (index-out art-index)
  "Write Apple //gs art data to output file"
  (with-output-to-file (out index-out :if-exists :supersede :if-does-not-exist :create)
    (format out ";;; Apple //gs Art Assets compiled
;;; Generated automatically
~2%")
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&Processing Apple //gs art: ~A (~Dx~D)…" png-name width-px height-px)
        ;; Dispatch to appropriate compilation function based on mode
        (ecase mode
          (:super-hires
           (compile-a2gs-super-hires png-name (directory-namestring index-out) height-px width-px
                                     (png->palette
                                      (png-read:image-data (png-read:read-png-file png-name))
                                      (png-read:transparency (png-read:read-png-file png-name)))))
          (:double-hires
           (compile-a2gs-double-hires png-name (directory-namestring index-out) height-px width-px
                                      (png->palette
                                       (png-read:image-data (png-read:read-png-file png-name))
                                       (png-read:transparency (png-read:read-png-file png-name)))))
          (:hires
           (compile-a2gs-hires png-name (directory-namestring index-out) height-px width-px
                               (png->palette 
                                (png-read:image-data (png-read:read-png-file png-name))
                                (png-read:transparency (png-read:read-png-file png-name)))))
          (:sprite
           (compile-a2gs-sprite png-name (directory-namestring index-out) height-px width-px
                                (png->palette
                                 (png-read:image-data (png-read:read-png-file png-name))
                                 (png-read:transparency (png-read:read-png-file png-name)))))))))
  (format *trace-output* "~&Apple //gs art compilation complete."))
