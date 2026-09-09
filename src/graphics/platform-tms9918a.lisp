(in-package :skyline-tool)

(defun read-colecovision-art-index (index-in)
  "Read ColecoVision art index file and return list of (png-name mode width-px height-px)"
  (let ((png-list (list)))
    (format *trace-output* "~&~A: reading ColecoVision art index …" (enough-namestring index-in))
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

(defun parse-into-colecovision-chr-data (art-index)
  "Parse PNG files into ColecoVision CHR data (8x8 tiles)"
  (let ((chr-data (list)))
    (dolist (art-item art-index)
      (destructuring-bind (mode png-name width-px height-px) art-item
        (format *trace-output* "~&~A: parsing ColecoVision CHR data … "
                png-name)
        (let* ((png (png-read:read-png-file png-name))
               (height (png-read:height png))
               (width (png-read:width png))
               (palette-pixels (png->palette (png-read:image-data png)
                                             (png-read:transparency png))))
          ;; ColecoVision CHR format: similar to NES but monochrome or limited colors
          (let ((tile-data (parse-colecovision-chr-tiles palette-pixels width-px height-px)))
            (setf chr-data (append chr-data tile-data))))
        (format *trace-output* " done.")))
    chr-data))

(defun parse-colecovision-chr-tiles (palette-pixels width-px height-px)
  "Convert palette pixels to ColecoVision CHR ROM format (8x8 tiles)"
  (let ((tiles (list))
        (tile-width (/ width-px 8))
        (tile-height (/ height-px 8)))
    ;; Process each 8x8 tile
    (dotimes (ty tile-height)
      (dotimes (tx tile-width)
        (let ((tile-bytes (make-array 8 :element-type '(unsigned-byte 8) :initial-element 0)))
          ;; ColecoVision uses 1-bit per pixel for CHR (monochrome)
          (dotimes (y 8)
            (dotimes (x 8)
              (let* ((px (+ (* tx 8) x))
                     (py (+ (* ty 8) y))
                     (pixel-value (if (and (< px width-px) (< py height-px))
                                      (aref palette-pixels py px)
                                      0)))
                ;; For ColecoVision, treat any non-zero as 1 (monochrome)
                (when (> pixel-value 0)
                  (setf (aref tile-bytes y)
                        (logior (aref tile-bytes y) (ash 1 (- 7 x))))))))
          (push tile-bytes tiles))))
    (nreverse tiles)))

(defun write-colecovision-chr-rom (index-out chr-data)
  "Write ColecoVision CHR ROM data to binary file"
  (with-output-to-file (out index-out :element-type '(unsigned-byte 8)
                                      :if-exists :supersede)
    (dolist (tile chr-data)
      (dotimes (i 8)
        (write-byte (aref tile i) out))))
  (format *trace-output* "~&Wrote ~:D bytes to ~A" (* (length chr-data) 8) index-out))

(defun compile-art-colecovision (index-out index-in)
  "Compile art assets for ColecoVision platform"
  (let ((*machine* 264))
    (write-colecovision-chr-rom index-out
                                (parse-into-colecovision-chr-data
                                 (read-colecovision-art-index index-in)))))


(defun blob-rip-tms9918 (png-file)
  "Write TMS9918-family tile-mapped blob assembly from PNG-FILE.

Divides the image into 8×8 character cells.  Deduplicates patterns (max
256 unique).  Each cell gets the dominant foreground color.  Outputs
pattern data (8 bytes per tile), name table, and color table (per
8-tile group, TMS9918 color-table format).

Output path: @file{Source/Generated/@emph{machine}/Assets/Blob.@emph{name}.s}"
  (let* ((root (uiop:ensure-directory-pathname (or (uiop:getcwd) (uiop:getcwd))))
         (out (merge-pathnames (generated-blob-assembly-pathname png-file) root))
         (label (assembler-label-name (pathname-name (merge-pathnames png-file))))
         (png (png-read:read-png-file png-file))
         (height (png-read:height png))
         (width (png-read:width png))
         (α (png-read:transparency png))
         (pal (png->palette (png-read:image-data png) α))
         (cols (floor width 8))
         (rows (floor height 8))
         (cells (* cols rows))
         (uniq (make-array 256 :adjustable t :fill-pointer 0))
         (ht (make-hash-table :test 'equal))
         (nametable (make-array cells :element-type '(unsigned-byte 8)))
         (fg-colors (make-array cells :element-type '(unsigned-byte 8))))
    (when (or (< cols 1) (< rows 1))
      (error "TMS9918 blob ~A too small (~D×~D)" png-file width height))
    (loop for cell from 0 below cells
          for row = (floor cell cols) for col = (mod cell cols)
          for sx = (* col 8) for sy = (* row 8)
          for key = (cell-binary-key-from-image pal sx sy 8 8)
          for idx = (gethash key ht)
          do (unless idx
               (setf idx (length uniq))
               (setf (gethash key ht) idx)
               (vector-push-extend key uniq))
             (setf (aref nametable cell) idx)
             (setf (aref fg-colors cell)
                   (dominant-color pal sx sy 8 8 1)))
    ;; Reduce if pattern table overflows
    (when (> (length uniq) 256)
      (warn "TMS9918 blob ~A: ~D unique patterns, reducing to 256 via H-distance..."
            png-file (length uniq))
      (reduce-tile-set uniq nametable :max-slots 256))
    (ensure-directories-exist out)
    (%write-blob-assembly-atomically
     out
     (lambda (stream)
       (format stream ";;; TMS9918 blob: ~A~%" (enough-namestring png-file))
       (format stream ";;; Grid: ~D×~D chars; ~D unique pattern~:P~2%" cols rows (length uniq))
       (format stream "~ASizeX EQU ~D~%" label cols)
       (format stream "~ASizeY EQU ~D~2%" label rows)
       ;; Pattern table: 8 bytes per tile
       (format stream "~APatterns:~%" label)
       (loop for u from 0 below (length uniq)
             for row-bytes = (aref uniq u)
             do (loop for byte in row-bytes
                      do (format stream "  .byte $~2,'0X~%" byte)))
       ;; Name table
       (format stream "~ANameTable:~%" label)
       (loop for i from 0 below cells
             do (format stream "  .byte ~D~%" (aref nametable i)))
       ;; Color table: per 8-tile group (TMS9918: upper nybble=fg, lower=bg)
       (format stream "~AColorTable:~%" label)
       (let ((groups (ceiling (length uniq) 8)))
         (loop for g from 0 below groups
               do (let ((fg 1) (bg 0))
                    ;; Most common fg color in this group of 8 tiles
                    (let ((cfreq (make-array 16 :initial-element 0)))
                      (loop for i from 0 below cells
                            for tile = (aref nametable i)
                            when (<= (* g 8) tile (1- (min (* (1+ g) 8) cells)))
                              do (incf (aref cfreq (aref fg-colors i))))
                      (dotimes (c 16)
                        (when (> (aref cfreq c) (aref cfreq fg))
                          (setf fg c))))
                    (format stream "  .byte ~D  ; group ~D~%"
                            (logior (ash (logand fg #x0F) 4) (logand bg #x0F))
                            g)))))
     (format *trace-output* "~&blob-rip-tms9918: wrote ~a~%" (enough-namestring out))
     t)))
