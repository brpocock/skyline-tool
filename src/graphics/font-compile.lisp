(in-package :skyline-tool)

(defun compile-font-8×8 (png-file out-dir height width image-nybbles)
  "Compile 8×8 font with deduplication: unique char definitions + mapping table.

Shared by 7800, 5200, Lynx, VIC-II, VDC, ClcV, and Intv.  Outputs
assembly with @code{FontChars} (unique 8-byte glyphs) and
@code{FontCharMap} (256-entry index map).  Identical characters share
one definition slot.

@table @asis
@item PNG-FILE
Source PNG path (for comments).
@item OUT-DIR
Output directory.
@item HEIGHT, WIDTH
Pixel dimensions.
@item IMAGE-NYBBLES
2D palette-pixel array from @code{png->palette}.
@end table"
  (let* ((total (* (ceiling height 8) (ceiling width 8)))
         (uniq (make-array total :adjustable t :fill-pointer 0))
         (ht (make-hash-table :test 'equalp))
         (char-map (make-array total :element-type '(unsigned-byte 8)))
         (out-file (merge-pathnames
                    (make-pathname :name (pathname-name png-file) :type "s")
                    out-dir)))
    (ensure-directories-exist (directory-namestring out-file))
    ;; Pass 1: deduplicate
    (loop for char from 0 below total
          for x-cell = (mod (* char 8) width)
          for y-cell = (* 8 (floor (* char 8) width))
          for char-data = (extract-region image-nybbles x-cell y-cell
                                          (+ 7 x-cell) (+ 7 y-cell))
          for bits = (tile->bits char-data)
          for idx = (gethash bits ht)
          do (unless idx
               (setf idx (length uniq))
               (setf (gethash bits ht) idx)
               (vector-push-extend bits uniq))
             (setf (aref char-map char) idx))
    ;; Pass 2: write output
    (with-output-to-file (src-file out-file :if-exists :supersede)
      (format src-file ";;; -*- asm -*-~%")
      (format src-file ";;; Font compiled from ~A~%" png-file)
      (format src-file ";;; ~D characters, ~D unique glyph~:P~2%"
              total (length uniq))
      ;; Unique glyph definitions (8 bytes each)
      (format src-file "FontChars:  ;; ~D glyphs × 8 bytes~%" (length uniq))
      (loop for glyph-index from 0 below (length uniq)
            for bits = (aref uniq glyph-index)
            do (format src-file "    ;; glyph ~D~%" glyph-index)
               (loop for byte in bits
                     do (format src-file "    .byte $~2,'0X~%" byte)))
      ;; Character → glyph index map
      (format src-file "~%FontCharMap:  ;; ~D entries~%" total)
      (loop for char from 0 below total
            do (format src-file "    .byte ~D  ; char ~D~%"
                       (aref char-map char) char))
      (format src-file "~%FontUniqueCount EQU ~D~%" (length uniq))
      (format src-file "FontTotalChars  EQU ~D~%" total))
    (format *error-output* "~&Wrote font (~D unique of ~D chars) to ~A."
            (length uniq) total out-file)))

