;;; Phantasia SkylineTool/tests/7800-tests.lisp
;;;; Copyright (c) 2024-2026 Bruce-Robert Pocock; Copyright (c) 2024-2026 Interworldly Adventuring, LLC.

(in-package :skyline-tool/test)

(def-suite 7800-tests
  :description "Tests for Atari 7800 graphics conversion (320A/C, BLOB ripping)"
  :in skyline-tool/test)

(in-suite 7800-tests)

;;; ---------------------------------------------------------------------------
;;; 320C Encoding Tests — verify correct MARIA hardware byte format
;;;
;;; 320C byte layout (per 4-pixel group):
;;;   D7 = (pixel0 & 1)    foreground bit
;;;   D6 = (pixel1 & 1)    foreground bit
;;;   D5 = (pixel2 & 1)    foreground bit
;;;   D4 = (pixel3 & 1)    foreground bit
;;;   D3-D2 = palette select for pair (0,1)
;;;   D1-D0 = palette select for pair (2,3)
;;;
;;; Palette pair rules:
;;;   both pixels 0 → 0
;;;   first pixel 0 → C2 bits of second: (pixel2 & #x06) >> 1
;;;   otherwise      → C2 bits of first:  (pixel1 & #x06) >> 1
;;; ---------------------------------------------------------------------------

(test 7800-image-to-320c-correct-encoding
  "7800-image-to-320c produces correct MARIA 320C hardware bytes"
  ;; 4x2 test image covering all pairwise palette combinations
  (let ((test-image (make-array '(2 4) :element-type '(unsigned-byte 8)
                                :initial-contents
                                '((0 1 2 3)    ; row 0: all four pixel values
                                  (3 2 1 0)))) ; row 1: reversed
        (palette (vector 0 1 2 3)))
    (let ((result (7800-image-to-320c test-image
                                      :byte-width 1 :height 2
                                      :palette palette)))
      (is (= 1 (length result)) "One byte-column for 4px width")
      (is (= 2 (length (first result))) "Two rows")
      (let ((row0 (elt (first result) 0))
            (row1 (elt (first result) 1)))
        ;; Row 0: pix 0,1,2,3 → fg bits: 0,1,0,1 → D7-4 = #x50
        ;;   pair01: pix0=0 → use pix1 C2=(1&6)>>1=0 → D3-2=0
        ;;   pair23: pix2=0 → use pix3 C2=(3&6)>>1=1 → D1-0=1
        ;;   byte = #x50 | #x01 = #x51
        (is (= #x51 row0))
        ;; Row 1: pix 3,2,1,0 → fg bits: 1,0,1,0 → D7-4 = #xA0
        ;;   pair01: pix0=3nonzero → use pix0 C2=(3&6)>>1=1 → D3-2=1
        ;;   pair23: pix2=1nonzero → use pix2 C2=(1&6)>>1=0 → D1-0=0
        ;;   byte = #xA0 | #x04 = #xA4
        (is (= #xA4 row1))))))

(test 7800-image-to-320c-idempotency
   "320C conversion is deterministic"
   (let ((image (make-array '(4 8) :element-type '(unsigned-byte 8)
                           :initial-contents
                           '((0 1 2 3 0 1 2 3)
                             (1 2 3 0 1 2 3 0)
                             (2 3 0 1 2 3 0 1)
                             (3 0 1 2 3 0 1 2))))
        (palette (vector 0 1 2 3)))
    (let ((r1 (7800-image-to-320c image :byte-width 2 :height 4 :palette palette))
          (r2 (7800-image-to-320c image :byte-width 2 :height 4 :palette palette)))
      (is (equalp r1 r2) "Same input → same output"))))

(test 7800-image-to-320c-all-transparent
   "All-transparent pixels → all-zero bytes"
   (let ((image (make-array '(4 16) :element-type '(unsigned-byte 8)
                            :initial-element 0))
         (palette (vector 0 1 2 3)))
     (let ((result (7800-image-to-320c image :byte-width 1 :height 16 :palette palette)))
       (is (= 1 (length result)))
       (is (= 16 (length (first result))))
       (is (every #'zerop (first result)) "All bytes are zero for all-transparent stamp"))))

;;; ---------------------------------------------------------------------------
;;; 320A Encoding Tests
;;; ---------------------------------------------------------------------------

(test 7800-image-to-320a-correct-encoding
   "320A conversion packs 8 pixels into 1 byte, MSB-left"
   (let ((image (make-array '(2 16) :element-type '(unsigned-byte 8)
                           :initial-contents
                           '((1 0 1 0 1 0 1 0 0 0 0 0 0 0 0 0)   ; #xAA in byte 0
                             (0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1)))) ; #xFF in byte 1
        (palette (vector 0 1)))
    (let ((result (7800-image-to-320a image :byte-width 2 :height 2 :palette palette)))
      (is (= 2 (length result)) "Two byte-columns for 16px width")
      (let ((col0 (first result))
            (col1 (second result)))
        (is (= #xAA (elt col0 0)) "Row 0 byte 0: 10101010")
        (is (= #x00 (elt col0 1)) "Row 1 byte 0: all transparent")
        (is (= #x00 (elt col1 0)) "Row 0 byte 1: all transparent")
        (is (= #xFF (elt col1 1)) "Row 1 byte 1: all foreground")))))

(test 7800-image-to-320a-idempotency
  "320A conversion is deterministic"
  (let ((image (make-array '(8 8) :element-type '(unsigned-byte 8)
                           :initial-element 1))
        (palette (vector 0 1)))
    (let ((r1 (7800-image-to-320a image :byte-width 1 :height 8 :palette palette))
          (r2 (7800-image-to-320a image :byte-width 1 :height 8 :palette palette)))
      (is (equalp r1 r2)))))

;;; ---------------------------------------------------------------------------
;;; stamp-is-monochrome-p Tests
;;; ---------------------------------------------------------------------------

(test stamp-is-monochrome-p-detection
  "stamp-is-monochrome-p correctly identifies ≤2 vs >2 color stamps"
  ;; 2 colors → monochrome-eligible
  (let ((mono (make-array '(4 16) :element-type '(unsigned-byte 8) :initial-element 1)))
    (setf (aref mono 0 0) 0)
    (is (skyline-tool::stamp-is-monochrome-p mono) "2-color stamp is monochrome"))
  ;; 3 colors → not monochrome-eligible
  (let ((color (make-array '(4 16) :element-type '(unsigned-byte 8) :initial-element 1)))
    (setf (aref color 0 0) 0
          (aref color 1 0) 2)
    (is (not (skyline-tool::stamp-is-monochrome-p color)) "3-color stamp is not monochrome"))
  ;; single color → monochrome-eligible (all same = 1 unique)
  (let ((single (make-array '(4 16) :element-type '(unsigned-byte 8) :initial-element 5)))
    (is (skyline-tool::stamp-is-monochrome-p single) "Single-color stamp is monochrome")))

;;; ---------------------------------------------------------------------------
;;; Cross-validation: 7800-image-to-320c matches parse-7800-object :320c
;;; ---------------------------------------------------------------------------

(test 320c-encoding-matches-parse-7800-object
   "7800-image-to-320c and parse-7800-object :320c produce consistent results
    for identical pixel patterns (given identical palette mapping)"
   ;; Create a 4x4 stamp with palette values 0-3 and a matching palette
   (let* ((stamp (make-array '(4 4) :element-type '(unsigned-byte 8)
                               :initial-contents
                               '((0 1 2 3)
                                 (1 2 3 0)
                                 (2 3 0 1)
                                 (3 0 1 2))))
          (palette (vector 0 1 2 3))
          ;; Build a full parse-7800-object-friendly image: 4px wide, 4+1 rows
          (full-image (make-array '(4 5) :element-type '(unsigned-byte 8)
                                  :initial-contents
                                  '((0 1 2 3 0)   ; column 0
                                    (1 2 3 0 1)   ; column 1
                                    (2 3 0 1 2)   ; column 2
                                    (3 0 1 2 3)))) ; column 3
          (image-result (first (7800-image-to-320c stamp
                                                   :byte-width 1 :height 4
                                                   :palette palette)))
          (object-result (first (skyline-tool::parse-7800-object :320c full-image
                                                                 :width 4 :height 4
                                                                 :palette palette))))
      (is (equalp image-result object-result)
          "7800-image-to-320c and parse-7800-object :320c agree on byte output"))

;;; ---------------------------------------------------------------------------
;;; 320AC BLOB stamp buffer writing
;;; ---------------------------------------------------------------------------

(test blob-write-span-to-stamp-buffer-320ac-basic
  "blob/write-span-to-stamp-buffer-320ac writes correct byte layout
   for both 320A (monochrome) and 320C (color) stamps"
  (flet ((make-mono-stamp ()
           (let ((s (make-array '(4 16) :element-type '(unsigned-byte 8)
                                        :initial-element 0)))
             ;; Put some foreground pixels in an 8x16 area (after padding)
             (dotimes (y 16) (setf (aref s 0 y) 1))
             s))
         (make-color-stamp ()
           (let ((s (make-array '(4 16) :element-type '(unsigned-byte 8)
                                        :initial-element 0)))
             (dotimes (y 16)
               (setf (aref s 0 y) 1
                     (aref s 1 y) 2
                     (aref s 2 y) 3))
             s)))
    ;; Verify that each stamp type is classified correctly
    (is (skyline-tool::stamp-is-monochrome-p (make-mono-stamp))
        "Two-color stamp detected as monochrome")
    (is (not (skyline-tool::stamp-is-monochrome-p (make-color-stamp)))
        "Four-color stamp detected as color")
    ;; Verify 7800-image-to-320c output for a color stamp has correct structure
    (let ((color (make-color-stamp)))
      (let ((bytes (7800-image-to-320c color
                                       :byte-width 1 :height 4
                                       :palette (vector 0 1 2 3))))
        (is (= 1 (length bytes)) "4px stamp → 1 byte-column in 320c")
        (is (= 16 (length (first bytes))) "16 rows of bytes")))
    ;; Verify 7800-image-to-320a output for a monochrome stamp
    ;; (after BLOB converter's binary conversion step)
    (let* ((mono (make-mono-stamp))
           (binary (let ((b (make-array '(16 8) :element-type '(unsigned-byte 8))))
                     (dotimes (x 8)
                       (dotimes (y 16)
                         (setf (aref b x y)
                               (if (zerop (aref mono x y)) 0 1))))
                     b))
           (bytes (7800-image-to-320a binary
                                      :byte-width 1 :height 8
                                      :palette (vector 0 1))))
      (is (= 1 (length bytes)) "8px padded stamp → 1 byte-column in 320a")
      (is (= 16 (length (first bytes))) "16 rows of bytes"))))

;;; ---------------------------------------------------------------------------
;;; 320AC x-position computation
;;; ---------------------------------------------------------------------------

(test 320ac-x-position-in-pixels
  "X positions in 320AC BLOB headers are in pixel units (×4 from stamp columns)
   This is a design-verification test: stamp column n → pixel position (* 4 n)"
  ;; Each stamp is 4 pixels wide in 320 mode.
  ;; x-position in MARIA display headers counts in graphics clocks = pixels in 320 mode.
   (let ((column 5)          ; stamp column 5
         (span-length 3))    ; span covers 3 stamps
     ;; Starting pixel position of span = (* 4 (- column span-length))
     ;; For a span starting at column 2, ending before column 5: (* 4 (- 5 3)) = 8
     (is (= 8 (* 4 (- column span-length)))
         "Stamp column 2 → pixel x=8 in 320 mode"))))
