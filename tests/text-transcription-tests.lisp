(in-package :skyline-tool/test)

;; Minifont encoder round-trip for pilcrow paragraph separator
(test minifont-encoder-paragraph-pilcrow
  "Minifont encoder handles 'Paragraph 1¶Paragraph 2' by mapping ¶ to space and round-tripping"
  (let* ((input "Paragraph 1¶Paragraph 2")
         ;; normalize like prepare-dialogue’s round-trip: lowercase + pilcrow→space
         (normalized (substitute #\Space #\¶ (string-downcase input)))
         (encoded (skyline-tool::unicode->minifont normalized))
         (decoded (skyline-tool::minifont->unicode encoded)))
    (is (typep encoded '(simple-array (unsigned-byte 8) (*))))
    (is (equalp encoded #(25 10 27 10 16 27 10 25 17 36 1 36 25 10 27 10 16 27 10 25 17 36 2)))
    (is (string= normalized decoded))))


