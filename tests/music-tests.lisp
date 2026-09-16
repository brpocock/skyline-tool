(in-package :skyline-tool/test)

(defun test-distortion-factors ()
  "Test distortion factor calculation."
  (is (= (%distortion-factor nil) 1.0d0) "NIL distortion factor")
  (is (= (%distortion-factor :10) 0.994d0) ":10 distortion factor")
  (is (= (%distortion-factor :8) 0.987d0) ":8 distortion factor")
  (is (= (%distortion-factor :invalid) 1.0d0) "Invalid distortion factor"))

(defun test-bits-divisor ()
  "Test bits divisor calculation."
  (is (= (%bits-divisor 0) 1) "0 bits divisor")
  (is (= (%bits-divisor 1) 2) "1 bit divisor")
  (is (= (%bits-divisor 2) 4) "2 bits divisor")
  (is (= (%bits-divisor 3) 8) "3 bits divisor")
  (is (<= (%bits-divisor 4) 16) "Bits overflow handled"))

(defun test-frequency->pokey ()
  "Test frequency to AUDF conversion with error calculation."
  ;; Test exact match case
  (multiple-value-bind (audf error)
      (frequency->pokey (+ 15699.9 2.0))
    (declare (ignore audf))
    (is (< error 0.001) "Small error for near-exact match"))
  
  ;; Test NTSC case
  (multiple-value-bind (audf error)
      (frequency->pokey 440.0 :ntsc)
    (is (typep audf '(integer 0 255)) "AUDF is valid integer")
    (is (<= 0.0 error 1.0) "Error is normalized [0.0, 1.0]"))
  
  ;; Test PAL case
  (multiple-value-bind (audf error)
      (frequency->pokey 440.0 :pal)
    (is (typep audf '(integer 0 255)) "AUDF is valid integer")
    (is (<= 0.0 error 1.0) "Error is normalized [0.0, 1.0]")))

(defun test-best-pokey-note-for ()
"Test best POKEY note function with distortion and bits."
;; Test without distortion
(multiple-value-bind (audf error)
    (best-pokey-note-for 60 nil 0 :ntsc)  ; C2
  (is (typep audf '(integer 0 255)) "AUDF is valid integer")
  (is (<= 0.0 error 1.0) "Error is normalized"))

;; Test with distortion
(multiple-value-bind (audf error)
    (best-pokey-note-for 60 :10 0 :ntsc)
  (is (typep audf '(integer 0 255)) "AUDF is valid with distortion")
  (is (<= 0.0 error 1.0) "Error is normalized with distortion"))

;; Test with bits divisor
(multiple-value-bind (audf error)
    (best-pokey-note-for 60 nil 3 :ntsc)  ; Divide by 8
  (is (typep audf '(integer 0 255)) "AUDF is valid with bits")
  (is (<= 0.0 error 1.0) "Error is normalized with bits")))

(defun test-pokey-error-normalization ()
"Test that error values follow the expected normalization."
;; Test edge cases
(multiple-value-bind (audf error)
    (frequency->pokey 15699.9 :ntsc)  ; exact frequency
  (is (<= error 0.01) "Near-zero error for exact match"))

(multiple-value-bind (audf error)
    (frequency->pokey 15699.9 2.0 :ntsc)  ; half frequency
  (is (<= 0.0 error 1.0) "Error is within bounds for half frequency")))

(defun test-pokey-audf-clamping ()
  "Test that AUDF values are clamped to 0-255 range."
  (multiple-value-bind (audf error)
      (frequency->pokey 100000000.0 :ntsc)  ; Very high frequency → AUDF=0
    (is (= audf 0) "High frequency → AUDF=0"))
  
  (multiple-value-bind (audf error)
      (frequency->pokey 0.1 :ntsc)  ; Very low frequency → AUDF=255
    (is (= audf 255) "Low frequency → AUDF=255")))

(defun test-all-pokey-functions ()
  "Run all POKEY function tests."
  (test-distortion-factors)
  (test-bits-divisor)
  (test-frequency->pokey)
  (test-best-pokey-note-for)
  (test-pokey-error-normalization)
  (test-pokey-audf-clamping))