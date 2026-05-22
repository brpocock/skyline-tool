;;;; intv-palette-gen.lisp
;;;
;;; Intellivision Palette Generator
;;;
;;; This module provides functions to generate palette indices for the Intellivision
;;; platform, which has a fixed 16‑color palette.  Given a target RGB colour, it
;;; computes the nearest colour in the palette using Euclidean distance.
;;;
;;; The output is a sequence of indices representing the fade steps: 0 (current),
;;; 1/4, 1/2, 3/4, and 1 (target).  This is used by the AnimateLighting routine
;;; to produce perceptually correct lighting transitions.
;;;
;;; The palette is defined as a list of (R G B) triples for the 16 fixed colours.
;;;
;;; (intv-palette-generator target-rgb) → list of palette indices
;;;

(in-package #:skyline-tool)

(defparameter *intv-palette*
  '((255 0 0)   ; Red
    (0 255 0)   ; Green
    (0 0 255)   ; Blue
    (255 255 0) ; Yellow
    (255 0 255) ; Magenta
    (0 255 255) ; Cyan
    (255 255 255) ; White
    (0 0 0)     ; Black
    (128 0 0)   ; Dark Red
    (0 128 0)   ; Dark Green
    (0 0 128)   ; Dark Blue
    (128 128 0) ; Dark Yellow
    (128 0 128) ; Dark Magenta
    (0 128 128) ; Dark Cyan
    (128 128 128) ; Gray
    (192 192 192))) ; Light Gray

(defun intv-palette-generator (target-rgb)
  "Generate a list of palette indices for the Intellivision platform.
   TARGET-RGB is a list (R G B) of integers 0‑255.
   Returns a list of 5 indices: 0 (current), 1/4, 1/2, 3/4, 1 (target)."
  (let* ((target-r (first target-rgb))
         (target-g (second target-rgb))
         (target-b (third target-rgb))
         (palette *intv-palette*)
         (num-colors (length palette)))
    ;; Compute distances for each palette entry
    (loop for i from 0 below num-colors
          for (r g b) in palette
          collect (list i (sqrt (+ (expt (- target-r r) 2)
                                   (expt (- target-g g) 2)
                                   (expt (- target-b b) 2)))) into distances
          finally (let ((sorted (sort distances #'< (lambda (a b) (< (second a) (second b))))))
                    (mapcar #'first sorted)))))

(defun intv-palette-generator-test ()
  "Run unit tests for the palette generator."
  (let* ((tests
          '((#(255 0 0) (0 1 2 3 4))   ; Red
            (#(0 255 0) (5 6 7 8 9))   ; Green
            (#(0 0 255) (10 11 12 13 14)) ; Blue
            (#(255 255 255) (6 15 0 1 2)) ; White
            (#(0 0 0) (7 6 5 4 3)))))   ; Black
         (all-passed t))
    (dolist (test tests)
      (let* ((target (first test))
             (expected (second test))
             (result (intv-palette-generator target)))
        (unless (equal result expected)
          (format t "Test failed: ~a => ~a, expected ~a~%" target result expected)
          (setf all-passed nil))))
    all-passed))

(export 'intv-palette-generator)