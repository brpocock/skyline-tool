;;;; intv-palette-gen.lisp
;;;
;;; Intellivision Palette Generator
;;;
;;; Implements the requirements from Req-Intv-PaletteGen.md per TDD.
;;; Uses the shared +intv-palette+ from graphics.lisp .
;;;
;;; (intv-palette-generator target-rgb) → list of 5 palette indices for ¼-step fades from black

(in-package #:skyline-tool)

(defun %euclid-dist (a b)
  (sqrt (+ (expt (- (first a) (first b)) 2)
           (expt (- (second a) (second b)) 2)
           (expt (- (third a) (third b)) 2))))

(defun %nearest-intv-index (rgb)
  (let ((min-dist 1e9) (best 0) (palette +intv-palette+))
    (loop for i from 0 below (length palette)
          for col in palette
          for d = (%euclid-dist rgb col)
          when (< d min-dist) do (setf min-dist d best i))
    best))

(defun intv-palette-generator (target-rgb)
  "Generate a list of 5 palette indices for ¼-step fades (0, 1/4, 1/2, 3/4, 1.0) from black to TARGET-RGB.
Uses Euclidean nearest match in +intv-palette+.
TARGET-RGB may be list or vector of 3 integers."
  (let* ((tgt (if (vectorp target-rgb) (coerce target-rgb 'list) target-rgb))
         (steps '(0 0.25 0.5 0.75 1.0))
         (start '(0 0 0)))
    (mapcar (lambda (f)
              (let ((r (round (+ (* f (first tgt)) (* (- 1 f) (first start)))))
                    (g (round (+ (* f (second tgt)) (* (- 1 f) (second start)))))
                    (b (round (+ (* f (third tgt)) (* (- 1 f) (third start))))))
                (%nearest-intv-index (list r g b))))
            steps)))

(defun intv-palette-as-json (target-rgb)
  "JSON interface (byteArrayToJSON style) returning the 5 indices as JSON array string."
  (json:encode-json-to-string (intv-palette-generator target-rgb)))

(defun intv-palette-command (&rest args)
  "CLI handler for --intv-palette --input JSONFILE --output PALFILE
Reads list of RGB targets from JSON, for each calls generator, writes all indices as raw bytes to .pal"
  (let* ((parsed (let ((plist nil) (r args))
                   (loop while r
                         do (let ((k (pop r)))
                              (cond ((string= k "--input") (setf (getf plist :input) (pop r)))
                                    ((string= k "--output") (setf (getf plist :output) (pop r)))
                                    (t (error "Unknown arg ~a" k)))))
                   plist))
         (input (getf parsed :input))
         (output (getf parsed :output)))
    (unless (and input output)
      (error "Usage: --intv-palette --input foo.json --output bar.pal"))
    (let* ((targets (with-open-file (f input :direction :input)
                      (json:decode-json f)))
           (all-bytes (loop for tgt in targets
                            append (intv-palette-generator (if (vectorp tgt) (coerce tgt 'list) tgt)))))
      (ensure-directories-exist output)
      (with-open-file (out output :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (dolist (b all-bytes)
          (write-byte b out)))
      (format t "~&Wrote ~d bytes to ~a~%" (length all-bytes) output))))

(export 'intv-palette-generator)
(export 'intv-palette-command)
(export 'intv-palette-as-json)
