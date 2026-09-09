(in-package :skyline-tool)

(defun extract-regions (pixels width height)
  "Split PIXELS into regions of WIDTH×HEIGHT"
  (let ((images (list)))
    (dotimes (y (floor (/ (array-dimension pixels 1) height)))
      (dotimes (x (floor (/ (array-dimension pixels 0) width)))
        (push (extract-region pixels
                              (* x width) (* y height)
                              (* (1+ x) width) (* (1+ y) height))
              images)))
    (nreverse images)))

(define-condition color-not-in-palette-error (error)
  ((x :initarg :x :reader color-not-in-palette-x)
   (y :initarg :y :reader color-not-in-palette-y)
   (i :initarg :i :reader color-not-in-palette-i)
   (image :initarg :image :reader color-not-in-palette-image)
   (pixel :initarg :pixel :reader color-not-in-palette-pixel)
   (palette :initarg :palette :reader color-not-in-palette-palette)
   (image-pixels :initarg :image-pixels :reader color-not-in-palette-image-pixels)))

(defmethod print-object ((c color-not-in-palette-error) s)
  (format s "The color found in the image data was not found in the palette:~%")
  (let ((i (color-not-in-palette-i c))
        (img (color-not-in-palette-image c)))
    (format s "At ~a, ~a" (color-not-in-palette-x c) (color-not-in-palette-y c))
    (when (and i (not (emptyp i)))
      (format s " at index ~a" i))
    (when (and img (not (emptyp img)))
      (format s " in image ~s" img))
    (format s "~%"))
  (format s "Pixel: ")
  (print-wide-pixel (color-not-in-palette-pixel c) s)
  (format s "~%Palette:")
  (dolist (p (coerce (color-not-in-palette-palette c) 'list))
    (format s " ")
    (print-wide-pixel p s))
  #+mcclim
  (when (typep s 'clim:sheet)
    (format s "~%Image region:")
    (%print-clim-pixels (color-not-in-palette-image-pixels c) s)))

(defun pixel-into-palette (pixel palette &key x0 y0 x i image best-fit-p)
  (check-type pixel (integer 0 #xff))
  (let ((index (or (position pixel palette)
                   (when best-fit-p
                     (destructuring-bind (r g b) (elt (machine-palette) pixel)
                       (position (find-nearest-in-palette (mapcar (lambda (i) (elt (machine-palette) i))
                                                                  (coerce palette 'list))
                                                          r g b)
                                 (mapcar (lambda (c) (elt (machine-palette) c))
                                         (coerce palette 'list))
                                 :test 'equalp))))))
    (or index
        (error 'color-not-in-palette-error
               :pixel pixel
               :x (if x (if x0 (+ x0 x) x) "?")
               :y (if y0 y0 "?")
               :i (if i (format nil "in image ~d " i) "")
               :image image
               :palette palette))))

(defun ansi-color-rgb (r g b &optional (foregroundp t))
  (format nil "~c[~d;2;~d;~d;~dm"
          #\Escape (if foregroundp 38 48) (round r) (round g) (round b)))

(defun ansi-color-pixel (r g b)
  (format nil "~a~a██~c[0m"
          (ansi-color-rgb r g b)
          (ansi-color-rgb r g b nil)
          #\Escape))

(defun pixels-to-ansi (pixels &key x y)
  (flet ((tb ()
           (terpri)
           (princ (ansi-color-pixel 0 0 0))
           (dotimes (x0 (array-dimension pixels 0))
             (princ (ansi-color-pixel 0 0 (if (eql x0 x) #xff 0))))
           (princ (ansi-color-pixel 0 0 0))
           (format t "~c[0m" #\Escape)))
    (format t "~& Image (~:d×~:d pixels):"
            (array-dimension pixels 0)
            (array-dimension pixels 1))
    (tb)
    (dotimes (y0 (array-dimension pixels 1))
      (terpri)
      (princ (ansi-color-pixel 0 0 (if (eql y y0) #xff 0)))
      (dotimes (x0 (array-dimension pixels 0))
        (destructuring-bind (r g b) (palette->rgb (aref pixels x0 y0))
          (princ (ansi-color-pixel r g b))))
      (princ (ansi-color-pixel 0 0 (if (eql y y0) #xff 0)))
      (format t "~c[0m" #\Escape))
    (tb)
    (terpri)
    (finish-output)))

(defun pixels-to-clim (pixels &key x y (stream t))
  (let ((s (or stream t)))
    (flet ((tb ()
             (terpri)
             (print-clim-pixel (list 0 0 0) s)
             (dotimes (x0 (array-dimension pixels 0))
               (print-clim-pixel (let ((val (if (= x x0) #xff 0))) (list val val val)) s))
             (print-clim-pixel (list 0 0 0) s)))
      (format t "~& Image (~:d×~:d pixels):"
              (array-dimension pixels 0)
              (array-dimension pixels 1))
      (tb)
      (dotimes (y0 (array-dimension pixels 1))
        (terpri)
        (print-clim-pixel (list 0 0 (if (eql y y0) #xff 0)) s)
        (dotimes (x0 (array-dimension pixels 0))
          (destructuring-bind (r g b) (palette->rgb (aref pixels x0 y0))
            (print-clim-pixel (list r g b) s)))
        (print-clim-pixel (list 0 0 (if (eql y y0) #xff 0)) s))
      (tb)
      (terpri)
      (finish-output))))

(defun pixels-to-ansi-string (pixels &key x y)
  (with-output-to-string (*standard-output*)
    (pixels-to-ansi pixels :x x :y y)))

(defun pixels-into-palette (pixels palette &key x0 y0 i best-fit-p image)
  "Assign every one of PIXELS to fit within PALETTE.

Optional X0, Y0, I are used for messaging, indicating that X0, Y0 is the
position within a larger image I."
  (assert (= 1 (array-dimension pixels 1)))
  (let* ((width (array-dimension pixels 0))
         (output (make-array (list width) :element-type '(unsigned-byte 8))))
    (dotimes (x width)
      (let ((pixel (aref pixels x 0)))
        (setf (aref output x)
              (pixel-into-palette pixel palette
                                  :x0 x0 :y0 y0 :x x :i i
                                  :image image
                                  :best-fit-p best-fit-p))))
    output))
