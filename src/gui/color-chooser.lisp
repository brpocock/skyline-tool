;; Skyline-Tool src/gui/color-chooser.lisp
;; Copyright © 2026 Interworldly Adventuring, LLC

;; Color chooser pop-up menu — gadget-based using menu-button-color-leaf-pane

(in-package :skyline-tool)

(defvar *instrument-palette* (list 0 0 0)
  "The current color palette for instruments")


;;; SWATCH RENDERING

(defun draw-region-swatches (pane color-index regions)
  "Draw a row of region swatches for COLOR-INDEX into PANE.
   Each swatch shows the palette color at COLOR-INDEX in that region."
  (let ((machine *machine*)
        (x 2)
        (y-spacing 1))
    (dolist (region regions)
      (when-let ((palette (ignore-errors
                           (palette-for-machine-and-region machine region))))
        (when (< color-index (length palette))
          (destructuring-bind (r g b) (elt palette color-index)
            (let ((ink (make-rgb-color (/ r 255.0) (/ g 255.0) (/ b 255.0)))
                  (swatch-size 12))
              (draw-rectangle* pane x y (+ x swatch-size) (+ y swatch-size)
                               :ink ink :filled t)
              (draw-rectangle* pane x y (+ x swatch-size) (+ y swatch-size)
                               :ink +black+ :filled nil :line-thickness 1)
              (incf x (+ swatch-size 2)))))))
    x))


;;; COLOR LEAF PANE FOR MENU-BAR SUB-MENUS

(defclass menu-button-color-leaf-pane (clim-internals::menu-button-leaf-pane)
  ((color-index :initarg :color-index :initform 0
                :accessor menu-button-color-index)
   (regions :initarg :regions :initform nil :accessor menu-button-regions))
  (:default-initargs
   :label ""
   :x-spacing 4 :y-spacing 2))

(defmethod handle-repaint ((pane menu-button-color-leaf-pane) region)
  (declare (ignore region))
  (with-bounding-rectangle* (x1 y1 x2 y2) (sheet-region pane)
    (let* ((x-spacing (pane-x-spacing pane))
           (y-spacing (pane-y-spacing pane))
           (inner-x1 (+ x1 x-spacing))
           (inner-y1 (+ y1 y-spacing))
           (inner-y2 (- y2 y-spacing)))
      ;; Background
      (draw-rectangle* pane x1 y1 x2 y2
                       :ink +white+ :filled t)
      ;; Arm/selection border — simple rectangle when armed
      (when (gadget-armed-p pane)
        (draw-rectangle* pane x1 y1 x2 y2
                         :ink +black+ :filled nil :line-thickness 2))
      ;; Selection indicator
      (let* ((color-index (menu-button-color-index pane))
             (current-colors (machine-colors))
             (is-current (and color-index
                              (< color-index (length current-colors))
                              (= color-index
                                 (or (and (boundp '*current-color*) *current-color*)
                                     -1)))))
        (draw-text* pane (if is-current "● " "○ ")
                    inner-x1 (+ inner-y1 8)
                    :ink +black+))
      ;; Region swatches
      (let ((regions (or (menu-button-regions pane)
                         (ignore-errors (regions-for-machine)))))
        (when regions
          (let ((swatch-end (draw-region-swatches pane
                                                  (menu-button-color-index pane)
                                                  regions)))
            ;; Color name
            (let* ((color-index (menu-button-color-index pane))
                   (color-names (machine-colors))
                   (name (and color-index (< color-index (length color-names))
                              (elt color-names color-index))))
              (when name
                (draw-text* pane name
                            (+ swatch-end 4)
                            (+ inner-y1 8)
                            :ink +black+)))))))))

(defmethod compose-space ((pane menu-button-color-leaf-pane) &key width height)
  (declare (ignore width height))
  (let* ((regions (or (menu-button-regions pane)
                      (ignore-errors (regions-for-machine))))
         (swatch-region-width (* (length regions) (+ 12 2)))
         (name-space (compose-label-space pane))
         (tw (space-requirement-width name-space))
         (th (space-requirement-height name-space))
         (spacing (pane-x-spacing pane))
         (yspacing (pane-y-spacing pane)))
    (make-space-requirement
     :min-width (+ (* 2 spacing) 24 swatch-region-width tw)
     :width (+ (* 2 spacing) 24 swatch-region-width tw)
     :max-width +fill+
     :min-height (+ (* 2 yspacing) th 4)
     :height (+ (* 2 yspacing) th 4)
     :max-height +fill+)))


;;; COLOR CHOOSER POP-UP — menu-choose with region swatches

(defvar *current-color* nil
  "Currently-selected palette index for ●/○ indicator.")

(defun color-chooser-popup (current-color palette)
  "Display a color chooser pop-up.
Returns the selected color index or NIL if cancelled.
For <50 colors: list with region swatches per named color.
For 50-256 colors: grid of palette-color swatches.
For >256 colors: RGB/HSL entry dialog."
  (let* ((total-colors (length palette))
         (color-names (ignore-errors (machine-colors))))
    (cond
      ((< total-colors 50)
       (color-list-popup current-color palette color-names))
      ((< total-colors 257)
       (color-grid-popup current-color palette color-names))
      (t
       (rgb-hsl-slider-popup current-color)))))

(defun color-list-popup (current-color palette color-names)
  "Show a color list pop-up with region swatches per entry."
  (let* ((regions (ignore-errors (regions-for-machine)))
         (machine *machine*)
         (items (loop with seen = (make-hash-table :test 'equal)
                      for i from 0 below (length palette)
                      for name = (and color-names
                                      (< i (length color-names))
                                      (elt color-names i))
                      unless (and name (gethash name seen))
                        do (when name (setf (gethash name seen) t))
                        and collect
                        `(,(or name (format nil "$~2,'0x" i))
                          :value ,i
                          :color-index ,i
                          :current-p ,(= i (or current-color -1))))))
    (push '("Cancel" :value :cancel) items)
    (let ((*current-color* current-color))
      (clim:menu-choose (nreverse items)
                        :label "Color Chooser"
                        :printer (lambda (item stream)
                                   (color-menu-printer
                                    item stream regions machine))))))

(defun color-menu-printer (item stream regions machine)
  "Print a color menu item with indicator, region swatch text, and name."
  (let* ((name (first item))
         (options (cdr item))
         (color-index (getf options :color-index)))
    ;; Selection indicator
    (princ (if (getf options :current-p) "◆ " "◇ ") stream)
    ;; Region swatches — draw inline rectangles at current cursor position
    (dolist (region regions)
      (when-let ((palette (ignore-errors
                            (palette-for-machine-and-region machine region))))
        (when (and color-index (< color-index (length palette)))
          (destructuring-bind (r g b) (elt palette color-index)
            (let ((ink (make-rgb-color (/ r 255.0) (/ g 255.0) (/ b 255.0))))
              (with-room-for-graphics (stream :height 12 :width 12)
                (draw-rectangle* stream 0 0 12 12 :ink ink :filled t)
                (draw-rectangle* stream 0 0 12 12 :ink +black+ :filled nil
                                 :line-thickness 1))
              (princ " " stream))))))
    ;; Color name
    (princ name stream)))

(defun color-grid-popup (current-color palette color-names)
  "Show a grid color pop-up for 50-256 colors.
Current selection gets black+white outline ring."
  (let* ((items (loop for i from 0 below (min (length palette) 256)
                      for color = (elt palette i)
                      for name = (and color-names
                                      (< i (length color-names))
                                      (elt color-names i))
                      collect
                      `(,(or name (format nil "$~2,'0x" i))
                        :value ,i
                        :color ,color
                        :current-p ,(= i (or current-color -1))
                        :style (:fix :roman :small)))))
    (push '("Cancel" :value :cancel) items)
    (clim:menu-choose (nreverse items)
                      :label "Color Chooser (Grid)"
                      :n-columns 8
                      :max-width 400
                      :printer (lambda (item stream)
                                 (color-grid-printer item stream)))))

(defun color-grid-printer (item stream)
  "Print a grid color swatch with selection outline."
  (let* ((name (first item))
         (options (cdr item))
         (color (getf options :color))
         (current-p (getf options :current-p)))
    (with-room-for-graphics (stream :height 24 :min-width 24)
      (let ((size 20)
            (offset 2))
        (when color
          (destructuring-bind (r g b) color
            (let ((ink (make-rgb-color (/ r 255.0) (/ g 255.0) (/ b 255.0))))
              ;; Selection outline ring: white outer, black inner
              (when current-p
                (draw-rectangle* stream (- offset 2) (- offset 2)
                                 (+ offset size 2) (+ offset size 2)
                                 :ink +white+ :filled nil :line-thickness 2)
                (draw-rectangle* stream (- offset 1) (- offset 1)
                                 (+ offset size 1) (+ offset size 1)
                                 :ink +black+ :filled nil :line-thickness 2))
              ;; Color swatch
              (draw-rectangle* stream offset offset
                               (+ offset size) (+ offset size)
                               :ink ink :filled t)
              (draw-rectangle* stream offset offset
                               (+ offset size) (+ offset size)
                               :ink +black+ :filled nil :line-thickness 1))))))
    ;; Name label below swatch
    (format stream "~%~a" name)))


;; RGB/HSL SLIDER POPUP FOR >256 COLORS


(defun rgb-hsl-slider-popup (current-color &key (stream *query-io*))
  "Display RGB/HSL slider dialog for high-color palettes.
Returns the selected color or NIL if cancelled."
  (let ((choice (clim:menu-choose
                 `(("Enter RGB values..." :rgb)
                   ("Enter HSL values..." :hsl)
                   ("Cancel" :cancel))
                 :label "High Color Mode (>256 colors)"
                 :title "Color Chooser")))
    (ecase choice
      (:rgb (read-rgb-from-user current-color))
      (:hsl (read-hsl-from-user current-color))
      (:cancel nil))))

(defun read-rgb-from-user (current-color)
  "Read RGB values from user."
  (declare (ignore current-color))
  (let ((r (clim:accept 'integer :prompt "Red (0-255): "))
        (g (clim:accept 'integer :prompt "Green (0-255): "))
        (b (clim:accept 'integer :prompt "Blue (0-255): ")))
    (values (list r g b))))

(defun read-hsl-from-user (current-color)
  "Read HSL values from user."
  (declare (ignore current-color))
  (format *query-io* "~&HSL color input not yet implemented.~%")
  nil)


;; COLOR PANE FOR USE IN COMPLEX INTERFACE


(defclass color-chooser-pane (clim::gadget-pane)
  ((value :initarg :value :accessor color-chooser-value)
   (value-changed-callback :initarg :value-changed-callback :accessor color-chooser-callback))
  (:default-initargs :display-function 'display-color-chooser))

(defun display-color-chooser (pane stream)
  "Display function for the color chooser pane."
  (let ((value (color-chooser-value pane))
        (palette (ignore-errors (machine-palette))))
    (format stream "Color: $~2,'0x$~2,'0x$~2,'0x"
            (first value) (second value) (third value))
    (when palette
      (let ((color-index (position value palette :test 'equal)))
        (let ((selected (color-chooser-popup color-index palette)))
          (when selected
            (setf (color-chooser-value pane) (elt palette selected))
            (when (color-chooser-callback pane)
              (funcall (color-chooser-callback pane) pane))))))))
