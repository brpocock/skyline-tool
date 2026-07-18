;; Skyline-Tool src/gui/color-chooser.lisp
;; Copyright © 2026 Interworldly Adventuring, LLC
;; Color chooser pop-up menu implementation using CLIM menu-choose

(in-package :skyline-tool)

;; ============================================================================
;; PALETTE ACCESS AND COLOR UTILS
;; ============================================================================

(defvar *instrument-palette* (list 0 0 0)
  "The current color palette for instruments")

(defun get-current-palette ()
  "Get the current palette based on machine type."
  (case *machine*
    (2600 (regions-for-machine 2600))
    (5200 (regions-for-machine 5200))
    (7800 (regions-for-machine 7800))
    (t (regions-for-machine *machine*))))

(defun get-palette-for-region (region &optional (machine *machine*))
  "Get the color palette for a specific region and machine."
  (ecase region
    (:ntsc (get-ntsc-palette machine))
    (:pal (get-pal-palette machine))
    (:secam (get-secam-palette machine))))

(defun get-ntsc-palette (machine)
  (ecase machine
    (2600 +vcs-ntsc-palette+)
    (5200 +vcs-ntsc-palette+)
    (7800 +prosystem-ntsc-palette+)))

(defun get-pal-palette (machine)
  (ecase machine
    (2600 +vcs-pal-palette+)
    (5200 +vcs-pal-palette+)
    (7800 +prosystem-pal-palette+)))

(defun get-secam-palette (machine)
  (ecase machine
    (2600 +vcs-secam-palette+)
    (5200 +vcs-secam-palette+)
    (7800 +prosystem-secam-palette+)))

(defun get-color-names-for-region (region &optional (machine *machine*))
  "Get the color names for a specific region and machine."
  (ecase region
    (:ntsc (ecase machine
             (2600 (vcs-ntsc-color-names))
             (5200 (vcs-ntsc-color-names))
             (7800 (prosystem-ntsc-color-names))))
    (:pal (ecase machine
            (2600 (vcs-pal-color-names))
            (5200 (vcs-pal-color-names))
            (7800 (prosystem-pal-color-names))))
    (:secam (ecase machine
              (2600 (mapcar #'string +vcs-secam-color-names+))
              (5200 (mapcar #'string +vcs-secam-color-names+))
              (7800 (mapcar #'string +prosystem-secam-color-names+))))))

(defun format-color-swatch (color &optional (stream *standard-output*))
  "Format a color as a print-wide-pixel swatch."
  (print-wide-pixel color stream))

;; ============================================================================
;; MAIN COLOR CHOOSER FUNCTION
;; ============================================================================

(defun color-chooser-popup (current-color &key (stream *query-io*))
  "Display a color chooser pop-up menu using CLIM's menu-choose.
Returns the selected color or NIL if cancelled."
  (let* ((regions (get-current-palette))
         (total-colors (reduce #'+ regions :key (lambda (r) (length (get-palette-for-region r)))))
         (menu-items nil))
    (dolist (region regions)
      (let* ((palette (get-palette-for-region region))
             (color-names (get-color-names-for-region region))
             (region-items nil))
        (cond
          ((< total-colors 50)
           ;; List view with region swatches for each color
           (setf region-items
                 (build-color-menu-items palette color-names)))
          ((< total-colors 257)
           ;; Grid view 16x16 per region
           (setf region-items
                 (build-grid-menu-items palette color-names)))
          (t
           ;; >256 colors: RGB/HSL entry
           (return-from color-chooser-popup
             (rgb-hsl-slider-popup current-color))))
        (push `("--- ~a ---" :region ,region :header t) region-items)
        (setf menu-items (append region-items menu-items))))
    
    (push '("Cancel" :cancel) menu-items)

    ;; Display pop-up menu
    (let ((choice (clim:menu-choose 
                    (nreverse menu-items)
                    :label "Select Color"
                    :title "Color Chooser"
                    :selected-value current-color)))
      (when (and choice (not (eq choice :cancel)))
        (getf choice :value)))))

(defun build-color-menu-items (palette color-names)
  "Build menu items for a palette in list view (<50 colors)."
  (loop for color in palette
        for name in color-names
        for index from 0
        collect (list (format nil "~a ~a ~a" 
                              (format-color-swatch color)
                              (format-color-swatch color)
                              name)
                      :value (cons color index))))

(defun build-grid-menu-items (palette color-names)
  "Build menu items for grid display (50-256 colors)."
  (let ((items nil))
    (loop for i from 0 below (min (length palette) 256)
          do (let ((color (elt palette i))
                   (name (if (< i (length color-names)) 
                              (elt color-names i) 
                              (format nil "$~2,'0x" i))))
               (push (list (format-color-swatch color)
                            :value (cons color i))
                     items)))
    (nreverse items)))

;; ============================================================================
;; RGB/HSL SLIDER POPUP FOR >256 COLORS
;; ============================================================================

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
  (let ((h (clim:accept 'integer :prompt "Hue (0-360): "))
        (s (clim:accept 'real :prompt "Saturation (0-1): "))
        (l (clim:accept 'real :prompt "Luminance (0-1): ")))
    (values (hsl-to-rgb h s l))))

(defun hsl-to-rgb (h s l)
  "Convert HSL to RGB."
  (let* ((h (mod h 360))
         (c (* (1- (abs (- 1.0 (* 2.0 l)))) s))
         (x (* c (- 1.0 (abs (- (/ h 60.0) (floor (/ h 60.0)) 2))))
         (m (- l (/ c 2.0))))
    (multiple-value-bind (r g b)
        (ecase (floor (/ h 60.0))
          (0 (values c x 0))
          (1 (values x c 0))
          (2 (values 0 c x))
          (3 (values 0 x c))
          (4 (values x 0 c))
          (5 (values c 0 x)))
      (list (round (* 255 (+ r m)))
            (round (* 255 (+ g m)))
            (round (* 255 (+ b m)))))))

;; ============================================================================
;; COLOR PANE FOR USE IN COMPLEX INTERFACE
;; ============================================================================

(defclass color-chooser-pane (clim:gadget-pane)
  ((value :initarg :value :accessor color-chooser-value)
   (value-changed-callback :initarg :value-changed-callback :accessor color-chooser-callback))
  (:default-initargs :display-function 'display-color-chooser))

(defun display-color-chooser (pane stream)
  "Display function for the color chooser pane."
  (let ((value (color-chooser-value pane)))
    (format stream "Color: $~2,'0x$~2,'0x$~2,'0x" (first value) (second value) (third value))
    (let ((selected (color-chooser-popup value :stream stream)))
      (when selected
        (setf (color-chooser-value pane) selected)
        (when (color-chooser-callback pane)
          (funcall (color-chooser-callback pane) pane))))))