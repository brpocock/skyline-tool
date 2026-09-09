;;; Skyline-Tool src/gui/gui-character-appearance.lisp
;;; Appearance tab for Character Inspector

(in-package :skyline-tool)

;; HELPERS

(defun common-palette-index (name-string)
  "Return the index (0-11) of NAME-STRING in *common-palette*."
  (when name-string
    (position name-string *common-palette*
              :key (lambda (entry) (string-downcase (string (first entry))))
              :test #'string=)))

;; DECAL-KIND SPECIFIC HELPERS
(defun decal-kind-head-type (kind)
  "Return the head UI type for a decal kind: :slider, :radio-box, :number"
  (case kind
    (:human    :slider)    ; 1-10 slider for human heads
    (:merfolk  :slider)    ; 1-15 slider for merfolk heads
    (:enemy    :number)    ; enemy: numeric (for memory bank?)
    (t         :number)))  ; default: numeric field

(defun decal-kind-body-type (kind)
  "Return the body UI type for a decal kind: :radio-box, :number"
  (case kind
    (:human :radio-box)    ; human: radio box (Robe/Tunic)
    (t      :number)))     ; all others: numeric field

;; PRESENTATION TYPES — clickable color fields in editing view

(clim:define-presentation-type hair-color-field ()
  :inherit-from 'string)

(clim:define-presentation-type skin-color-field ()
  :inherit-from 'string)

(clim:define-presentation-type clothes-color-field ()
  :inherit-from 'string)

;; COMMANDS — also invoked from Edit menu (gui-character.lisp)

(macrolet ((def-color-command (name slot)
             `(clim:define-command (,name :menu nil :name t)
                  ((color-name 'string :gesture :select :default nil))
                (let* ((frame clim:*application-frame*)
                       (resource (frame-resource frame))
                       (current (or color-name (,slot resource)))
                       (selected (common-palette-chooser-popup current)))
                  (when selected
                    (setf (,slot resource) selected)
                    (publish :resource-changed :payload resource))))))
  (def-color-command com-char-edit-hair game-resource-character-hair-color)
  (def-color-command com-char-edit-skin game-resource-character-skin-color)
  (def-color-command com-char-edit-clothes game-resource-character-clothes-color))

;; COMMON PALETTE CHOOSER POPUP

(defun common-palette-chooser-popup (current-color-name)
  "Show a color chooser for the 12 common palette colors.
Each entry shows an NTSC swatch, PAL swatch, and the color name.
Returns the selected color name string or NIL."
  (let* ((regions (regions-for-machine))
         (machine *machine*)
         (items (loop for entry in *common-palette*
                      for sym = (first entry)
                      for lower-name = (string-downcase (string sym))
                      for display-name = (title-case lower-name)
                      collect `(,display-name
                                :value ,lower-name
                                :rgb (,(second entry) ,(third entry) ,(fourth entry))
                                :current-p ,(string-equal lower-name current-color-name)))))
    (push '("Cancel" :value :cancel) items)
    (let ((selected (clim:menu-choose (nreverse items)
                                      :label "Color Chooser"
                                      :printer (lambda (item stream)
                                                 (common-palette-menu-printer
                                                  item stream regions machine)))))
      (when (eq selected :cancel)
        (return-from common-palette-chooser-popup nil))
      selected)))

(defun common-palette-menu-printer (item stream regions machine)
  "Print a common palette menu item: indicator, NTSC/PAL swatches, name."
  (let* ((display-name (first item))
         (options (cdr item))
         (rgb (getf options :rgb))
         (current-p (getf options :current-p))
         (r (first rgb))
         (g (second rgb))
         (b (third rgb)))
    (princ (if current-p "◆ " "◇ ") stream)
    (dolist (region regions)
      (let ((palette (palette-for-machine-and-region machine region)))
        (when palette
          (let ((closest (find-nearest-in-palette palette r g b)))
            (clim:with-room-for-graphics (stream :height 12 :width 12)
              (clim:draw-rectangle* stream 0 0 12 12
                                    :ink clim:+white+
                                    :filled t)
              (clim:draw-rectangle* stream 0 0 12 12
                                    :ink clim:+black+ :filled nil :line-thickness 1))
            (princ " " stream)))))
    (princ display-name stream)))

;; TAB DISPATCH

(defmethod display-appearance-tab ((frame character-inspector-frame) pane)
  (ecase (frame-view-mode frame)
    (:reading (display-appearance-reading frame pane))
    (:editing (display-appearance-editing frame pane))))

(defmethod display-appearance-reading ((frame character-inspector-frame) pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (display-appearance-reading-decal resource pane)
      (display-character-reading-appearance-hair resource pane)
      (display-character-reading-appearance-skin resource pane)
      (display-character-reading-appearance-clothes resource pane)
      (display-character-reading-appearance-head resource pane)
      (display-character-reading-appearance-body resource pane))))

(defun display-appearance-reading-decal (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Decal Kind: "))
    (clim:formatting-cell (pane :align-x :left)
      (format pane "~a" (decal-display-name (game-resource-character-decal resource))))))

(defun display-color-reference (resource pane slot)
  (let* ((name (funcall slot resource))
         (index (common-palette-index name)))
    (when index
      (destructuring-bind (sym r g b) (elt *common-palette* index)
        (declare (ignore sym))
        (print-wide-pixel (rgb->palette r g b) pane))
      (format pane "  ~a" (title-case name)))))

(defun display-character-reading-appearance-hair (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Hair Color: "))
    (clim:formatting-cell (pane :align-x :left)
      (display-color-reference resource pane #'game-resource-character-hair-color))))

(defun display-appearance-reading-skin (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Skin Color: "))
    (clim:formatting-cell (pane :align-x :left)
      (display-color-reference resource pane #'game-resource-character-skin-color))))

(defun display-appearance-reading-clothes (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Clothes Color: "))
    (clim:formatting-cell (pane :align-x :left)
      (display-color-reference resource pane #'game-resource-character-clothes-color))))

(defun display-character-reading-appearance-head (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Head: "))
    (clim:formatting-cell (pane :align-x :left)
      (format pane "~a" (game-resource-character-head resource)))))

(defun display-character-reading-appearance-body (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Body: "))
    (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-body resource)))))

;; EDITING VIEW — gadget-based with clickable color swatches

(defmethod display-appearance-editing ((frame character-inspector-frame) pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane :multiple-columns t)
      (display-appearance-editing-decal resource pane)
      (display-appearance-editing-hair resource pane)
      (display-appearance-editing-skin resource pane)
      (display-appearance-editing-clothes resource pane)
      (display-appearance-editing-head resource pane)
      (display-appearance-editing-body resource pane)
      (display-animation-sequences-section resource pane))))

(defun display-appearance-editing-decal (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Decal Kind: "))
    (clim:formatting-cell (pane :align-x :left)
      (clim:make-pane 'clim:menu-button-pane
                      :label (decal-display-name (game-resource-character-decal resource))
                      :items (mapcar (lambda (k) (cons (decal-kind->display-name k) k)) +decal-kinds+)
                      :value-changed-callback
                      (lambda (gadget value)
                        (declare (ignore gadget))
                        (setf (game-resource-character-decal resource) value)
                        (publish :resource-changed :payload resource))))))

(defun display-editing-color-swatch (resource pane slot presentation-type)
  (let ((name (funcall slot resource)))
    (clim:with-output-as-presentation (pane name presentation-type)
      (let ((index (common-palette-index name)))
        (when index
          (destructuring-bind (sym r g b) (elt *common-palette* index)
            (declare (ignore sym))
            (let* ((machine-index (rgb->palette r g b))
                   (machine-rgb (elt (machine-palette) machine-index)))
              (clim:with-room-for-graphics (pane :height 16 :width 24)
                (clim:draw-rectangle* pane 0 0 24 16
                                      :ink (clim:make-rgb-color
                                            (/ (first machine-rgb) 255.0)
                                            (/ (second machine-rgb) 255.0)
                                            (/ (third machine-rgb) 255.0))
                                      :filled t)
                (clim:draw-rectangle* pane 0 0 24 16
                                      :ink clim:+black+ :filled nil :line-thickness 1)))
            (format pane "  ~a" (title-case name))))))))

(defun display-appearance-editing-hair (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Hair Color: "))
    (clim:formatting-cell (pane :align-x :left)
      (display-editing-color-swatch resource pane
                                    #'game-resource-character-hair-color
                                    'hair-color-field))))

(defun display-appearance-editing-skin (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Skin Color: "))
    (clim:formatting-cell (pane :align-x :left)
      (display-editing-color-swatch resource pane
                                    #'game-resource-character-skin-color
                                    'skin-color-field))))

(defun display-appearance-editing-clothes (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Clothes Color: "))
    (clim:formatting-cell (pane :align-x :left)
      (display-editing-color-swatch resource pane
                                    #'game-resource-character-clothes-color
                                    'clothes-color-field))))

(defun display-appearance-editing-head (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Head: "))
    (clim:formatting-cell (pane :align-x :left)
      (let ((kind (game-resource-character-decal resource))
            (current (game-resource-character-head resource)))
        (case (decal-kind-head-type kind)
          (:slider
           (clim:make-pane 'clim:slider
                           :min-value 1
                           :max-value (cond ((eq kind :human) 10)
                                          ((eq kind :merfolk) 15)
                                          (t 255))
                           :value current
                           :value-changed-callback
                           (lambda (gadget value)
                             (declare (ignore gadget))
                              (setf (game-resource-character-head resource) value)
                              (publish :resource-changed :payload resource))))
          (t
           (clim:make-pane 'clim:text-field-pane
                           :value (format nil "~d" current)
                           :activation-callback
                           (lambda (pane)
                             (setf (game-resource-character-head resource)
                                   (parse-integer (clim:gadget-value pane)))
                             (publish :resource-changed :payload resource)))))))))

(defun display-appearance-editing-body (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Body: "))
    (clim:formatting-cell (pane :align-x :left)
      (let ((kind (game-resource-character-decal resource))
            (current (game-resource-character-body resource)))
        (case (decal-kind-body-type kind)
          (:radio-box
           (clim:make-pane 'clim:radio-box-pane
                           :items (list (cons "Robe" 0) (cons "Tunic" 1))
                           :value current
                           :value-changed-callback
                           (lambda (gadget value)
                             (declare (ignore gadget))
                             (setf (game-resource-character-body resource) value)
                             (publish :resource-changed :payload resource))))
          (t
           (clim:make-pane 'clim:text-field-pane
                           :value (format nil "~d" current)
                           :activation-callback
                           (lambda (pane)
                             (setf (game-resource-character-body resource)
                                   (parse-integer (clim:gadget-value pane)))
                             (publish :resource-changed :payload resource)))))))))


;; ANIMATION SEQUENCES SECTION
;; Display animation assignments for decal-kind/body/action/facing
;; Shows North/South centered, West/East side-by-side
(defun display-animation-sequences-section (resource pane)
  (let ((decal-kind (game-resource-character-decal resource))
        (body (game-resource-character-body resource)))
    (when (or (eq decal-kind :human) (eq decal-kind :merfolk) (eq decal-kind :enemy))
      (clim:surrounding-output-with-border (pane :shape :rectangle :ink clim:+black+)
        (clim:formatting-table (pane :multiple-columns t)
          ;; Section header
          (clim:formatting-row (pane)
            (clim:formatting-cell (pane :align-x :left)
              (clim:with-text-style (pane (clim:make-text-style :sans-serif :bold :larger))
                (format pane "Animation Sequences"))))
          
          ;; Action selector
          (clim:formatting-row (pane)
            (clim:formatting-cell (pane :align-x :right) (format pane "For this Action: "))
            (clim:formatting-cell (pane :align-x :left)
              (clim:make-pane 'clim:menu-button-pane
                              :label (format nil "~a"
                                             *current-action-selection*)
                              :items +all-actions+
                              :value-changed-callback
                              (lambda (gadget value)
                                (declare (ignore gadget))
                                (setf
                                 *current-action-selection* value)
                                (publish :resource-changed :payload resource)))))
          
          ;; North facing (centered)
          (clim:formatting-row (pane)
            (clim:formatting-cell (pane :align-x :center)
              (let ((seq (find-assigned-animation-sequence decal-kind body
                                                           
                                                           *current-action-selection* :north)))
                (clim:with-output-as-presentation (pane (list decal-kind body
                                                              *current-action-selection* :north) 'animation-assignment-slot)
                  (if seq
                      (format pane "North: ~a~@[ (~a)~]"
                              (title-case (simple-animation-sequence-index seq))
                              (simple-animation-sequence-label seq))
                      (format pane "North: —"))))))
          
          ;; West and East side by side
          (clim:formatting-row (pane)
            (clim:formatting-cell (pane :align-x :left)
              (let ((seq (find-assigned-animation-sequence decal-kind body
                                                           *current-action-selection* :west)))
                (clim:with-output-as-presentation (pane (list decal-kind body
                                                              *current-action-selection* :west) 'animation-assignment-slot)
                  (if seq
                      (format pane "West: ~a~@[ (~a)~]"
                              (title-case (simple-animation-sequence-index seq))
                              (simple-animation-sequence-label seq))
                      (format pane "West: —")))))
            (clim:formatting-cell (pane :align-x :left)
              (let ((seq (find-assigned-animation-sequence decal-kind body
                                                           *current-action-selection* :east)))
                (clim:with-output-as-presentation (pane (list decal-kind body
                                                              *current-action-selection* :east) 'animation-assignment-slot)
                  (if seq
                      (format pane "East: ~a~@[ (~a)~]"
                              (title-case (simple-animation-sequence-index seq))
                              (simple-animation-sequence-label seq))
                      (format pane "East: —"))))))
          
          ;; South facing (centered)
          (clim:formatting-row (pane)
            (clim:formatting-cell (pane :align-x :center)
              (let ((seq (find-assigned-animation-sequence decal-kind body
                                                           
                                                           *current-action-selection* :south)))
                (clim:with-output-as-presentation (pane (list decal-kind body
                                                              
                                                              *current-action-selection* :south) 'animation-assignment-slot)
                  (if seq
                      (format pane "South: ~a~@[ (~a)~]"
                              (title-case (simple-animation-sequence-index seq))
                              (simple-animation-sequence-label seq))
                      (format pane "South: —")))))))))))
