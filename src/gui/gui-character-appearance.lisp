;;; Skyline-Tool src/gui/gui-character-appearance.lisp
;;; Appearance tab for Character Inspector

(in-package :skyline-tool)

;; Appearance tab — dispatch read-only vs editing by frame-view-mode
(defmethod display-appearance-tab ((frame character-inspector-frame) pane)
  (ecase (frame-view-mode frame)
    (:reference (display-appearance-reference frame pane))
    (:editing   (display-appearance-editing frame pane))))

;; Reference view — read-only display
(defmethod display-appearance-reference ((frame character-inspector-frame) pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (display-appearance-reference-decal resource pane)
      (display-appearance-reference-hair resource pane)
      (display-appearance-reference-skin resource pane)
      (display-appearance-reference-clothes resource pane)
      (display-appearance-reference-head resource pane)
      (display-appearance-reference-body resource pane))))

(defun display-appearance-reference-decal (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Decal Kind: "))
    (clim:formatting-cell (pane :align-x :left)
      (format pane "~a" (decal-display-name (game-resource-character-decal resource))))))

(defun display-appearance-reference-hair (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Hair Color: "))
    (clim:formatting-cell (pane :align-x :left)
      (destructuring-bind (name r g b)
          (elt *common-palette* (game-resource-character-hair-color resource))
        (print-wide-pixel (rgb->palette r g b) pane)
        (format pane "  ~a" (title-case (string name)))))))

(defun display-appearance-reference-skin (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Skin Color: "))
    (clim:formatting-cell (pane :align-x :left)
      (destructuring-bind (name r g b)
          (elt *common-palette* (game-resource-character-skin-color resource))
        (print-wide-pixel (rgb->palette r g b) pane)
        (format pane "  ~a" (title-case (string name)))))))

(defun display-appearance-reference-clothes (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Clothes Color: "))
    (clim:formatting-cell (pane :align-x :left)
      (destructuring-bind (name r g b)
          (elt *common-palette* (game-resource-character-clothes-color resource))
        (print-wide-pixel (rgb->palette r g b) pane)
        (format pane "  ~a" (title-case (string name)))))))

(defun display-appearance-reference-head (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Head: "))
    (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-head resource)))))

(defun display-appearance-reference-body (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Body: "))
    (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-body resource)))))

;; Editing view — gadget-based editable display
(defmethod display-appearance-editing ((frame character-inspector-frame) pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane :multiple-columns t)
      (display-appearance-editing-decal resource pane)
      (display-appearance-editing-hair resource pane)
      (display-appearance-editing-skin resource pane)
      (display-appearance-editing-clothes resource pane)
      (display-appearance-editing-head resource pane)
      (display-appearance-editing-body resource pane))))

(defun display-appearance-editing-decal (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Decal Kind: "))
    (clim:formatting-cell (pane :align-x :left)
      (clim:make-pane 'clim:option-pane
                      :items (mapcar (lambda (k) (cons (decal-display-name k) k)) +decal-kinds+)
                      :current-value (game-resource-character-decal resource)
                      :callback
                      (lambda (pane value)
                        (declare (ignore pane))
                        (setf (game-resource-character-decal resource) value)
                        (publish-resource-changed resource))))))

(defun color-palette-items ()
  (loop for i from 0 for (name r g b) in *common-palette*
        collect (cons (title-case (string name)) i)))

(defun display-appearance-editing-hair (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Hair Color: "))
    (clim:formatting-cell (pane :align-x :left)
      (clim:make-pane 'clim:option-pane
                      :items (color-palette-items)
                      :current-value (game-resource-character-hair-color resource)
                      :callback
                      (lambda (pane value)
                        (declare (ignore pane))
                        (setf (game-resource-character-hair-color resource) value)
                        (publish-resource-changed resource))))))

(defun display-appearance-editing-skin (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Skin Color: "))
    (clim:formatting-cell (pane :align-x :left)
      (clim:make-pane 'clim:option-pane
                      :items (color-palette-items)
                      :current-value (game-resource-character-skin-color resource)
                      :callback
                      (lambda (pane value)
                        (declare (ignore pane))
                        (setf (game-resource-character-skin-color resource) value)
                        (publish-resource-changed resource))))))

(defun display-appearance-editing-clothes (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Clothes Color: "))
    (clim:formatting-cell (pane :align-x :left)
      (clim:make-pane 'clim:option-pane
                      :items (color-palette-items)
                      :current-value (game-resource-character-clothes-color resource)
                      :callback
                      (lambda (pane value)
                        (declare (ignore pane))
                        (setf (game-resource-character-clothes-color resource) value)
                        (publish-resource-changed resource))))))

(defun display-appearance-editing-head (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Head: "))
    (clim:formatting-cell (pane :align-x :left)
      (clim:make-pane 'clim:text-field-pane
                      :value (game-resource-character-head resource)
                      :activation-callback
                      (lambda (pane)
                        (setf (game-resource-character-head resource) (clim:gadget-value pane))
                        (publish-resource-changed resource))))))

(defun display-appearance-editing-body (resource pane)
  (clim:formatting-row (pane)
    (clim:formatting-cell (pane :align-x :right) (format pane "Body: "))
    (clim:formatting-cell (pane :align-x :left)
      (clim:make-pane 'clim:text-field-pane
                      :value (game-resource-character-body resource)
                      :activation-callback
                      (lambda (pane)
                        (setf (game-resource-character-body resource) (clim:gadget-value pane))
                        (publish-resource-changed resource))))))
