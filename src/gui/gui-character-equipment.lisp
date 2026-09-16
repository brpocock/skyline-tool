;;; Skyline-Tool src/gui/gui-character-equipment.lisp
;;; Equipment tab for Character Inspector

(in-package :skyline-tool)

;; Equipment tab - read-only display
(defmethod display-equipment-tab (frame pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right)
          (format pane "Equipment: "))
        (clim:formatting-cell (pane :align-x :left)
          (format pane "~a" (game-resource-character-equipment resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right)
          (format pane "Shield: "))
        (clim:formatting-cell (pane :align-x :left)
          (format pane "~a" (game-resource-character-shield resource))))
      (render-inventory-section resource pane frame)
      (render-keys-list resource pane frame))))

(defun inventory-icon (item)
  "Return the appropriate icon glyph for an inventory item."
  (cond ((game-resource-item-equippable-p item) "👊")
        ((game-resource-item-shield-p item) "🛡")
        ((game-resource-item-armor-p item) "⛑")
        ((game-resource-item-worn-p item) "💍")
        (t "")))

(defun render-inventory-section (resource pane frame)
  "Render the inventory section with 4 columns: checkbox, locator, title, icon."
  (clim:formatting-table (pane)
    (loop for item in (load-item-list)
          for i = (game-resource-item-id item)
          do (clim:formatting-row (pane)
               (clim:formatting-cell (pane :align-x :right)
                 (clim:make-pane 'clim:check-box-pane
                                 :value (aref (game-resource-character-inventory resource) i)
                                 :value-changed-callback
                                 (lambda (gadget value)
                                   (declare (ignore gadget))
                                   (setf (aref (game-resource-character-inventory resource) i)
                                         (if value 1 0))
                                   (publish :resource-changed resource))))
               (clim:formatting-cell (pane :align-x :left)
                 (game-resource-locator item))
               (clim:formatting-cell (pane :align-x :left)
                 (game-resource-title item))
               (clim:formatting-cell (pane :align-x :center)
                 (inventory-icon item))))))

;; Helper function: Render the 32-key checkbox list (single column)
(defun render-keys-list (resource pane frame)
  "Render a single-column list of 32 key checkboxes for the equipment tab."
  (clim:with-text-style (pane :bold)
    (clim:with-text-size (pane :larger)
      (format pane "Keys")))
  (clim:surrounding-output-with-border (pane :shape :rectangle :ink clim:+gray+)
    (loop for i from 0 below 32
          do (clim:make-pane 'clim:check-box-pane
                             :label (elt (frame-key-names-cache frame) i)
                             :value (aref (game-resource-character-keys resource) i)
                             :value-changed-callback
                             (lambda (gadget value)
                               (declare (ignore gadget))
                               (setf (aref (game-resource-character-keys resource) i) value)
                               (publish :resource-changed resource))))))
