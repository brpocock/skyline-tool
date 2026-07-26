;;; Skyline-Tool src/gui/gui-character-equipment.lisp
;;; Equipment tab for Character Inspector

(in-package :skyline-tool)

;; Equipment tab - read-only display
(defmethod display-equipment-tab (frame pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Equipment: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-equipment resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Shield: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-shield resource)))))))