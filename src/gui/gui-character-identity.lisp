;;; Skyline-Tool src/gui/gui-character-identity.lisp
;;; Identity tab for Character Inspector

(in-package :skyline-tool)

;; Identity tab - read-only display
(defmethod display-identity-tab ((frame character-inspector-frame) pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Name: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-name resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "ID: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~d" (game-resource-character-id resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Decal: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-decal resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Gender: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-gender resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Home: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (or (game-resource-character-home resource) "None"))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Comments: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-memo resource))))))

;; Identity tab - editable display
(defmethod display-identity-editing ((frame character-inspector-frame) pane)
  (let ((resource (frame-resource frame))
        (name (game-resource-character-name resource))
        (char-id (game-resource-character-id resource))
        (decal (game-resource-character-decal resource))
        (gender (game-resource-character-gender resource))
        (memo (game-resource-character-memo resource)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Name: "))
        (clim:formatting-cell (pane :align-x :left)
          (clim:make-pane 'clim:text-field-pane
                          :value name
                          :activation-callback
                          (lambda (pane)
                            (let ((new-value (clim:gadget-value pane)))
                              (when (validate-minifont-name new-value 12)
                                (setf (game-resource-character-name resource) new-value)
                                (publish-resource-changed resource))))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "ID: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~d (read-only)" char-id)))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Decal: "))
        (clim:formatting-cell (pane :align-x :left)
          (clim:make-pane 'clim:text-field-pane
                          :value decal
                          :activation-callback
                          (lambda (pane)
                            (setf (game-resource-character-decal resource) (clim:gadget-value pane))
                            (publish-resource-changed resource)))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Gender: "))
        (clim:formatting-cell (pane :align-x :left)
          (clim:make-pane 'clim:radio-box-pane
                          :items '(:male :female :other)
                          :current-value gender
                          :callback
                          (lambda (pane value)
                            (setf (game-resource-character-gender resource) value)
                            (publish-resource-changed resource)))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Memo: "))
        (clim:formatting-cell (pane :align-x :left)
          (clim:make-pane 'clim:text-field-pane
                          :value memo
                          :activation-callback
                          (lambda (pane)
                            (setf (game-resource-character-memo resource) (clim:gadget-value pane))
                            (publish-resource-changed resource)))))))