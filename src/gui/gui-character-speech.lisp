;;; Skyline-Tool src/gui/gui-character-speech.lisp
;;; Speech tab for Character Inspector

(in-package :skyline-tool)

;; Speech tab - read-only display
(defmethod display-speech-tab (frame pane)
  (let ((resource (frame-resource frame)))
    (clim:formatting-table (pane)
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Speech Pitch: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-speech-pitch resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Speech Speed: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-speech-speed resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Speech Bend: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-speech-bend resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Speech Color: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "[~2,'0X]" (game-resource-character-speech-color resource))))
      (clim:formatting-row (pane)
        (clim:formatting-cell (pane :align-x :right) (format pane "Nicks: "))
        (clim:formatting-cell (pane :align-x :left) (format pane "~a" (game-resource-character-nicks resource)))))))
