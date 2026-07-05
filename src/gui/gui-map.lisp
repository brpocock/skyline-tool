;;; Skyline-Tool src/gui/gui-map.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-map-reference ()
  :inherit-from 'game-resource-map)

(clim:define-presentation-type game-resource-map-editable ()
  :inherit-from 'game-resource-map)

(clim:define-presentation-type game-resource-map-viewing ()
  :inherit-from 'game-resource-map)

(clim:define-presentation-method clim:present ((resource game-resource-map) (type game-resource-map-reference) stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
        (format stream "~3%"))
      (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
        (game-resource-present-icon resource stream))
      (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 150)
        (clim:with-text-face (stream :bold)
          (game-resource-present-title resource stream))
        (format stream "~%~5t")
        (clim:with-text-size (stream :smaller)
          (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.75))
            (game-resource-present-subheading resource stream))))
      (clim:formatting-cell (stream :align-x :right :align-y :top :min-height 90 :min-width 125)
        (game-resource-present-right-margin resource stream)))))

(clim:define-presentation-method clim:present ((resource game-resource-map) (type game-resource-map-editable) stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (fixme-interactive-editing-gadget-with-validation stream resource 'game-resource-title)))))

(clim:define-presentation-method clim:present ((resource game-resource-map) (type game-resource-map-viewing) stream view &key)
   (declare (ignore view))
   (clim:surrounding-output-with-border (stream :shape :rounded)
     (format stream "[MAP ~a] ~a" (game-resource-asset-id resource)
             (game-resource-title resource))))

(defun open-map-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-map)) :editing))

(defmethod game-resource-action-menu ((resource game-resource-map))
  (list
   (make-menu-item "Inspect..." (lambda () (open-map-inspector resource)))
   (make-menu-item "Open in Tiled..."
                   (lambda ()
                     (uiop:run-program (list "tiled" (or (game-resource-full-path resource) (game-resource-moniker resource)))
                                      :output nil :ignore-error-status t)))))