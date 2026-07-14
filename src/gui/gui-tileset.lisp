;;; Skyline-Tool src/gui/gui-tileset.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-tileset-reference ()
   :inherit-from 'game-resource-tileset)

(clim:define-presentation-type game-resource-tileset-editable ()
   :inherit-from 'game-resource-tileset)

(clim:define-presentation-type game-resource-tileset-viewing ()
   :inherit-from 'game-resource-tileset)

(defmethod present-reference ((resource game-resource-tileset) stream)
    (clim:with-output-as-presentation
        (stream resource 'game-resource-tileset-reference)
      (clim:formatting-table (stream)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
            (format stream "~3%"))
          (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
            (game-resource-present-icon resource stream))
          ;; Title
          (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 150)
            (clim:with-text-face (stream :bold)
              (game-resource-present-title resource stream))
            ;; Subheading on next line in small, possibly gray text
            (format stream "~%~5t")
            (clim:with-text-size (stream :smaller)
              (clim:with-drawing-options (stream :ink (clim:make-gray-color 0.75))
                (game-resource-present-subheading resource stream))))
          (clim:formatting-cell (stream :align-x :right :align-y :top :min-height 90 :min-width 125)
            (game-resource-present-right-margin resource stream))))))

(defmethod open-resource-inspector ((resource game-resource-tileset) &optional (mode :editing))
  (declare (ignore mode))
  (let* ((path (game-resource-full-path resource))
         (fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
         (frame (clim:make-application-frame 'tileset-inspector-frame
                                             :resource (or resource (make-instance 'game-resource-tileset))
                                             :pathname path
                                             :frame-manager fm
                                             :width 820 :height 700)))
    (clim:run-frame-top-level frame)))

(defun open-tileset-inspector (resource)
   (open-resource-inspector (or resource (make-instance 'game-resource-tileset)) :editing))

(defmethod present-reading ((resource game-resource-tileset) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-kind resource))))
))

(defmethod present-editing ((resource game-resource-tileset) stream)
  (clim:formatting-table (stream)
    ;; Name (editable)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (fixme-interactive-editing-gadget-with-validation stream resource 'game-resource-title)))
    ;; Kind (read-only)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (game-resource-kind resource))))
    ;; Paths
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Path: "))
      (clim:formatting-cell (stream :align-x :left)
        (if (typep resource 'game-resource-from-file)
            (princ (game-resource-pathnames resource) stream)
            (format stream "(none)"))))
    ;; Tile count
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Tiles: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~d" (or (ignore-errors (tileset-tile-count resource)) 0))))))
