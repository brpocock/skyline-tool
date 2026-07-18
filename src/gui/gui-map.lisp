;;; Skyline-Tool src/gui/gui-map.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-map-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-map-reference)))
  (typep object 'game-resource-map))

(clim:define-presentation-type game-resource-map-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-map-editable)))
  (typep object 'game-resource-map))

(clim:define-presentation-type game-resource-map-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-map-viewing)))
  (typep object 'game-resource-map))

(clim:define-presentation-method clim:present ((resource game-resource-map) (type game-resource-map-reference) stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 0)
        (format stream "~3%"))
      (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 125)
        (game-resource-present-icon resource stream))
      (clim:formatting-cell (stream :align-x :left :align-y :top :min-height 90 :min-width 150)
        (clim:with-text-size (stream :larger)
          (clim:with-text-face (stream :bold)
            (game-resource-present-title resource stream)))
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
        (interactive-editing-gadget-with-validation
         stream resource
         (lambda (r) (game-resource-title r))
         (lambda (r v) (rename-asset r v))
         :label "Name:"
         :validator #'validate-asset-name
         :max-length 200)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Subdirectory: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (changing subdirectory not supported)" (game-resource-locale resource))))))

(clim:define-presentation-method clim:present ((resource game-resource-map) (type game-resource-map-viewing) stream view &key)
   (declare (ignore view))
   (clim:surrounding-output-with-border (stream :shape :rounded)
     (format stream "[MAP ~a] ~a" (game-resource-asset-id resource)
             (game-resource-title resource))))

(defmethod present-editing ((resource game-resource-map) stream)
  (clim:present resource 'game-resource-map-editable :stream stream))

(defmethod present-reading ((resource game-resource-map) stream)
  (clim:present resource 'game-resource-map-viewing :stream stream))

(defmethod present-reference ((resource game-resource-map) stream)
  (clim:present resource 'game-resource-map-reference :stream stream))

(defun open-map-inspector (resource)
  (open-resource-inspector (or resource
                                (make-instance 'game-resource-map
                                  :kind "Map"
                                  :moniker "Maps/new-map.tmx")) :editing))

(defmethod game-resource-action-menu ((resource game-resource-map))
  (list
   (make-menu-item "Inspect..." (lambda () (open-map-inspector resource)))
   (make-menu-item "Open in Tiled..."
                   (lambda ()
                     (uiop:run-program (list "tiled" (or (game-resource-full-path resource) (game-asset-moniker resource)))
                                       :output nil :ignore-error-status t)))))