;;; Skyline-Tool src/gui/gui-blob.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-blob-reference ()
  :inherit-from 'game-resource-blob)

(clim:define-presentation-type game-resource-blob-editable ()
  :inherit-from 'game-resource-blob)

(clim:define-presentation-type game-resource-blob-viewing ()
  :inherit-from 'game-resource-blob)

(clim:define-presentation-method clim:present ((resource game-resource-blob) (type game-resource-blob-reference) stream view &key)
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

(clim:define-presentation-method clim:present ((resource game-resource-blob) (type game-resource-blob-editable) stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (fixme-interactive-editing-gadget-with-validation stream resource 'game-resource-title)))))

(clim:define-presentation-method clim:present ((resource game-resource-blob) (type game-resource-blob-viewing) stream view &key)
  (declare (ignore view))
  (clim:surrounding-output-with-border (stream :shape :rounded)
    (format stream "[#~a] ~a" (first (game-resource-pathnames resource))
            (game-resource-title resource))))

(defun open-blob-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-blob)) :editing))

(defmethod game-resource-action-menu ((resource game-resource-blob))
  (list
   (make-menu-item "Inspect..." (lambda () (open-blob-inspector resource)))
   (make-menu-item "Open in Gimp..."
                   (lambda ()
                     (uiop:run-program
                      (list "gimp" (truename (first (game-resource-pathnames resource))))
                      :output nil :ignore-error-status t)))))
