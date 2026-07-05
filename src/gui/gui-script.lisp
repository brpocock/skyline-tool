;;; Skyline-Tool src/gui/gui-script.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-script-reference ()
  :inherit-from 'game-resource-script)

(clim:define-presentation-type game-resource-script-editable ()
  :inherit-from 'game-resource-script)

(clim:define-presentation-type game-resource-script-viewing ()
  :inherit-from 'game-resource-script)

(clim:define-presentation-method clim:present ((resource game-resource-script) (type game-resource-script-reference) stream view &key)
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

(clim:define-presentation-method clim:present ((resource game-resource-script) (type game-resource-script-editable) stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
     (clim:formatting-row (stream)
       (clim:formatting-cell (stream :align-x :right)
         (format stream "Title: "))
       (clim:formatting-cell (stream :align-x :left)
         (fixme-interactive-editing-gadget-with-validation stream resource 'game-resource-title)))))

(clim:define-presentation-method clim:present ((resource game-resource-script) (type game-resource-script-viewing) stream view &key)
  (declare (ignore view))
  (clim:surrounding-output-with-border (stream :shape :rounded)
     (format stream "[SCRIPT] ~a" (game-resource-title resource))))

(defun open-script-inspector (resource)
  (open-resource-inspector (or resource (make-instance 'game-resource-script)) :editing))

(defmethod game-resource-action-menu ((resource game-resource-script))
  (list
   (make-menu-item "Inspect..." (lambda () (open-script-inspector resource)))
   (make-menu-item "Open in Emacs..."
                   (lambda ()
                     (open-in-emacs (game-resource-full-path resource))))))