;;; Skyline-Tool src/gui/gui-flag.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-flag-reference ()
   :inherit-from 'game-resource-flag)

(defmethod present-reference ((resource game-resource-flag) stream)
    (clim:with-output-as-presentation
        (stream resource 'game-resource-flag-reference)
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
            (game-resource-present-right-margin resource stream))))))

(defun open-flag-inspector (resource)
   (open-resource-inspector (or resource (make-instance 'game-resource-flag)) :editing))

(defmethod present-reading ((resource game-resource-flag) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Title: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))))

(defmethod present-editing ((resource game-resource-flag) stream)
  (let ((name (game-resource-title resource)))
    (multiple-value-bind (valid-p error) (validate-minifont-name name)
      (clim:formatting-table (stream)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Name: "))
          (clim:formatting-cell (stream :align-x :left)
            (let ((gadget (clim:insert-gadget stream
                        :label nil
                        :variable name
                        :activation-callback
                        (lambda (gadget)
                          (setf (game-resource-title resource)
                                (clim:gadget-value gadget))))))
              (unless valid-p
                (clim:with-drawing-options (stream :ink :red)
                  (clim:draw-line* stream (clim:gadget-left gadget) 
                                       (+ (clim:gadget-top gadget) 10)
                                       (clim:gadget-right gadget) 
                                       (+ (clim:gadget-top gadget) 10))
                  (clim:draw-text stream "✗" 
                                  (+ (clim:gadget-right gadget) 5)
                                   (clim:gadget-top gadget)))))))))))

