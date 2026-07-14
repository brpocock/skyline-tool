;;; Skyline-Tool src/gui/gui-boat.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-boat-reference ()
   :inherit-from 'game-resource-boat)

(clim:define-presentation-type game-resource-boat-editable ()
   :inherit-from 'game-resource-boat)

(clim:define-presentation-type game-resource-boat-viewing ()
   :inherit-from 'game-resource-boat)

(defmethod present-reference ((resource game-resource-boat) stream)
    (clim:with-output-as-presentation
        (stream resource 'game-resource-boat-reference)
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

(defmethod open-resource-inspector ((resource game-resource-boat) &optional (mode :editing))
  (declare (ignore mode))
  (let ((name (game-resource-title resource)))
    (run-boat-inspector (or name "unknown"))))

(defun open-boat-inspector (resource)
   (open-resource-inspector (or resource
                                 (make-instance 'game-resource-boat
                                  
                                   :id 0
                                   :name "New Boat"
                                   :boat-class "rowboat"
                                   :notes "")) :editing))

(defmethod present-reading ((resource game-resource-boat) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Title: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))))

(defmethod present-editing ((resource game-resource-boat) stream)
  (let* ((boat resource)
         (ods-path (game-resource-collective-path boat))
         (headers (get-ods-headers ods-path))
         (values (get-boat-values boat)))
    (clim:formatting-table (stream)
      (dolist (header headers)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream) (clim:with-text-face (stream :bold) header))
          (clim:formatting-cell (stream)
            (let* ((value (assoc header values :test #'string-equal))
                   (gadget-value (if value (cdr value) "")))
              (push-button-fixme stream
                                 :label nil
                                 :variable gadget-value
                                 :activation-callback
                                 (lambda (gadget)
                                   (setf (cdr (assoc header values :test #'string-equal))
                                         (clim:gadget-value gadget))
                                   (save-boat-to-ods boat ods-path values))))))))))

