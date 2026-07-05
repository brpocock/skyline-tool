;;; Skyline-Tool src/gui/gui-character.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-character-reference ()
   :inherit-from 'game-resource-character)

(defmethod present-reference ((resource game-resource-character) stream)
    (clim:with-output-as-presentation
        (stream resource 'game-resource-character-reference)
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

(defun open-character-inspector (resource)
   (open-resource-inspector (or resource (make-instance 'game-resource-character)) :editing))

(defmethod present-reading ((resource game-resource-character) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Title: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))))

(defmethod present-editing ((resource game-resource-character) stream)
  (let* ((character resource)
         (ods-path (merge-pathnames "Source/Tables/NPCStats.ods" (uiop:getcwd)))
         (headers (get-ods-headers ods-path))
         (values (get-character-values character)))
    (clim:formatting-table (stream)
      (dolist (header headers)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream) (clim:with-text-face (stream :bold) header))
          (clim:formatting-cell (stream)
            (let* ((value (assoc header values :test #'string-equal))
                   (gadget-value (if value (cdr value) "")))
              (clim:insert-gadget stream
                :label nil
                :variable gadget-value
                :activation-callback
                (lambda (gadget)
                  (setf (cdr (assoc header values :test #'string-equal))
                        (clim:gadget-value gadget))
                  (save-character-to-ods character ods-path values))))))))))

