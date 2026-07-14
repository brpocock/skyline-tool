;;; Skyline-Tool src/gui/gui-intellivoice-dictionary.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-intellivoice-dictionary-reference ()
   :inherit-from 'game-resource-intellivoice-dictionary)

(clim:define-presentation-type game-resource-intellivoice-dictionary-editable ()
   :inherit-from 'game-resource-intellivoice-dictionary)

(clim:define-presentation-type game-resource-intellivoice-dictionary-viewing ()
   :inherit-from 'game-resource-intellivoice-dictionary)

(defmethod present-reference ((resource game-resource-intellivoice-dictionary) stream)
    (clim:with-output-as-presentation
        (stream resource 'game-resource-intellivoice-dictionary-reference)
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

(defun open-intellivoice-dictionary-inspector (resource)
   (open-resource-inspector (or resource (make-instance 'game-resource-intellivoice-dictionary
                                                         :kind "IntelliVoice Dictionary"
                                                         :moniker "IntelliVoice Dictionary/IntelliVoice.dic")) :editing))

(defmethod open-resource-inspector ((resource game-resource-intellivoice-dictionary) &optional (mode :editing))
  (declare (ignore mode))
  (open-intellivoice-dictionary-inspector resource))

(defmethod present-reading ((resource game-resource-intellivoice-dictionary) stream)
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

(defmethod present-editing ((resource game-resource-intellivoice-dictionary) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (fixme-interactive-editing-gadget-with-validation stream resource 'game-resource-title)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (game-resource-kind resource))))
))
