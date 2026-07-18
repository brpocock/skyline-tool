;;; Skyline-Tool src/gui/gui-script.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-script-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-script-reference)))
  (typep object 'game-resource-script))

(clim:define-presentation-type game-resource-script-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-script-editable)))
  (typep object 'game-resource-script))

(clim:define-presentation-type game-resource-script-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-script-viewing)))
  (typep object 'game-resource-script))

(clim:define-presentation-method clim:present ((resource game-resource-script) (type game-resource-script-reference) stream view &key)
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

(clim:define-presentation-method clim:present ((resource game-resource-script) (type game-resource-script-editable) stream view &key)
   (declare (ignore view))
   (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :right)
          (format stream "Title: "))
        (clim:formatting-cell (stream :align-x :left)
          (interactive-editing-gadget-with-validation
           stream resource
           (lambda (r) (game-resource-title r))
           (lambda (r v) (setf (game-resource-title r) v))
           :label "Title:"
           :validator #'validate-asset-name
           :max-length 200)))))

(clim:define-presentation-method clim:present ((resource game-resource-script) (type game-resource-script-viewing) stream view &key)
  (declare (ignore view))
  (clim:surrounding-output-with-border (stream :shape :rounded)
     (format stream "[SCRIPT] ~a" (game-resource-title resource))))

(defmethod present-editing ((resource game-resource-script) stream)
  (clim:present resource 'game-resource-script-editable :stream stream))

(defmethod present-reading ((resource game-resource-script) stream)
  (clim:present resource 'game-resource-script-viewing :stream stream))

(defmethod present-reference ((resource game-resource-script) stream)
  (clim:present resource 'game-resource-script-reference :stream stream))

(defun open-script-inspector (resource)
  (open-resource-inspector (or resource
                                (make-instance 'game-resource-script
                                  :kind "Script"
                                  :moniker "Scripts/new-script.sky")) :editing))

(defmethod game-resource-action-menu ((resource game-resource-script))
  (list
   (make-menu-item "Inspect..." (lambda () (open-script-inspector resource)))
   (make-menu-item "Open in Emacs..."
                   (lambda ()
                     (open-in-emacs (game-resource-full-path resource))))))