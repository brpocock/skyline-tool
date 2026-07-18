;;; Skyline-Tool src/gui/gui-blob.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-blob-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-blob-reference)))
  (typep object 'game-resource-blob))

(clim:define-presentation-type game-resource-blob-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-blob-editable)))
  (typep object 'game-resource-blob))

(clim:define-presentation-type game-resource-blob-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-blob-viewing)))
  (typep object 'game-resource-blob))

(clim:define-presentation-method clim:present ((resource game-resource-blob)
                                               (type game-resource-blob-reference) stream view &key)
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

(clim:define-presentation-method clim:present ((resource game-resource-blob) (type game-resource-blob-editable) stream view &key)
  (declare (ignore view))
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Name: "))
      (clim:formatting-cell (stream :align-x :left)
        (interactive-editing-gadget-with-validation
         stream resource
         (lambda (r) (game-resource-title r))
         (lambda (r v) (setf (game-resource-title r) v))
         :label "Name:"
         :validator #'validate-minifont-name
         :max-length 20)))))

(clim:define-presentation-method clim:present ((resource game-resource-blob) (type game-resource-blob-viewing) stream view &key)
  (declare (ignore view))
  (clim:surrounding-output-with-border (stream :shape :rounded)
    (format stream "[#~a] ~a" (first (game-resource-pathnames resource))
            (game-resource-title resource))))

(defmethod present-editing ((resource game-resource-blob) stream)
  (clim:present resource 'game-resource-blob-editable :stream stream))

(defmethod present-reading ((resource game-resource-blob) stream)
  (clim:present resource 'game-resource-blob-viewing :stream stream))

(defmethod present-reference ((resource game-resource-blob) stream)
  (clim:present resource 'game-resource-blob-reference :stream stream))

(defun open-blob-inspector (resource)
  (open-resource-inspector (or resource
                                (make-instance 'game-resource-blob
                                 
                                  :full-path nil
                                  :moniker "Blobs/new-blob.xcf")) :editing))

(defmethod game-resource-action-menu ((resource game-resource-blob))
  (list
   (make-menu-item "Inspect..." (lambda () (open-blob-inspector resource)))
   (make-menu-item "Open in Gimp..."
                   (lambda ()
                     (uiop:run-program
                      (list "gimp" (truename (first (game-resource-pathnames resource))))
                      :output nil :ignore-error-status t)))))
