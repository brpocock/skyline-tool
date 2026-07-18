;;; Skyline-Tool src/gui/gui-sprite-sheet.lisp
;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-sprite-sheet-reference ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-sprite-sheet-reference)))
  (typep object 'game-resource-sprite-sheet))

(clim:define-presentation-type game-resource-sprite-sheet-editable ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-sprite-sheet-editable)))
  (typep object 'game-resource-sprite-sheet))

(clim:define-presentation-type game-resource-sprite-sheet-viewing ())

(clim:define-presentation-method clim:presentation-typep (object (type (eql 'game-resource-sprite-sheet-viewing)))
  (typep object 'game-resource-sprite-sheet))

(defmethod present-reference ((resource game-resource-sprite-sheet) stream)
  (clim:with-output-as-presentation (stream resource 'game-resource-sprite-sheet-reference)
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
          (game-resource-present-right-margin resource stream))))))

(defmethod present-reading ((resource game-resource-sprite-sheet) stream)
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
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Path: "))
      (clim:formatting-cell (stream :align-x :left)
        (if (typep resource 'game-resource-from-file)
            (princ (game-resource-full-path resource) stream)
            (format stream "(none)"))))))

(defmethod present-editing ((resource game-resource-sprite-sheet) stream)
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
         :validator #'validate-asset-name
         :max-length 200)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Kind: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a (read-only)" (game-resource-kind resource))))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Path: "))
      (clim:formatting-cell (stream :align-x :left)
        (if (typep resource 'game-resource-from-file)
            (princ (game-resource-pathnames resource) stream)
            (format stream "(none)"))))))

(defun open-sprite-sheet-inspector (resource)
  (open-resource-inspector (or resource
                                (make-instance 'game-resource-sprite-sheet
                                  :full-path nil)) :editing))

(defmethod open-resource-inspector ((resource game-resource-sprite-sheet) &optional (mode :editing))
  (declare (ignore mode))
  (let ((fm (clim:find-frame-manager :port (or (clim:find-port) (clim:find-port :server-path :x))))
        (frame (clim:make-application-frame 'sprite-sheet-inspector-frame
                                            :resource (or resource (make-instance 'game-resource-sprite-sheet))
                                            :pretty-name (format nil "~a — ~a ~a"
                                                                 (game-resource-title resource)
                                                                 *game-title* (machine-directory-name))
                                            :frame-manager fm)))
    (clim:run-frame-top-level frame)))

(defmethod game-resource-action-menu ((resource game-resource-sprite-sheet))
  (list
   (make-menu-item "Inspect..." (lambda () (open-sprite-sheet-inspector resource)))
   (make-menu-item "Open in Gimp..."
                   (lambda ()
                     (uiop:run-program
                      (list "gimp" (truename (first (game-resource-pathnames resource))))
                      :output nil :ignore-error-status t)))))
