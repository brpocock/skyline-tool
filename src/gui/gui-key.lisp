;;; Skyline-Tool src/gui/gui-key.lisp
;;;; Copyright © 2026 Interworldly Adventuring, LLC

(in-package :skyline-tool)

(clim:define-presentation-type game-resource-key-reference ()
   :inherit-from 'game-resource-key)

(defmethod present-reference ((resource game-resource-key) stream)
    (clim:with-output-as-presentation (stream resource 'game-resource-key-reference)
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

(defun open-key-inspector (resource)
  (open-resource-inspector (or resource
                                (make-instance 'game-resource-key
                                              :key-id 0
                                              :name "New Key")) :editing))

(defmethod open-resource-inspector ((resource game-resource-key) &optional (mode :editing))
  (clim:run-frame-top-level
   (clim:make-application-frame 'game-resource-key-inspector
                                :resource resource
                                :view-mode mode)))

(clim:define-application-frame game-resource-key-inspector (gui-inspector-frame clim:standard-application-frame)
  ()
  (:menu-bar key-menu-bar)
  (:pretty-name "Key Inspector"))

(clim:define-command-table key-menu-bar
  :menu (("Key" :menu inspector-file-menu)
         ("Edit" :menu inspector-edit-menu)
         ("Run"  :menu inspector-run-menu)
         ("View" :menu inspector-view-menu)
         ("Help" :menu inspector-help-menu)))

(defmethod present-reading ((resource game-resource-key) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream :align-x :right)
        (format stream "Title: "))
      (clim:formatting-cell (stream :align-x :left)
        (format stream "~a" (game-resource-title resource))))))

(defmethod present-editing ((resource game-resource-key) stream)
  (let ((name (game-resource-title resource)))
    (multiple-value-bind (valid-p error) (validate-minifont-name name)
      (clim:formatting-table (stream)
        (clim:formatting-row (stream)
          (clim:formatting-cell (stream :align-x :right)
            (format stream "Name: "))
          (clim:formatting-cell (stream :align-x :left)
            (let ((gadget (insert-gadget stream
                                         :label nil
                                         :variable name
                                         :activation-callback
                                         (lambda (gadget)
                                           (setf (game-resource-title resource)
                                                 (clim:gadget-value gadget)))))))
            (unless valid-p
              (clim:with-drawing-options (stream :ink :red)
                (clim:draw-line* stream (gadget-left gadget) 
                                 (+ (gadget-top gadget) 10)
                                 (gadget-right gadget) 
                                 (+ (gadget-top gadget) 10))
(clim:draw-text* stream "✗" 
                                 (+ (gadget-right gadget) 5)
                                 (gadget-top gadget))))))))))

